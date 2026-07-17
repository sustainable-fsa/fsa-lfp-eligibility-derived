
# pak::pak(
#   c(
#     "arrow?source",
#     "curl",
#     "tidyverse",
#     "furrr",
#     "future.mirai",
#     "dtplyr",
#     "fs",
#     "jsonlite",
#     "processx"
#   )
# )

library(magrittr)
library(tidyverse)
library(arrow)
library(furrr)
library(future.mirai)
library(dtplyr)

source("R/s3-archive.R")
s3_preflight()
s3_bucket_name <- Sys.getenv("S3_BUCKET", unset = "sustainable-fsa")
s3_prefix      <- Sys.getenv("S3_PREFIX", unset = "fsa-lfp-eligibility-reanalysis")
## Pull prior archive state so incremental guards see existing outputs
s3_pull(s3_bucket_name, paste0(s3_prefix, "/data"), "data")

## WEEKLY USDM COUNTY COMPARISON
## For each USDM date, get data for each county aggregation data source, 
## and find the maximum designation in each county.

dir.create(
  file.path("data","usdm"),
  recursive = TRUE,
  showWarnings = FALSE
)

# Download all USDM County Aggregations (Census, LFP, and USDM Reported)
usdm_updates <-
  c("usdm-counties-reported",
    "usdm-counties-fsa-lfp",
    "usdm-counties-census-2020",
    "usdm-counties"
  ) %>%
  magrittr::set_names(.,.) |>
  purrr::map_dfr(\(x){
    paste0("https://data.sustainable-fsa.com/",x,"/_manifest.txt") |>
      read_lines() |>
      stringr::str_subset("USDM_") |>
      tibble::tibble(url = _)
  },
  .id = "source"
  ) |>
  dplyr::mutate(file = basename(url)) |>
  dplyr::group_by(file) |>
  tidyr::nest() |>
  dplyr::mutate(outfile =
                  file.path("data", "usdm",
                            file)) %>%
  dplyr::filter(!file.exists(outfile)) %>%
  ## Freshness gate: drop weeks whose upstream usdm parquet isn't published
  ## yet by all four archives (fallback/premature runs no-op instead of
  ## failing in read_parquet).
  dplyr::mutate(
    posted = purrr::map_lgl(data, \(d) all(purrr::map_lgl(d$url, url_exists)))
  ) |>
  dplyr::ungroup()

usdm_updates |>
  dplyr::filter(!posted) |>
  purrr::pwalk(
    \(file, ...)
    gate_skip(paste0(file, " not yet posted by all upstream archives; skipping."))
  )

plan(mirai_multisession)
usdm_updates |>
  dplyr::filter(posted) |>
  furrr::future_pwalk(
    .f = function(data,
                  outfile,
                  ...){

      if(!file.exists(outfile)){

        data %>%
          dplyr::rowwise() %>%
          dplyr::mutate(counties = list(arrow::read_parquet(url))) |>
          dplyr::select(!url) |>
          tidyr::unnest(counties) |>
          dplyr::filter(!is.na(STATEFP)) |>
          dplyr::transmute(
            source,
            FIPS = stringr::str_c(STATEFP, COUNTYFP),
            usdm_date,
            usdm_class
          ) |>
          dtplyr::lazy_dt() %>%
          dplyr::group_by(source, FIPS, usdm_date) %>%
          dplyr::summarise(
            usdm_class = max(usdm_class),
            .groups = "drop"
          ) %>%
          dplyr::collect() |>
          tidyr::pivot_wider(names_from = source,
                             values_from = usdm_class) %>%
          dplyr::arrange(FIPS) %>%
          arrow::write_parquet(sink = outfile,
                               version = "latest",
                               compression = "zstd",
                               compression_level = 13,
                               use_dictionary = TRUE)
      }
    }
  )

plan(sequential)

## Create a single parquet output, for simplicity
usdm_counties_max <-
  arrow::open_dataset("data/usdm") %>%
  dplyr::collect() %>%
  dplyr::arrange(FIPS, usdm_date)

usdm_counties_max %>%
  arrow::write_parquet(sink = "usdm.parquet",
                       version = "latest",
                       compression = "zstd",
                       compression_level = 13,
                       use_dictionary = TRUE)

## WEEKLY LFP PAYMENT CALCULATION
## For each USDM date and sequence of maximum county USDM categories,
## calculate the LFP Payment history across Pasture Types

# First, Download the Normal Grazing Period history
fsa_normal_grazing_period <-
  readr::read_csv(
    "https://data.sustainable-fsa.com/fsa-normal-grazing-period/fsa-normal-grazing-period.csv"
  ) |>
  dplyr::transmute(
    `Program Year` = as.integer(`Program Year`), 
    FIPS = stringr::str_c(`FIPS State Code`, `FIPS County Code`),
    `Pasture Type`,
    `Grazing Period Start Date`,
    `Grazing Period End Date`,
    `Normal Grazing Period` = 
      lubridate::interval(
        start = `Grazing Period Start Date`,
        end = `Grazing Period End Date`
      )) |>
  dplyr::arrange(FIPS, `Program Year`, `Pasture Type`) %>%
  dplyr::distinct()

# The county level max USDM classes, from above
# Recode classes less that D2, which don't qualify for LFP
usdm_counties <-
  usdm_counties_max %>%
  # Only include FIPS in the NGP data
  dplyr::filter(FIPS %in% unique(fsa_normal_grazing_period$FIPS)) %>%
  tidyr::pivot_longer(!c(FIPS, usdm_date),
                      names_to = "source",
                      values_to = "usdm_class") %>%
  dplyr::mutate(
    usdm_class = forcats::fct_collapse(usdm_class,
                                       `< D2` = c("None", "D0", "D1"))
  ) %>%
  dplyr::filter(!is.na(usdm_class))

# Create a Run-Length encoded version of the counties dataset
usdm_counties_rle <-
  usdm_counties %>%
  dplyr::group_by(FIPS, source) %>%
  # Implement a version of run-length encoding
  lazy_dt() %>%
  # Add a column for end date (`USDM End`) of the week's USDM map (inclusive) and 
  # create an index (`group`) of changes in the county status
  dplyr::mutate(`USDM End` = usdm_date + 6,
                group = cumsum(c(0, diff(usdm_class)) != 0)) %>%
  dplyr::group_by(FIPS, source, usdm_class, group) %>%
  # By group, calculate the start and end date (inclusive) of the county USDM status, and
  # the number of weeks in that status.
  dplyr::summarise(
    `USDM Start` = min(usdm_date),
    `USDM End` = max(`USDM End`),
    `USDM Weeks` = n(),
    .groups = "drop") %>%
  as_tibble() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(`USDM Interval` = lubridate::interval(start = `USDM Start`,
                                                      end = `USDM End`)) %>%
  dplyr::select(FIPS,  
                source,
                # usdm_date,
                `USDM Start`, `USDM End`,
                `USDM Interval`, 
                `USDM Weeks`,
                USDM = usdm_class) %>%
  dplyr::arrange(FIPS, source, `USDM Start`) %>%
  dplyr::filter(!is.na(USDM))

# First, find runs of USDM categories that overlap with normal grazing periods,
# and calculate their overlaps. Date intervals in `lubridate` are inclusive of 
# start and end dates.
lfp_usdm_calculated <-
  fsa_normal_grazing_period |>
  dplyr::full_join(usdm_counties_rle,
                   by = "FIPS",
                   relationship = "many-to-many") %>%
  # First, filter out rows where the NGP and USDM intervals do not overlap
  # Also, filter periods that are not D2, D3, or D4. They don't matter for LFP.
  dplyr::filter(
    lubridate::int_overlaps(`Normal Grazing Period`, `USDM Interval`)
  ) %>%
  # Then, calculate the intersecting intervals
  dplyr::mutate(
    `LFP Interval` = lubridate::intersect(`Normal Grazing Period`, 
                                          `USDM Interval`)
  ) %>%
  dplyr::select(FIPS, 
                source,
                `Program Year`, `Pasture Type`, `Normal Grazing Period`, 
                `USDM Interval`, `LFP Interval`, USDM, `USDM Weeks`) %>%
  dplyr::mutate(`LFP Weeks` = (lubridate::time_length(`LFP Interval`, unit = "days") + 1) / 7) %>%
  dplyr::select(FIPS, 
                source,
                `Program Year`, `Pasture Type`, 
                `LFP Interval`, USDM, `LFP Weeks`) %>%
  dplyr::arrange(FIPS, source, `Program Year`, `Pasture Type`)

# A general pattern identifier
find_pattern <- function(x, pattern) {
  n <- length(x)
  m <- length(pattern)
  if (m == 0L || m > n) return(integer(0))
  
  starts <- seq_len(n - m + 1L)
  
  match_all <- Reduce(`&`, lapply(seq_len(m), function(j) {
    x[starts + j - 1L] == pattern[j]
  }))
  
  starts[which(match_all)]   # which() drops NAs -> no match at NA positions
}

WEEK          <- 604800                                    # seconds in a week
int_start_num <- function(iv) as.numeric(iv@start)         # int_start() in secs-since-epoch
int_end_num   <- function(iv) as.numeric(iv@start) + iv@.Data  # int_end() in secs-since-epoch
secs_to_date  <- function(s)  as.Date(.POSIXct(s, tz = "UTC")) # UTC Date coercion

lfp_payments_calculated <-
  lfp_usdm_calculated %>%
  dplyr::mutate(
    `LFP Interval Start` = int_start_num(`LFP Interval`),
    `LFP Interval End` = int_end_num(`LFP Interval`)
  ) %>%
  dplyr::group_by(FIPS, source, `Program Year`, `Pasture Type`) %>%
  dplyr::mutate(
    `D2_Sandwich` =
      find_pattern(USDM, c("D2", "< D2", "D2")) %>%
      { .[which(`LFP Weeks`[.] < 7 &
                  `LFP Weeks`[. + 1] == 1 &
                  (`LFP Weeks`[.] + `LFP Weeks`[. + 2]) >= 7)] + 2L } %>%
      `[<-`(
        rep(NA_real_, length(USDM)),
        .,
        value = `LFP Interval Start`[.] + (7 - `LFP Weeks`[. - 2]) * WEEK
      ),
    D3b = {
      idx <- which(USDM == "D3")
      cs  <- cumsum(`LFP Weeks`[idx])
      k   <- which(cs >= 4)[1]
      out <- rep(NA_real_, length(USDM))
      if (!is.na(k))
        out[idx[k]] <- `LFP Interval End`[idx[k]] + (4 - cs[k]) * WEEK
      out
    },
    D4b = {
      idx <- which(USDM == "D4")
      cs  <- cumsum(`LFP Weeks`[idx])
      k   <- which(cs >= 4)[1]
      out <- rep(NA_real_, length(USDM))
      if (!is.na(k))
        out[idx[k]] <- `LFP Interval End`[idx[k]] + (4 - cs[k]) * WEEK
      out
    }
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    dplyr::across(c(D2_Sandwich, D3b, D4b), secs_to_date),
    D2a_2026 = ifelse(USDM == "D2" & `LFP Weeks` >= 4, 
                      lubridate::int_start(`LFP Interval`) + dweeks(4), 
                      as.Date(NA)) |>
      lubridate::as_datetime() |>
      lubridate::as_date(),
    D2b_2026 = ifelse(USDM == "D2" & `LFP Weeks` >= 7, 
                      lubridate::int_start(`LFP Interval`) + dweeks(7), 
                      as.Date(NA)) |>
      lubridate::as_datetime() |>
      lubridate::as_date(),
    D2 = ifelse(USDM == "D2" & `LFP Weeks` >= 8, 
                lubridate::int_start(`LFP Interval`) + dweeks(8), 
                as.Date(NA)) |>
      lubridate::as_datetime() |>
      lubridate::as_date(),
    D3a = ifelse(USDM == "D3", 
                 lubridate::int_start(`LFP Interval`), 
                 as.Date(NA)) |>
      lubridate::as_datetime() |>
      lubridate::as_date(),
    D4a = ifelse(USDM == "D4", 
                 lubridate::int_start(`LFP Interval`), 
                 as.Date(NA)) |>
      lubridate::as_datetime() |>
      lubridate::as_date()
  ) %>%
  dplyr::select(!c(`LFP Interval Start`, `LFP Interval End`)) %>%
  tidyr::pivot_longer(D2_Sandwich:D4a,
                      names_to = "Qualifying Drought Event",
                      values_to = "Qualifying Date") %>%
  dplyr::filter(!is.na(`Qualifying Date`)) %>%
  dplyr::arrange(FIPS, source, `Program Year`, `Pasture Type`, `Qualifying Date`) %>%
  dplyr::mutate(
    `Qualifying Drought Event` = 
      ifelse(`Qualifying Drought Event` == "D2_Sandwich", 
             "D2b_2026", 
             `Qualifying Drought Event`)
  ) %>%
  dplyr::mutate(
    `Payments` = 
      dplyr::case_when(
        `Program Year` %in% 2008:2011 & 
          `Qualifying Drought Event` %in%
          c("D4b", "D3b & D4a", "D3b", "D4a") ~ 3L,
        `Program Year` %in% 2008:2011 & 
          `Qualifying Drought Event` %in%
          c("D3a") ~ 2L,
        `Program Year` %in% 2008:2011 & 
          `Qualifying Drought Event` %in%
          c("D2") ~ 1L,
        
        # payments switched in Program Year 2012
        `Program Year` %in% 2012:2025 & 
          `Qualifying Drought Event` %in%
          c("D4b") ~ 5L,
        `Program Year` %in% 2012:2025 & 
          `Qualifying Drought Event` %in%
          c("D3b & D4a", "D3b", "D4a") ~ 4L,
        `Program Year` %in% 2012:2025 & 
          `Qualifying Drought Event` %in%
          c("D3a") ~ 3L,
        `Program Year` %in% 2012:2025 & 
          `Qualifying Drought Event` %in%
          c("D2") ~ 1L,
        
        # payments switched again in Program Year 2026
        `Program Year` >= 2026 & 
          `Qualifying Drought Event` %in%
          c("D4b") ~ 5L,
        `Program Year`  >= 2026 & 
          `Qualifying Drought Event` %in%
          c("D3b", "D4a") ~ 4L,
        `Program Year`  >= 2026 & 
          `Qualifying Drought Event` %in%
          c("D3a") ~ 3L,
        `Program Year`  >= 2026 & 
          `Qualifying Drought Event` %in%
          c("D2b_2026") ~ 2L,
        `Program Year`  >= 2026 & 
          `Qualifying Drought Event` %in%
          c("D2a_2026") ~ 1L,
        .default = 0L
      )
  ) %>%
  dplyr::filter(`Payments` > 0L) %>%
  dplyr::group_by(FIPS, source, `Program Year`, `Pasture Type`) %>%
  dplyr::distinct(`Payments`, .keep_all = TRUE) %>%
  dplyr::filter(`Payments` == cummax(`Payments`)) %>%
  dplyr::ungroup() %>%
  dplyr::select(!c(`LFP Interval`, USDM, `LFP Weeks`)) %>%
  dplyr::mutate(source = factor(source),
                `Pasture Type` = factor(`Pasture Type`),
                `Qualifying Drought Event` = factor(`Qualifying Drought Event`))

arrow::write_parquet(lfp_payments_calculated,
                     sink = "fsa-lfp-eligibility-reanalysis.parquet",
                     version = "latest",
                     compression = "zstd",
                     compression_level = 13,
                     use_dictionary = TRUE)

## Create directory listing infrastructure
generate_tree_flat <- function(
    data_dir = "data",
    output_file = file.path("manifest.json")) {

  all_entries <-
    fs::dir_ls(data_dir, recurse = TRUE, all = TRUE, type = "file") |>
    stringr::str_subset("(^|/)[.][^/]+", negate = TRUE)

  entries <- list()

  for (entry in all_entries) {
    rel_path <- fs::path_rel(entry, start = ".")
    info <- fs::file_info(entry)
    is_dir <- fs::is_dir(entry)
    entry_data <- list(
      path = as.character(rel_path),
      size = if (is_dir) "-" else info$size,
      mtime = if (is_dir) "-" else format(info$modification_time, "%Y-%Om-%d %H:%M:%S")
    )
    entries[[length(entries) + 1]] <- entry_data
  }

  # Sort by path
  entries <- entries[order(sapply(entries, function(x) x$path))]

  jsonlite::write_json(entries, output_file, pretty = TRUE, auto_unbox = TRUE)
  message("✅ Wrote ", length(entries), " entries to ", output_file)
}

# Generate the flat index
generate_tree_flat()

## ---- Publish to S3 ---------------------------------------------------
s3_push(s3_bucket_name, paste0(s3_prefix, "/data"), "data", delete = TRUE)
s3_put(s3_bucket_name, paste0(s3_prefix, "/fsa-lfp-eligibility-reanalysis.parquet"),
       "fsa-lfp-eligibility-reanalysis.parquet",
       content_type = "application/vnd.apache.parquet",
       cache_control = "max-age=3600")
s3_put(s3_bucket_name, paste0(s3_prefix, "/usdm.parquet"),
       "usdm.parquet",
       content_type = "application/vnd.apache.parquet",
       cache_control = "max-age=3600")
s3_put(s3_bucket_name, paste0(s3_prefix, "/manifest.json"), "manifest.json",
       content_type = "application/json",
       cache_control = "max-age=3600")
s3_verify(s3_bucket_name, paste0(s3_prefix, "/data"), "data",
          allow_extra = character(0))
s3_write_manifest(s3_bucket_name, s3_prefix)
cf_invalidate(c(paste0("/", s3_prefix, "/fsa-lfp-eligibility-reanalysis.parquet"),
                paste0("/", s3_prefix, "/usdm.parquet"),
                paste0("/", s3_prefix, "/manifest.json"),
                paste0("/", s3_prefix, "/_manifest.txt")))

# TODO: when README.Rmd is ready, render it here from the freshly updated
# archive and add the commit-back step to the workflow:
#   cf_wait_manifest("https://data.sustainable-fsa.com/fsa-lfp-eligibility-reanalysis/manifest.json",
#                    "manifest.json")
#   rmarkdown::render("README.Rmd")
# (see sustainable-fsa/usdm-counties for the pattern)
