
# Packages are provided by mt-climate-office/actions/setup-geospatial in CI.

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

## ---- Weekly USDM county comparison -----------------------------------
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

## ---- Combined weekly USDM archive ------------------------------------
## One row per Census county and USDM week, with a column per county
## aggregation carrying that week's worst class in the county.
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

## ---- Weekly LFP drought factor calculation ---------------------------
## For each USDM date and sequence of maximum county USDM categories,
## calculate the LFP drought factor history across Pasture Types

## ---------------------------------------------------------------------------
## Two county keys
##
## The Normal Grazing Period is set per FSA county (1-LFP Amend. 7, par. 27); the
## USDM county aggregations are keyed on Census county (par. 23). The two do not
## nest in either direction, so a determination needs both keys and the archive
## carries the pair unreduced. See README and qa-report.txt.
## ---------------------------------------------------------------------------

# The FSA county -> Census county (FIPS) crosswalk. dd22 is used for every
# program year: it resolves every FSA county in the Normal Grazing Period
# archive, and dd17 drops Shoshone County, ID (16079) from 2015 on.
fsa_county_crosswalk <-
  arrow::read_parquet(
    "https://data.sustainable-fsa.com/fsa-counties-dd22/fsa-counties-dd22.parquet",
    col_select = c("FSA_STCOU", "FIPS_C")
  ) |>
  dplyr::transmute(`FSA County` = FSA_STCOU, FIPS = FIPS_C) |>
  dplyr::distinct()

# The Normal Grazing Period history, at the FSA county grain FSA publishes.
fsa_normal_grazing_period_raw <-
  arrow::read_parquet(
    "https://data.sustainable-fsa.com/fsa-normal-grazing-period/fsa-normal-grazing-period.parquet"
  ) |>
  dplyr::transmute(
    `Program Year` = as.integer(`Program Year`),
    `FSA County` = stringr::str_c(`State FSA Code`, `County FSA Code`),
    `Pasture Type`,
    `Grazing Period Start Date`,
    `Grazing Period End Date`
  )

# Mapped onto Census counties. The join is many-to-many in both directions.
fsa_normal_grazing_period <-
  fsa_normal_grazing_period_raw |>
  dplyr::inner_join(fsa_county_crosswalk,
                    by = "FSA County",
                    relationship = "many-to-many") |>
  dplyr::mutate(
    `Normal Grazing Period` =
      lubridate::interval(
        start = `Grazing Period Start Date`,
        end = `Grazing Period End Date`
      )) |>
  dplyr::select(FIPS, `FSA County`, `Program Year`, `Pasture Type`, dplyr::everything()) |>
  dplyr::arrange(FIPS, `FSA County`, `Program Year`, `Pasture Type`)

# Fail with the count and a sample, so a CI log alone identifies the cause.
assert_empty <- function(offenders, what) {
  if (nrow(offenders) == 0L) {
    return(invisible(NULL))
  }
  stop("Validation failed — ", what, ": ", nrow(offenders), " record(s).\n",
       paste(
         utils::capture.output(print(utils::head(offenders, 10L), width = 200)),
         collapse = "\n"
       ),
       call. = FALSE)
}

# Input invariants, checked before the expensive USDM join.
assert_empty(
  fsa_normal_grazing_period %>%
    dplyr::count(`Program Year`, FIPS, `FSA County`, `Pasture Type`) %>%
    dplyr::filter(n > 1L),
  "duplicate (Program Year, FIPS, FSA county, Pasture Type) keys"
)

# An FSA county the NGP names but dd22 does not define cannot be placed on the
# USDM, and the inner_join above would drop it without saying so.
assert_empty(
  fsa_normal_grazing_period_raw %>%
    dplyr::distinct(`FSA County`) %>%
    dplyr::anti_join(fsa_county_crosswalk, by = "FSA County"),
  "FSA counties in the Normal Grazing Period archive absent from dd22"
)

# Collapse everything below D2 into one class; nothing below D2 qualifies.
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
  # Inner: unmatched rows would carry NA county keys.
  dplyr::inner_join(usdm_counties_rle,
                    by = "FIPS",
                    relationship = "many-to-many") %>%
  # First, filter out rows where the NGP and USDM intervals do not overlap
  # Periods below D2 do not qualify.
  dplyr::filter(
    lubridate::int_overlaps(`Normal Grazing Period`, `USDM Interval`)
  ) %>%
  # Then, calculate the intersecting intervals
  dplyr::mutate(
    `LFP Interval` = lubridate::intersect(`Normal Grazing Period`, 
                                          `USDM Interval`)
  ) %>%
  dplyr::select(FIPS,
                `FSA County`,
                source,
                `Program Year`, `Pasture Type`, `Normal Grazing Period`, 
                `USDM Interval`, `LFP Interval`, USDM, `USDM Weeks`) %>%
  dplyr::mutate(`LFP Weeks` = (lubridate::time_length(`LFP Interval`, unit = "days") + 1) / 7) %>%
  dplyr::select(FIPS,
                `FSA County`,
                source,
                `Program Year`, `Pasture Type`,
                `LFP Interval`, USDM, `LFP Weeks`) %>%
  # `tier_date()` and the D2 sandwich pattern both require runs to be in
  # chronological order within each group, so sort on the run start explicitly
  # rather than inheriting the order from the upstream arrange().
  dplyr::arrange(FIPS, `FSA County`, source, `Program Year`, `Pasture Type`,
                 lubridate::int_start(`LFP Interval`))

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

# Qualifying date for a duration-based drought tier.
#
# Convention (applies to every duration tier): the qualifying date is the LAST DAY
# of the window in which `required` weeks at `class` accumulate. `interval_end` is
# the inclusive last day of each clipped run (runs are built with
# `USDM End = usdm_date + 6`), so subtracting the overshoot lands on the last day
# of the required window rather than the day after it.
#
#   consecutive = TRUE  -> weeks must accrue within a single run (the D2 family;
#                          7 CFR 1416.110(a)(1) requires "8 consecutive weeks")
#   consecutive = FALSE -> weeks accrue across runs (D3b, D4b; the regulation says
#                          "at least 4 weeks", with no consecutiveness requirement)
#
# Expects runs to be in chronological order within the group. Returns
# seconds-since-epoch, NA everywhere except the run that satisfies the test.
tier_date <- function(usdm, class, weeks, interval_end, required, consecutive) {
  out <- rep(NA_real_, length(usdm))
  idx <- which(usdm == class)
  if (!length(idx)) return(out)
  acc <- if (consecutive) weeks[idx] else cumsum(weeks[idx])
  k   <- which(acc >= required)[1]
  if (!is.na(k)) out[idx[k]] <- interval_end[idx[k]] - (acc[k] - required) * WEEK
  out
}

lfp_eligibility_calculated <-
  lfp_usdm_calculated %>%
  dplyr::mutate(
    `LFP Interval Start` = int_start_num(`LFP Interval`),
    `LFP Interval End` = int_end_num(`LFP Interval`)
  ) %>%
  dplyr::group_by(FIPS, `FSA County`, source, `Program Year`, `Pasture Type`) %>%
  dplyr::mutate(
    `D2_Sandwich` =
      find_pattern(USDM, c("D2", "< D2", "D2")) %>%
      { .[which(`LFP Weeks`[.] < 7 &
                  `LFP Weeks`[. + 1] == 1 &
                  (`LFP Weeks`[.] + `LFP Weeks`[. + 2]) >= 7)] + 2L } %>%
      `[<-`(
        rep(NA_real_, length(USDM)),
        .,
        # Last day of the 7-week window, accrued across the two D2 runs.
        # `.` indexes the second D2 run, `. - 2` the first.
        value = `LFP Interval End`[.] -
          ((`LFP Weeks`[. - 2] + `LFP Weeks`[.]) - 7) * WEEK
      ),
    D2       = tier_date(USDM, "D2", `LFP Weeks`, `LFP Interval End`, 8, TRUE),
    D2a_2026 = tier_date(USDM, "D2", `LFP Weeks`, `LFP Interval End`, 4, TRUE),
    D2b_2026 = tier_date(USDM, "D2", `LFP Weeks`, `LFP Interval End`, 7, TRUE),
    D3b      = tier_date(USDM, "D3", `LFP Weeks`, `LFP Interval End`, 4, FALSE),
    D4b      = tier_date(USDM, "D4", `LFP Weeks`, `LFP Interval End`, 4, FALSE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    dplyr::across(c(D2_Sandwich, D2, D2a_2026, D2b_2026, D3b, D4b), secs_to_date),
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
  # Order matters: where two tiers award the same `Drought Factor` on the same
  # date, it breaks the tie in the distinct() below.
  tidyr::pivot_longer(c(D2_Sandwich, D3b, D4b, D2a_2026, D2b_2026, D2, D3a, D4a),
                      names_to = "Qualifying Drought Event",
                      values_to = "Qualifying Date") %>%
  dplyr::filter(!is.na(`Qualifying Date`)) %>%
  dplyr::arrange(FIPS, `FSA County`, source, `Program Year`, `Pasture Type`, `Qualifying Date`) %>%
  dplyr::mutate(
    `Qualifying Drought Event` = 
      ifelse(`Qualifying Drought Event` == "D2_Sandwich", 
             "D2b_2026", 
             `Qualifying Drought Event`)
  ) %>%
  # Monthly payments the tier earns. FSA's payable figure is this capped by the
  # Maximum Eligible Payment Months; this archive does not carry the cap.
  dplyr::mutate(
    `Drought Factor` =
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
        
        # 2014 Farm Bill, from Program Year 2012
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
        
        # P.L. 119-21, from Program Year 2026: D2 splits into a 4-consecutive-week
        # tier and a 7-of-8-consecutive-week tier
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
        # Events outside the era's ladder score 0 and are filtered below.
        .default = 0L
      )
  ) %>%
  dplyr::filter(`Drought Factor` > 0L) %>%
  dplyr::group_by(FIPS, `FSA County`, source, `Program Year`, `Pasture Type`) %>%
  dplyr::distinct(`Drought Factor`, .keep_all = TRUE) %>%
  dplyr::filter(`Drought Factor` == cummax(`Drought Factor`)) %>%
  dplyr::ungroup() %>%
  dplyr::select(!c(`LFP Interval`, USDM, `LFP Weeks`)) %>%
  dplyr::mutate(source = factor(source),
                `Pasture Type` = factor(`Pasture Type`),
                `Qualifying Drought Event` = factor(`Qualifying Drought Event`))

## ---------------------------------------------------------------------------
## Validation
##
## Invariants abort before write_csv(), so a bad archive reaches neither git nor
## S3. The FSA-county-to-Census-county fan-out is reported instead: it
## is a property of FSA's own administrative geography, not a defect, and the
## consumer decides how to resolve it.
## ---------------------------------------------------------------------------

assert_empty(
  lfp_eligibility_calculated %>%
    dplyr::filter(dplyr::if_any(dplyr::everything(), is.na)),
  "records with a missing value"
)

# The ladder of monthly payments in force for each era; see README.
assert_empty(
  lfp_eligibility_calculated %>%
    dplyr::filter(
      dplyr::case_when(
        `Program Year` <= 2011L ~ !(`Drought Factor` %in% 1:3),
        `Program Year` <= 2025L ~ !(`Drought Factor` %in% c(1L, 3L, 4L, 5L)),
        .default = !(`Drought Factor` %in% 1:5)
      )
    ),
  "drought factors outside the ladder in force for their program year"
)

# A qualifying date outside the grazing period would mean the tier was satisfied
# by drought outside the window FSA counts.
assert_empty(
  lfp_eligibility_calculated %>%
    dplyr::inner_join(
      fsa_normal_grazing_period %>%
        dplyr::select(FIPS, `FSA County`, `Program Year`, `Pasture Type`,
                      `Grazing Period Start Date`, `Grazing Period End Date`),
      by = c("FIPS", "FSA County", "Program Year", "Pasture Type"),
      relationship = "many-to-one"
    ) %>%
    dplyr::filter(`Qualifying Date` < `Grazing Period Start Date` |
                    `Qualifying Date` > `Grazing Period End Date`),
  "qualifying dates outside their normal grazing period"
)

## ---- The FSA county / Census county fan-out --------------------------
## Reported, never enforced.

county_pairs <-
  lfp_eligibility_calculated %>%
  dplyr::distinct(FIPS, `FSA County`)

# FSA counties covering several Census counties. Joining on FIPS replicates the
# FSA county's grazing period across every Census county it covers.
qa_fsa_spanning <-
  county_pairs %>%
  dplyr::count(`FSA County`, name = "Census Counties") %>%
  dplyr::filter(`Census Counties` > 1L) %>%
  dplyr::arrange(dplyr::desc(`Census Counties`), `FSA County`)

# Census counties administered as several FSA offices, each setting its own
# grazing period. A join on FIPS returns several rows for these.
qa_fips_split <-
  county_pairs %>%
  dplyr::count(FIPS, name = "FSA Counties") %>%
  dplyr::filter(`FSA Counties` > 1L) %>%
  dplyr::arrange(dplyr::desc(`FSA Counties`), FIPS)

# Where the fan-out actually changes the answer. A "cell" is one determination:
# a source, program year, pasture type, and county key.
qa_cells <-
  lfp_eligibility_calculated %>%
  dplyr::group_by(source, `Program Year`, `Pasture Type`, FIPS, `FSA County`) %>%
  dplyr::summarise(`Drought Factor` = max(`Drought Factor`), .groups = "drop")

qa_fsa_disagree <-
  qa_cells %>%
  dplyr::filter(`FSA County` %in% qa_fsa_spanning$`FSA County`) %>%
  dplyr::group_by(source, `Program Year`, `Pasture Type`, `FSA County`) %>%
  dplyr::summarise(Lowest = min(`Drought Factor`),
                   Highest = max(`Drought Factor`),
                   .groups = "drop") %>%
  dplyr::filter(Highest > Lowest)

qa_fips_disagree <-
  qa_cells %>%
  dplyr::filter(FIPS %in% qa_fips_split$FIPS) %>%
  dplyr::group_by(source, `Program Year`, `Pasture Type`, FIPS) %>%
  dplyr::summarise(Lowest = min(`Drought Factor`),
                   Highest = max(`Drought Factor`),
                   .groups = "drop") %>%
  dplyr::filter(Highest > Lowest)

qa_cells_spanning <-
  qa_cells %>%
  dplyr::filter(`FSA County` %in% qa_fsa_spanning$`FSA County`) %>%
  dplyr::distinct(source, `Program Year`, `Pasture Type`, `FSA County`) %>%
  nrow()

qa_cells_split <-
  qa_cells %>%
  dplyr::filter(FIPS %in% qa_fips_split$FIPS) %>%
  dplyr::distinct(source, `Program Year`, `Pasture Type`, FIPS) %>%
  nrow()

# Detail tables as indented CSV; a tibble's print wraps wide frames across
# several blocks.
qa_detail <- function(x) {
  if (nrow(x) == 0L) {
    return(character(0))
  }
  paste0("  ", strsplit(readr::format_csv(x), "\n", fixed = TRUE)[[1]])
}

qa_report <- c(
  "FSA LFP eligibility reanalysis — QA report",
  "",
  "Grain: one record per Census county (FIPS), FSA county, USDM county",
  "aggregation, program year, pasture type, and qualifying drought event. Both",
  "county keys are carried because an LFP determination needs both: FSA sets the",
  "normal grazing period per FSA county, and the USDM is aggregated per Census",
  "county. Neither key is reduced away — see \"Resolving the fan-out\" below.",
  "",
  paste0("Records published: ", nrow(lfp_eligibility_calculated)),
  paste0("County pairs: ", nrow(county_pairs)),
  paste0("Census counties: ",
         dplyr::n_distinct(lfp_eligibility_calculated$FIPS)),
  paste0("FSA counties: ",
         dplyr::n_distinct(lfp_eligibility_calculated$`FSA County`)),
  paste0("Program years: ",
         paste(range(lfp_eligibility_calculated$`Program Year`),
               collapse = "-")),
  paste0("Pasture types: ",
         dplyr::n_distinct(lfp_eligibility_calculated$`Pasture Type`)),
  paste0("USDM county aggregations: ",
         dplyr::n_distinct(lfp_eligibility_calculated$source)),
  "",
  "Invariants enforced (the run aborts on any violation):",
  "  * exactly one grazing period per (program year, Census county, FSA county,",
  "    pasture type)",
  "  * every FSA county resolves against FSA's published county definitions",
  "  * no missing values in any published field",
  "  * every drought factor within the ladder in force for its program year",
  "  * every qualifying date inside its normal grazing period",
  "",
  "Resolving the fan-out",
  "",
  "  FSA counties and Census counties do not nest, in either direction. This",
  "  archive reports every (Census county, FSA county) pair and combines none of",
  "  them. Reducing to a single county grain is the consumer's decision, and the",
  "  tables below are the records that decision touches. There is no default:",
  "  taking the highest drought factor, taking the FSA county whose code matches",
  "  the FIPS code, and keeping both keys are all defensible, and they disagree",
  "  on the records listed here.",
  "",
  paste0("FSA counties covering several Census counties: ",
         nrow(qa_fsa_spanning)),
  "  Joining on FIPS replicates one reported grazing period across every Census",
  "  county the FSA office covers.",
  qa_detail(qa_fsa_spanning),
  "",
  paste0("Census counties administered as several FSA counties: ",
         nrow(qa_fips_split)),
  "  Each FSA office sets its own grazing period, so a join on FIPS returns",
  "  several determinations for these.",
  qa_detail(qa_fips_split),
  "",
  paste0("Determinations where the Census counties disagree: ",
         nrow(qa_fsa_disagree), " of ", qa_cells_spanning),
  "  Collapsing to FSA county grain changes the drought factor for these.",
  qa_detail(qa_fsa_disagree),
  "",
  paste0("Determinations where the FSA counties disagree: ",
         nrow(qa_fips_disagree), " of ", qa_cells_split),
  "  Collapsing to Census county grain changes the drought factor for these.",
  qa_detail(qa_fips_disagree),
  ""
)

writeLines(qa_report, "qa-report.txt")

message(paste(qa_report, collapse = "\n"))

# Mirrored CSV and Parquet, identical records. CSV carries no types, so codes like
# "01" read back as 1; Parquet keeps them character and dates as dates.
readr::write_csv(lfp_eligibility_calculated,
                 "fsa-lfp-eligibility-reanalysis.csv")
arrow::write_parquet(lfp_eligibility_calculated,
                     sink = "fsa-lfp-eligibility-reanalysis.parquet",
                     version = "latest",
                     compression = "zstd",
                     compression_level = 13,
                     use_dictionary = TRUE)

## ---- Directory listing infrastructure --------------------------------
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
s3_put(s3_bucket_name, paste0(s3_prefix, "/fsa-lfp-eligibility-reanalysis.csv"),
       "fsa-lfp-eligibility-reanalysis.csv",
       content_type = "text/csv",
       cache_control = "max-age=3600")
s3_put(s3_bucket_name, paste0(s3_prefix, "/fsa-lfp-eligibility-reanalysis.parquet"),
       "fsa-lfp-eligibility-reanalysis.parquet",
       content_type = "application/vnd.apache.parquet",
       cache_control = "max-age=3600")
s3_put(s3_bucket_name, paste0(s3_prefix, "/usdm.parquet"),
       "usdm.parquet",
       content_type = "application/vnd.apache.parquet",
       cache_control = "max-age=3600")
s3_put(s3_bucket_name, paste0(s3_prefix, "/qa-report.txt"), "qa-report.txt",
       content_type = "text/plain",
       cache_control = "max-age=3600")
s3_put(s3_bucket_name, paste0(s3_prefix, "/manifest.json"), "manifest.json",
       content_type = "application/json",
       cache_control = "max-age=3600")
s3_verify(s3_bucket_name, paste0(s3_prefix, "/data"), "data",
          allow_extra = character(0))
s3_write_manifest(s3_bucket_name, s3_prefix)
cf_invalidate(c(paste0("/", s3_prefix, "/fsa-lfp-eligibility-reanalysis.csv"),
                paste0("/", s3_prefix, "/fsa-lfp-eligibility-reanalysis.parquet"),
                paste0("/", s3_prefix, "/usdm.parquet"),
                paste0("/", s3_prefix, "/qa-report.txt"),
                paste0("/", s3_prefix, "/manifest.json"),
                paste0("/", s3_prefix, "/_manifest.txt")))

## ---- Render the README -----------------------------------------------
# Regenerates README.md and the example map from the freshly updated archive;
# the workflow commits these (and only these) back to git.
cf_wait_manifest(
  "https://data.sustainable-fsa.com/fsa-lfp-eligibility-reanalysis/manifest.json",
  "manifest.json"
)
rmarkdown::render("README.Rmd")
