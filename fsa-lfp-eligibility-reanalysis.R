#' ---
#' title: For good neighbors and good government, know your boundaries
#' format: html
#' editor: source
#' editor_options:
#'   chunk_output_type: console
#' ---
#' 

#| echo: false

# update.packages(repos = "https://cran.rstudio.com/",
#                 ask = FALSE)

# install.packages("pak",
#                  repos = "https://cran.rstudio.com/")

# installed.packages() |>
#   rownames() |>
#   pak::pkg_install(upgrade = TRUE,
#                  ask = FALSE)

# pak::pak(
#   c(
#     "arrow?source",
#     "sf?source",
#     "curl",
#     "tidyverse",
#     "tigris",
#     "rmapshaper",
#     "furrr",
#     "future.mirai",
#     "dtplyr",
#     "ggspatial"
#   )
# )

library(magrittr)
library(tidyverse)
library(sf)
library(arrow)
library(furrr)
library(future.mirai)
library(dtplyr)

usdm_get_dates <-
  function(as_of = lubridate::today("America/Denver")){
    as_of %<>%
      lubridate::as_date()
    
    usdm_dates <-
      seq(lubridate::as_date("20000104"), lubridate::today(), "1 week")
    
    usdm_dates <- usdm_dates[(as_of - usdm_dates) >= 2]
    
    return(usdm_dates)
  }

fsa_lfp_counties_dates <-
  sf::read_sf(
    "https://data.sustainable-fsa.com/usdm-counties-fsa-lfp/data/fsa-lfp-counties.parquet"
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::transmute(FIPS = paste0(STATEFP, COUNTYFP)) %>%
  dplyr::distinct() %>%
  dplyr::cross_join(
    tibble::tibble(date = usdm_get_dates())
  )

fsa_lfp_counties <-
  sf::read_sf(
    "https://data.sustainable-fsa.com/usdm-counties-fsa-lfp/data/fsa-lfp-counties.parquet"
  ) %>%
  dplyr::transmute(FIPS = paste0(STATEFP, COUNTYFP)) %>%
  dplyr::distinct() %>%
  dplyr::arrange(FIPS)

census_2020_counties <-
  tigris::counties(year = 2020) %>%
  dplyr::transmute(FIPS = paste0(STATEFP, COUNTYFP)) %>%
  dplyr::distinct() %>%
  dplyr::arrange(FIPS)

dplyr::anti_join(
  sf::st_drop_geometry(census_2020_counties),
  sf::st_drop_geometry(fsa_lfp_counties)
)


fsa_normal_grazing_period <-
  readr::read_csv(
    "https://data.sustainable-fsa.com/fsa-normal-grazing-period/fsa-normal-grazing-period.csv"
  ) |>
  dplyr::select(`Program Year`,
                FIPS = `FSA Code`,
                `Pasture Type`,
                `Normal Grazing Period Start Date`,
                `Normal Grazing Period End Date`) %>%
  dplyr::mutate(
    `Normal Grazing Period` = 
      lubridate::interval(
        start = `Normal Grazing Period Start Date`,
        end = `Normal Grazing Period End Date`
      )) %>%
  dplyr::arrange(`Program Year`, FIPS, `Pasture Type`)

fsa_lfp_eligibility <-
  readr::read_csv(
    "https://data.sustainable-fsa.com/fsa-lfp-eligibility/fsa-lfp-eligibility.csv"
  ) |>
  dplyr::filter(`Disaster Type` == "Drought") %>%
  dplyr::mutate(FIPS = paste0(`FIPS State Code`, `FIPS County Code`)) %>%
  dplyr::select(`Program Year`,
                FIPS,
                `Pasture Type`,
                `D2 START DATE`:`Note (FOIA 2025-FSA-04690-F Bocinsky)`) %>%
  tidyr::pivot_longer(`D2 START DATE`:`D4B END`, 
                      names_to = "Qualifying Drought Event",
                      values_to = "Date"
  ) %>%
  tidyr::separate_wider_delim(cols = `Qualifying Drought Event`,
                              delim = " ",
                              names = c("CQualifying Drought Event", "Type"),
                              too_many = "merge") %>%
  dplyr::filter(!is.na(Date)) %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(names_from = Type,
                     values_from = Date) %>%
  dplyr::filter(!is.na(`START DATE`),
                !is.na(`Drought Factor`))

usdm_counties <-
  jsonlite::fromJSON(
    "https://data.sustainable-fsa.com/usdm-counties/manifest.json"
  )$path |>
  stringr::str_subset("parquet") %>%
  stringr::str_subset("usdm") %>%
  tibble::tibble(parquet = .) %>%
  dplyr::arrange(parquet) %>%
  dplyr::mutate(date = parquet |>
                  stringr::str_extract("\\d{4}-\\d{2}-\\d{2}") |>
                  lubridate::as_date()) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    data = list(
      arrow::read_parquet(
        file.path(
          # "https://sustainable-fsa.com/usdm-counties",
          "../usdm-counties",
          parquet)
      )
    )
  ) %>%
  dplyr::ungroup() %>%
  tidyr::unnest(data) %>%
  dplyr::mutate(
    COUNTYFP = dplyr::case_when(
      STATEFP == "46" & COUNTYFP == "113" ~ "102", # Shannon, SD to Oglala Lakota, SD
      .default = COUNTYFP
    )
  ) %>%
  dplyr::group_by(date, 
                  FIPS = paste0(STATEFP, COUNTYFP)) %>%
  dplyr::summarise(usdm_class = max(usdm_class),
                   .groups = "drop") %>%
  dplyr::right_join(
    fsa_lfp_counties_dates
  ) %>%
  dplyr::select(FIPS, date, usdm_class) %>%
  dplyr::mutate(
    usdm_class = 
      tidyr::replace_na(usdm_class, "None") %>%
      factor(levels = c("None", paste0("D", 0:4)),
             ordered = TRUE)
  ) %>%
  dplyr::arrange(FIPS, date) %>%
  dplyr::filter(!stringr::str_starts(FIPS, "60"),
                !stringr::str_starts(FIPS, "66"),
                !stringr::str_starts(FIPS, "69"),
                !stringr::str_starts(FIPS, "78"))

usdm_counties_fsa_lfp <-
  file.path("https://data.sustainable-fsa.com/usdm-counties-fsa-lfp",
            "usdm-counties-fsa-lfp.parquet") %>%
  arrow::read_parquet() %>%
  dtplyr::lazy_dt() %>%
  dplyr::group_by(
    FIPS = paste0(STATEFP, COUNTYFP),
    date = usdm_date
  ) %>%
  dplyr::summarise(
    usdm_class = max(usdm_class),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    FIPS, 
    date
  ) %>%
  dplyr::collect()

calc_lfp_payments <-
  function(counties){
    usdm_counties_rle <-
      counties %>%
      # Implement a version of run-length encoding
      lazy_dt() %>%
      dplyr::group_by(FIPS) %>%
      # Add a column for end date (`USDM End`) of the week's USDM map (inclusive) and 
      # create an index (`group`) of changes in the county status
      dplyr::mutate(`USDM End` = date + 6,
                    group = cumsum(c(0, diff(usdm_class)) != 0)) %>%
      dplyr::group_by(FIPS, usdm_class, group) %>%
      # By group, calculate the start and end date (inclusive) of the county USDM status, and 
      # the number of weeks in that status.
      dplyr::summarise(
        `USDM Start` = min(date),
        `USDM End` = max(`USDM End`),
        `USDM Weeks` = n(),
        .groups = "drop") %>%
      as_tibble() %>%
      dplyr::mutate(`USDM Interval` = lubridate::interval(start = `USDM Start`,
                                                          end = `USDM End`)) %>%
      dplyr::select(FIPS,  
                    `USDM Start`, `USDM End`, 
                    `USDM Interval`, `USDM Weeks`,
                    USDM = usdm_class) %>%
      dplyr::arrange(FIPS, `USDM Start`)
    
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
      dplyr::filter(lubridate::int_overlaps(`Normal Grazing Period`, `USDM Interval`),
                    USDM >= "D2") %>%
      # Then, calculate the intersecting intervals
      dplyr::mutate(
        `LFP Interval` = lubridate::intersect(`Normal Grazing Period`, 
                                              `USDM Interval`)
      ) %>%
      dplyr::select(FIPS, 
                    `Program Year`, `Pasture Type`, `Normal Grazing Period`, 
                    `USDM Interval`, `LFP Interval`, USDM, `USDM Weeks`) %>%
      dplyr::mutate(`LFP Weeks` = (lubridate::time_length(`LFP Interval`, unit = "days") + 1) / 7) %>%
      dplyr::select(FIPS, 
                    `Program Year`, `Pasture Type`, 
                    `LFP Interval`, USDM, `USDM Weeks`, `LFP Weeks`) %>%
      dplyr::arrange(FIPS, `Program Year`, `Pasture Type`) %>%
      dplyr::mutate(
        `Disaster Start Date` = 
          dplyr::case_when(
            USDM == "D2" & `LFP Weeks` >= 8 ~
              as.character(lubridate::int_start(`LFP Interval`) + lubridate::weeks(8)),
            USDM == "D3" ~
              as.character(lubridate::int_start(`LFP Interval`)),
            USDM == "D4" ~
              as.character(lubridate::int_start(`LFP Interval`)),
            .default = as.character(NA)
          ) %>%
          lubridate::as_date()
      )
    
    lfp_payments_calculated <-
      lfp_usdm_calculated %>%
      dplyr::group_by(FIPS, `Program Year`, `Pasture Type`) %>%
      dplyr::filter(!all(is.na(`Disaster Start Date`))) %>%
      dplyr::summarise(
        `Calculated Qualifying Drought Event` = 
          case_when(
            # Rules for program years 2012--2024
            sum(`LFP Weeks`[USDM == "D4"]) >= 4 ~ "D4b",
            sum(`LFP Weeks`[USDM == "D3"]) >= 4 & 
              sum(`LFP Weeks`[USDM == "D4"]) > 0 ~ "D3b & D4a",
            sum(`LFP Weeks`[USDM == "D3"]) >= 4 ~ "D3b",
            sum(`LFP Weeks`[USDM == "D4"]) > 0 ~ "D4a",
            sum(`LFP Weeks`[USDM == "D3"]) > 0 ~ "D3a",
            any(`LFP Weeks`[USDM == "D2"] >= 8) ~ "D2",
            .default = NA
          ),
        `Calculated Disaster Start Date` = 
          min(`Disaster Start Date`, 
              na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        `Calculated Payments` = 
          dplyr::case_when(
            `Program Year` < 2012 & 
              `Calculated Qualifying Drought Event` %in%
              c("D4b", "D3b & D4a", "D3b", "D4a") ~ 3L,
            `Program Year` < 2012 & 
              `Calculated Qualifying Drought Event` %in%
              c("D3a") ~ 2L,
            `Program Year` < 2012 & 
              `Calculated Qualifying Drought Event` %in%
              c("D2") ~ 1L,
            
            # Calculated payments switched in Program Year 2012
            `Program Year` >= 2012 & 
              `Calculated Qualifying Drought Event` %in%
              c("D4b") ~ 5L,
            `Program Year` >= 2012 & 
              `Calculated Qualifying Drought Event` %in%
              c("D3b & D4a", "D3b", "D4a") ~ 4L,
            `Program Year` >= 2012 & 
              `Calculated Qualifying Drought Event` %in%
              c("D3a") ~ 3L,
            `Program Year` >= 2012 & 
              `Calculated Qualifying Drought Event` %in%
              c("D2") ~ 1L,
            .default = 0L
          )
      )
    
    return(lfp_payments_calculated)
  }


lfp_payments_usdm_counties <- calc_lfp_payments(usdm_counties)
lfp_payments_usdm_counties_fsa_lfp <- calc_lfp_payments(usdm_counties_fsa_lfp)


payment_diffs <-
  dplyr::full_join(
    fsa_lfp_eligibility %>%
      dplyr::transmute(
        FIPS, 
        `Program Year`, 
        `Pasture Type`, 
        `FSA Date of Qualifying Drought` = `Date of Qualifying Drought`, 
        `FSA Drought Factor` = as.integer(`Drought Factor`),
        `FSA Maximum Eligible Payment Months` = as.integer(`Maximum Eligible Payment Months`),
        `FSA Payments` = as.integer(`Payment Factor`)
      ) %>%
      dplyr::distinct(),
    lfp_payments_calculated %>%
      dplyr::select(
        FIPS, 
        `Program Year`, 
        `Pasture Type`, 
        `Calculated Date of Qualifying Drought` = `Calculated Disaster Start Date`,
        `Calculated Payments`
      ) %>%
      dplyr::distinct(),
    by = join_by(FIPS, `Program Year`, `Pasture Type`)
  ) %>%
  dplyr::filter(`FSA Drought Factor` != `Calculated Payments`) %>%
  dplyr::arrange(dplyr::desc(`Program Year`))

payment_diffs %>%
  dplyr::group_by(FIPS) %>%
  dplyr::count()

fsa_counties <-
  sf::read_sf("https://data.sustainable-fsa.com/fsa-lfp-counties/fsa-lfp-counties.parquet") %>%
  dplyr::transmute(STATEFP = StateFIPS, COUNTYFP = stringr::str_sub(CountyFIPS, start = 3L, end = 5L))

census_counties_2000 <- 
  sf::read_sf("https://data.sustainable-fsa.com/usdm-counties/data/census/parquet/2010-counties.parquet") %>%
  dplyr::select(STATEFP, COUNTYFP) %>%
  sf::st_transform(sf::st_crs(fsa_counties))

census_counties_2024 <- 
  sf::read_sf("https://data.sustainable-fsa.com/usdm-counties/data/census/parquet/2024-counties.parquet") %>%
  dplyr::select(STATEFP, COUNTYFP) %>%
  sf::st_transform(sf::st_crs(fsa_counties))


fsa_counties %>%
  dplyr::transmute(FIPS = paste0(STATEFP, COUNTYFP)) %>%
  dplyr::right_join(
    payment_diffs %>%
      dplyr::group_by(FIPS) %>%
      dplyr::count()
  ) %>%
  mapview::mapview(zcol = "n")

httr2::url_modify("https://usdmdataservices.unl.edu",
                  path = "/api/USStatistics/GetDroughtSeverityStatisticsByArea",
                  query = list(
                    aoi="30063",
                    startdate="1/1/2000",
                    enddate = "9/22/2025",
                    statisticsType="2"
                  )) %>%
  readr::read_csv()



usdm_counties_ndmc <-
  readr::read_csv("usdm_export_20000104_20250922.csv",
                  col_types = cols(.default = col_character())) %>%
  dplyr::select(FIPS,
                date = MapDate,
                None:D4) %>%
  tidyr::pivot_longer(None:D4,
                      names_to = "usdm_class") %>%
  dplyr::mutate(date = lubridate::as_date(date),
                usdm_class = factor(usdm_class,
                                    levels = c("None", paste0("D", 0:4)),
                                    ordered = TRUE),
                value = as.numeric(value)) %>%
  dplyr::arrange(FIPS, date) %>%
  dplyr::filter(value > 0) %>%
  dplyr::group_by(FIPS, date) %>%
  dplyr::summarise(usdm_class = max(usdm_class),
                   .groups = "drop") %>%
  tidyr::complete(FIPS, date,
                  fill = list(usdm_class = "None")) %>%
  dplyr::arrange(FIPS, date)


mapview_usdm_county_week <-
  function(county_fips = "01001",
           date = "2018-04-10"){
    
    counties <-
      list(`FSA Counties` = fsa_counties,
           `Census 2000` = census_counties_2000,
           `Census 2024` = census_counties_2024) %>%
      purrr::map_dfr(
        \(x){
          x %>%
            dplyr::mutate(FIPS = paste0(STATEFP, COUNTYFP)) %>%
            dplyr::filter(FIPS == county_fips)
        },
        .id = "Counties"
      )
    
    drought <-
      date %>%
      lubridate::as_date() %>%
      paste0("https://data.sustainable-fsa.com/usdm/data/parquet/USDM_",.,".parquet") %>%
      sf::read_sf()
    
    mapview::mapview(list(counties, drought))
  }


usdm_counties_compare <-
  dplyr::full_join(
    usdm_counties_ndmc %>%
      dplyr::rename(`FSA Counties` = usdm_class),
    usdm_counties %>%
      dplyr::rename(`Census Counties` = usdm_class)
  ) %>%
  dplyr::filter(
    `FSA Counties` != `Census Counties`
  )

usdm_counties_compare %>%
  dplyr::arrange(FIPS, date) %>%
  dplyr::group_by(FIPS) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    fsa_counties %>%
      dplyr::transmute(FIPS = paste0(STATEFP, COUNTYFP))
    , .) %>%
  dplyr::mutate(n = tidyr::replace_na(n, 0),
                n = ifelse(n > 50, 50, n)) %>%
  
  mapview::mapview(zcol = "n")

usdm_counties_compare %>%
  dplyr::filter(FIPS == "32011") %>%
  print(n = 50)

payment_diffs %>%
  dplyr::filter(FIPS == "32011",
                `Program Year` == 2021)




mapview_usdm_county_week(
  county_fips = "32011",
  date = "2021-08-10"
)

usdm_counties_ndmc %>%
  dplyr::filter(FIPS == "01007",
                date == "2019-09-24")



### County-based USDM comparison ###
usdm_counties_compare <-
  dplyr::full_join(
    usdm_counties_fsa_lfp %>%
      dplyr::rename(`FSA Counties` = usdm_class),
    usdm_counties %>%
      dplyr::rename(`Census Counties` = usdm_class)
  ) %>%
  dplyr::filter(
    `FSA Counties` != `Census Counties`
  )

usdm_counties_compare %>%
  nrow()
# 8,653 County-Weeks

usdm_counties_compare %>%
  dplyr::filter(
    `FSA Counties` >= "D2" | `Census Counties` >= "D2"
  ) %>%
  nrow()
# 2,687 County-Weeks

usdm_counties_compare %>%
  dplyr::filter(FIPS == "02063") %>%
  print(n = 1000)

fsa_lfp_counties %>%
  dplyr::filter(STATEFP == "02",
                COUNTYFP == "063")

census_counties %>%
  dplyr::filter(STATEFP == "02",
                COUNTYFP == "063")



dplyr::group_by(FIPS) %>%
  count() %>%
  dplyr::filter(n > 100)

lfp_payments_compare <-
  dplyr::full_join(
    lfp_payments_usdm_counties %>%
      dplyr::select(FIPS, `Program Year`, `Pasture Type`, 
                    `FSA Counties` = `Calculated Payments`),
    lfp_payments_usdm_counties_fsa_lfp %>%
      dplyr::select(FIPS, `Program Year`, `Pasture Type`, 
                    `Census Counties` = `Calculated Payments`)
  ) %>%
  dplyr::filter(
    `FSA Counties` != `Census Counties`
  )

# 120 County-Pasture-Years




