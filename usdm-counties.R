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
#     "rmapshaper"
#   )
# )

library(magrittr)
library(tidyverse)
library(sf)
library(arrow)
library(furrr)
library(future.mirai)


sf::sf_use_s2(TRUE)

dir.create("data-raw",
           showWarnings = FALSE,
           recursive = TRUE)

dir.create("data-derived",
           showWarnings = FALSE,
           recursive = TRUE)

dir.create(
  file.path("data-raw", "counties", "census"),
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  file.path("data-derived", "counties"),
  showWarnings = FALSE,
  recursive = TRUE
)

lfp_payments <-
  arrow::s3_bucket("/sustainable-fsa/fsa-payment-files",
                   anonymous = TRUE) %>%
  arrow::open_dataset() |>
  dplyr::filter(`Accounting Program Year` >= 2012,
                `Accounting Program Description` %in% 
                  c(
                    "LIVESTOCK FORAGE PROGRAM",
                    "LIVESTOCK FORAGE DISASTER PROGRAM"
                  )) |>
  dplyr::group_by(`Program Year` = `Accounting Program Year`, `FSA Code`) |>
  dplyr::summarise(
    `Disbursement Amount` = sum(`Disbursement Amount`, na.rm = TRUE)
  ) |>
  dplyr::collect() |>
  dplyr::mutate(`FSA Code` = as.character(`FSA Code`)) |>
  dplyr::rename(`LFP Payments` = `Disbursement Amount`)

usdm_get_dates <-
  function(as_of = lubridate::today()){
    as_of %<>%
      lubridate::as_date()
    
    usdm_dates <-
      seq(lubridate::as_date("20000104"), lubridate::today(), "1 week")
    
    usdm_dates <- usdm_dates[(as_of - usdm_dates) >= 2]
    
    return(usdm_dates)
  }

usdm_dates <- usdm_get_dates()

fsa_normal_grazing_period <-
  readr::read_csv(
    "https://sustainable-fsa.github.io/fsa-normal-grazing-period/fsa-normal-grazing-period.csv"
  ) |>
  dplyr::select(`Program Year`, 
                `FSA Code`,
                `Pasture Type`,
                `Normal Grazing Period Start Date`,
                `Normal Grazing Period End Date`) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    date = 
      list(
        tibble::tibble(
          date = usdm_dates[usdm_dates >= `Normal Grazing Period Start Date` & 
                              usdm_dates <= `Normal Grazing Period End Date`])
      )
  ) |>
  tidyr::unnest(date)

fsa_lfp_eligibility <-
  readr::read_csv(
    "https://sustainable-fsa.github.io/fsa-lfp-eligibility/fsa-lfp-eligibility.csv"
  ) |>
  dplyr::filter(`Disaster Type` == "Drought") |>
  dplyr::select(`Program Year`, `FSA Code`, `State Name`, `County Name`, `Pasture Type`, 
                `Disaster Start Date`, `Qualifying Drought Event`, 
                `Official Payments` = `Payment Type`) |>
  dplyr::mutate(`Official Payments` = 
                  `Official Payments` |>
                  stringr::str_remove(" Month") |>
                  as.integer()) |>
  dplyr::arrange(`FSA Code`, `Program Year`, `Pasture Type`, dplyr::desc(`Official Payments`)) |>
  dplyr::group_by(`FSA Code`, `Program Year`, `Pasture Type`) |>
  dplyr::filter(`Official Payments` == max(`Official Payments`))

counties <-
  c(
    # US Census, 2010-2024
    jsonlite::fromJSON(
      "https://sustainable-fsa.github.io/usdm-counties/manifest.json") %>%
      dplyr::filter(stringr::str_detect(path, "census/parquet")) %$%
      path %>%
      file.path("https://sustainable-fsa.github.io", "usdm-counties", .) %>%
      curl::multi_download(
        urls = .,
        destfiles = file.path("data-raw", "counties", "census", basename(.)),
        resume = TRUE) %$%
      destfile %>%
      fs::path_rel() %>%
      magrittr::set_names(
        .,
        basename(.) %>%
          stringr::str_remove("-counties.parquet") %>%
          paste0("Census ", .)) %>%
      purrr::imap(
        \(x, y){
          outfile <- file.path("data-derived", "counties", paste0(y, ".parquet"))
          
          if(!file.exists(outfile))
            
            sf::read_sf(x) %>%
            tidyr::unite(col = "County_FIPS", STATEFP, COUNTYFP, sep = "") |>
            dplyr::select(County_FIPS) %>%
            sf::st_transform("EPSG:4326") %>%
            dplyr::arrange(County_FIPS) %>%
            sf::st_set_geometry("geometry") %>%
            dplyr::mutate(Total = sf::st_area(geometry) %>%
                            units::drop_units()
            ) %>%
            dplyr::select(!geometry) %>%
            sf::write_sf(
              outfile,
              driver = "Parquet",
              layer_options = c("COMPRESSION=BROTLI",
                                "GEOMETRY_ENCODING=GEOARROW",
                                "WRITE_COVERING_BBOX=NO"),
            )
          
          return(outfile)
        }
      ),
    
    # NDMC Albers
    curl::multi_download(
      urls = 
        "https://sustainable-fsa.github.io/ndmc-counties-albers/Albers.gdb.zip",
      destfiles = file.path("data-raw", "counties", "Albers.gdb.zip"),
      resume = TRUE) %$%
      destfile %>% 
      fs::path_rel() %>%
      sf::read_sf(layer = "counties_detailed_all_2021") %>%
      dplyr::select(County_FIPS = CountyFIPS) %>%
      sf::st_transform("EPSG:4326") %>%
      dplyr::arrange(County_FIPS) %>%
      list(`NDMC Albers` = .) %>%
      purrr::imap(
        \(x, y){
          outfile <- file.path("data-derived", "counties", paste0(y, ".parquet"))
          
          if(!file.exists(outfile))
            x %>%
            sf::st_set_geometry("geometry") %>%
            dplyr::mutate(Total = sf::st_area(geometry) %>%
                            units::drop_units()
            ) %>%
            dplyr::select(!geometry) %>%
            sf::write_sf(
              outfile,
              driver = "Parquet",
              layer_options = c("COMPRESSION=BROTLI",
                                "GEOMETRY_ENCODING=GEOARROW",
                                "WRITE_COVERING_BBOX=NO"),
            )
          return(outfile)
        }
      ),
    
    # FSA dd17
    curl::multi_download(
      urls = 
        "https://sustainable-fsa.github.io/fsa-counties-dd17/FSA_Counties_dd17.gdb.zip",
      destfiles = file.path("data-raw", "counties", "FSA_Counties_dd17.gdb.zip"),
      resume = TRUE) %$%
      destfile %>% 
      fs::path_rel() %>%
      sf::read_sf() %>%
      dplyr::select(County_FIPS = state_county_fips_code) %>% 

      {
        # Round-trip to geojson to get rid of strange geometry
        tmp <- tempfile(fileext = ".geojson")
        sf::write_sf(., tmp,
                     delete_dsn = TRUE)
        sf::read_sf(tmp)
      } %>%
      dplyr::arrange(County_FIPS) %>%
      sf::st_transform("EPSG:4326") %>%
      dplyr::group_by(County_FIPS) %>%
      dplyr::summarise(.groups = "drop") %>%
      sf::st_cast("MULTIPOLYGON") %>%
      sf::st_cast("POLYGON", warn = FALSE, do_split = TRUE) %>%
      sf::st_make_valid() %T>%
      {suppressMessages(sf::sf_use_s2(FALSE))} %>%
      sf::st_make_valid() %T>%
      {suppressMessages(sf::sf_use_s2(TRUE))} %>%
      # Group by class and generate multipolygons
      dplyr::group_by(County_FIPS) %>%
      dplyr::summarise(.groups = "drop",
                       is_coverage = TRUE) %>%
      sf::st_cast("MULTIPOLYGON", warn = FALSE) %>%
      list(`FSA dd17` = .) %>%
      purrr::imap(
        \(x, y){
          outfile <- file.path("data-derived", "counties", paste0(y, ".parquet"))
          
          if(!file.exists(outfile))
            
            x %>%
            sf::st_set_geometry("geometry") %>%
            dplyr::mutate(Total = sf::st_area(geometry) %>%
                            units::drop_units()
            ) %>%
            dplyr::select(!geometry) %>%
            sf::write_sf(
              outfile,
              driver = "Parquet",
              layer_options = c("COMPRESSION=BROTLI",
                                "GEOMETRY_ENCODING=GEOARROW",
                                "WRITE_COVERING_BBOX=NO"),
            )
          
          return(outfile)
        }
      )
  )

dir.create(
  file.path("data-raw","usdm"),
  showWarnings = FALSE,
  recursive = TRUE
)

## USDM Archive
jsonlite::fromJSON(
  "https://sustainable-fsa.github.io/usdm/usdm-manifest.json"
) %>%
  dplyr::filter(stringr::str_detect(path, "parquet")) %$%
  path %>%
  file.path("https://sustainable-fsa.github.io", "usdm", .) %>%
  curl::multi_download(urls = .,
                       destfiles = file.path("data-raw", "usdm", basename(.)),
                       resume = TRUE)

## Intersection of Counties and USDM
extract_usdm_counties <-
  function(usdm_sf, counties_sf, outfile){
    
    if(!file.exists(outfile))
      test <- sf::st_intersection(
        counties_sf %>%
          sf::`st_agr<-`("constant"),
        usdm_sf %>%
          sf::`st_agr<-`("constant")
      ) %>%
      # sf::st_cast("MULTIPOLYGON") %>%
      sf::st_make_valid() %>%
      dplyr::mutate(
        `Percent` = units::drop_units(
          sf::st_area(geometry)
        ) / `Total`
      ) %>%
      sf::st_drop_geometry() %>%
      dplyr::select(Date = date, County_FIPS, USDM = usdm_class, Percent) %>%
      dplyr::arrange(Date, County_FIPS, USDM, Percent) %>%
      arrow::write_parquet(sink = outfile,
                           version = "latest",
                           compression = "zstd",
                           use_dictionary = TRUE)
    
    return(outfile)
  }



counties <-
  tibble::tibble(
    County_Path = fs::dir_ls("data-derived/counties",
                             recurse = TRUE,
                             type = "file")
  ) %>%
  dplyr::mutate(County_File = 
                  basename(County_Path) |>
                  stringr::str_remove(".parquet")
  ) 

counties_full <-
  counties %>%
  dplyr::filter(
    County_File %in% 
      c(
        "Census 2000",
        "Census 2010",
        "Census 2020",
        "Census 2024",
        "FSA dd17",
        "NDMC Albers"
      )
  )

usdm <-
  tibble::tibble(
    USDM = fs::dir_ls(file.path("data-raw", "usdm"))
  ) %>%
  dplyr::mutate(Date = 
                  str_extract(USDM, "\\d{4}-\\d{2}-\\d{2}") |>
                  lubridate::as_date()) |>
  dplyr::select(Date, USDM) |>
  dplyr::arrange(Date)


process_counties <-
  function(usdm, counties){
    the.counties <-
      sf::read_sf(counties)
    
    future::plan(future.callr::callr)
    # future::plan(sequential)
    
    usdm %<>%
      dplyr::mutate(
        out = 
          furrr::future_map2_chr(
            USDM, outfile,
            \(USDM, outfile){
              extract_usdm_counties(
                usdm_sf = sf::read_sf(USDM),
                counties_sf = the.counties,
                outfile = outfile
              )
              
            },
            .options = furrr::furrr_options(
              packages = c("sf")
            )
          )
      )
    
    plan(sequential)
    
    return(usdm)
  }

usdm_counties <- 
  cross_join(usdm, counties_full) %>%
  dplyr::bind_rows(
    dplyr::left_join(
      usdm %>%
        dplyr::mutate(Year = lubridate::year(Date)),
      counties %>%
        dplyr::filter(stringr::str_starts(County_File, "Census")) %>%
        dplyr::mutate(Year = str_extract(County_File, "\\d{4}") %>%
                        as.integer() + 1L) %>%
        
        tidyr::complete(Year = 1999:(lubridate::year(lubridate::today()))) %>%
        tidyr::fill(County_Path, County_File) %>%
        tidyr::fill(County_Path, County_File, .direction = "up")
    ) %>%
      dplyr::mutate(County_File = "Census Lagged") %>%
      dplyr::select(-Year)
  ) %>%
  dplyr::mutate(outfile = 
                  file.path("data-derived", "usdm", 
                            paste0("County_File=",County_File), 
                            paste0("Date=", Date),
                            paste0(County_File, "_", Date, ".parquet")
                  )
  ) %>%
  dplyr::arrange(Date, County_File) %T>%
  {
    .$outfile %>%
      dirname() %>%
      unique() %>%
      purrr::walk(
        \(x){
          dir.create(x,
                     recursive = TRUE,
                     showWarnings = FALSE)
        }
      )
  } %>%
  tidyr::nest(usdm = !c(County_Path)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    usdm_processed = list(process_counties(usdm = usdm, 
                                      counties = County_Path))
  ) %>%
  dplyr::select(usdm_processed) %>%
  tidyr::unnest(usdm_processed) %>%
  dplyr::select(County_File, out) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(out = list(arrow::read_parquet(out))) %>%
  tidyr::unnest(out) %>%
  tidyr::pivot_wider(names_from = County_File,
                     values_from = Percent) %T>%
  arrow::write_parquet(sink = file.path("data-derived", "usdm_counties.parquet"),
                       version = "latest",
                       compression = "zstd",
                       use_dictionary = TRUE)
  
  
calc_lfp_payments <-
  function(x, year){
    x <-
      x |>
      tidyr::replace_na("None") |>
      rle() %$%
      magrittr::set_names(lengths, values)
    
    if(year >= 2012)
      return(
        dplyr::case_when(
          sum(x[names(x) == "D4"]) >= 4 ~ 5L,
          sum(x[names(x) == "D3"]) >= 4 | sum(x[names(x) == "D4"]) > 0 ~ 4L,
          sum(x[names(x) == "D3"]) > 0 ~ 3L,
          any(x[names(x) == "D2"] >= 8) ~ 1L,
          .default = 0L
        )
      )
    
    return(
      dplyr::case_when(
        sum(x[names(x) == "D3"]) >= 4 | sum(x[names(x) == "D4"]) > 0 ~ 3L,
        sum(x[names(x) == "D3"]) > 0 ~ 2L,
        any(x[names(x) == "D2"] >= 8) ~ 1L,
        .default = 0L
      )
    )
  }

usdm_counties <-
  arrow::read_parquet(file.path("data-derived", "usdm_counties.parquet"))

lfp_payments_calculated <-
  fsa_normal_grazing_period |>
  dplyr::left_join(usdm_counties,
                   by = c("FSA Code" = "County_FIPS",
                          "date" = "Date"),
                   relationship = "many-to-many") |>
  dplyr::arrange(`FSA Code`, `Program Year`, `Pasture Type`, Date = date, USDM) %>%
  dplyr::group_by(`Program Year`, `FSA Code`, `Pasture Type`) %>%
  tidyr::pivot_longer(c(`Census 2000`,
                        `Census Lagged`, 
                        `Census 2010`, 
                        `Census 2020`, 
                        `Census 2024`,
                        `FSA dd17`, 
                        `NDMC Albers`),
                      values_to = "Percent",
                      names_to = "County_File") %>%
  dplyr::filter(Percent > 0) %>%
  dplyr::arrange(`Program Year`, `FSA Code`, `Pasture Type`, County_File, date) %>%
  dplyr::group_by(`Program Year`, `FSA Code`, `Pasture Type`, date, County_File) %>%
  dplyr::summarise(USDM = max(USDM, na.rm = TRUE)) %>%
  dplyr::group_by(`Program Year`, `FSA Code`, `Pasture Type`, County_File) %>%
  dplyr::arrange(date) %>%
  dplyr::summarise(usdm = list(magrittr::set_names(USDM, date))) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    `Calculated Payments` = 
      calc_lfp_payments(
        x = usdm, 
        year = `Program Year`
      )
  ) |>
  dplyr::ungroup() %>%
  dplyr::select(!usdm) %>%
  tidyr::pivot_wider(names_from = County_File,
                     values_from = `Calculated Payments`) %>%
  dplyr::arrange(`FSA Code`, `Program Year`) |>
  dplyr::full_join(fsa_lfp_eligibility) |>
  dplyr::select(!c(`State Name`, `County Name`, `Qualifying Drought Event`)) |>
  dplyr::mutate(`Official Payments` = tidyr::replace_na(`Official Payments`, 0)) %>%
  dplyr::filter(!if_all(`Census 2000`:`NDMC Albers`, ~`Official Payments` == .x))








lfp_payments_calculated %>%
  dplyr::filter(is.na(`Census 2000`))


fsa_lfp_eligibility %>%
  dplyr::filter(`FSA Code` == "46102")




usdm_counties %>%
  dplyr::arrange(County_FIPS, Date, USDM) %>%
  dplyr::group_by()












fsa_normal_grazing_period %>%
dplyr::filter(`FSA Code` == "01001", date == "2017-12-05")


eligibility <-
  
  dplyr::group_by(`Program Year`, `FSA Code`, `Pasture Type`, 
                  `Normal Grazing Period Start Date`, 
                  `Normal Grazing Period End Date`) |>
  tidyr::nest(usdm = c(date, USDM, Percent, counties_file)) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    `Calculated Payments` = 
      calc_lfp_payments(
        x = usdm$USDM, 
        year = `Program Year`
      )
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(`Calculated Payments` > 0) |>
  dplyr::full_join(fsa_lfp_eligibility)

eligibility_errors <-
  eligibility %>%
  dplyr::filter(`Calculated Payments` != `Official Payments`,
                `Program Year` >= 2012) %>%
  dplyr::arrange(`FSA Code`, `Pasture Type`, `Program Year`) %>%
  dplyr::select(
    `Program Year`, `FSA Code`,
    `State Name`, `County Name`,
    `Pasture Type`, `Normal Grazing Period Start Date`, `Normal Grazing Period End Date`,
    `FSA Disaster Start Date` = `Disaster Start Date`, 
    `FSA Qualifying Drought Event` = `Qualifying Drought Event`,
    `FSA Payments` = `Official Payments`,
    `Calculated Payments`,
    `Calculated USDM Record` = usdm
  )

eligibility_errors %>%
  dplyr::filter(`State Name` == "Montana") %>%
  print(n = 100) %$%
  `Calculated USDM Record`[[23]] %>%
  print(n = 30)



payment_errors <-
  eligibility_errors %>%
  dplyr::mutate(`Difference Payments` = `Calculated Payments` - `FSA Payments`,
                `Proportion Payments` = `Calculated Payments` / `FSA Payments`) |>
  dplyr::group_by(`Program Year`, `FSA Code`, `State Name`, `County Name`) |>
  dplyr::summarise(
    `Mean Proportion Payments` = mean(`Proportion Payments`),
    .groups = "drop"
  ) |>
  dplyr::left_join(lfp_payments) |> 
  dplyr::mutate(`Calculated LFP Payments` = `LFP Payments` * `Mean Proportion Payments`,
                `Difference LFP Payments` = `Calculated LFP Payments` - `LFP Payments`)

payment_errors |>
  dplyr::group_by(`Program Year`) |>
  dplyr::summarise(`Difference LFP Payments` = sum(`Difference LFP Payments`, na.rm = TRUE),
                   `LFP Payments` = sum(`LFP Payments`, na.rm = TRUE)) %>%
  ggplot(aes(x = `Program Year`,
             y = `Difference LFP Payments`/`LFP Payments`)) +
  geom_line() +
  ggplot2::ylim(-2,2)


sum(payment_errors$`Difference LFP Payments`, na.rm = TRUE) |>
  scales::label_currency()()
# $4,499,515,989

print(payment_errors, n = 200)

