get_usdm_county <-
  function(x,
           return_data = FALSE,
           out_dir = file.path("usdm-archive", "county")){
    
    x %<>%
      lubridate::as_date() %>%
      format("%Y%m%d")
    
    cat(x, "\n")
    
    outfile <- 
      file.path(out_dir, "parquet", paste0(x,".parquet"))
    
    dirname(outfile) %>%
      dir.create(recursive = TRUE,
                 showWarnings = FALSE)
    
    if(
      !file.exists(outfile)
    ){
      
      oconus_counties <- 
        get_oconus_counties(rotate = FALSE) %>%
        dplyr::select(
          COUNTY_FIPS,
          AREA
        )
      
      sf::st_intersection(
        oconus_counties %>%
          sf::`st_agr<-`("constant"),
        get_usdm(x,
                 return_sf = TRUE,
                 rotate = FALSE) %>%
          sf::`st_agr<-`("constant")
      ) %>%
        sf::st_cast("MULTIPOLYGON") %>%
        sf::st_make_valid() %>%
        dplyr::arrange(COUNTY_FIPS, date, usdm_class) %>%
        dplyr::mutate(
          area = sf::st_area(geometry),
          percent = area / AREA
        ) %>%
        sf::st_drop_geometry() %>%
        dplyr::transmute(date, COUNTY_FIPS, usdm_class, 
                         percent = percent %>%
                           units::drop_units()) %>%
        dplyr::arrange(date, COUNTY_FIPS, usdm_class) %>%
        arrow::write_parquet(sink = outfile,
                             version = "latest",
                             compression = "zstd",
                             use_dictionary = TRUE)
    }
    
    if(!return_data){
      return(outfile)
    }
    
    return(
      outfile %>%
        arrow::read_parquet() %>%
        dplyr::mutate(
          usdm_class = 
            factor(usdm_class,
                   levels = c("None", paste0("D", 0:4)),
                   ordered = TRUE)
        )
      )
  }

update_usdm_counties <-
  function(as_of = lubridate::today(),
           out_dir = file.path("usdm-archive", "county")){
    
    plan(future.callr::callr, 
         workers = parallel::detectCores())
    
    out <-
      get_usdm_dates(as_of = as_of) %>%
      furrr::future_map_chr(get_usdm_county,
                            out_dir = out_dir,
                            .options = furrr::furrr_options(seed = TRUE))
    
    plan(sequential)
    
    return(out)
    
  }
