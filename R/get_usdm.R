get_usdm <-
  function(x, 
           return_sf = FALSE,
           rotate = TRUE,
           out_dir = "usdm-archive/usdm"){
    
    raw_dir <-
      file.path(out_dir, "raw") %T>%
      dir.create(recursive = TRUE,
                 showWarnings = FALSE)
    
    parquet_dir <-
      file.path(out_dir, "parquet") %T>%
      dir.create(recursive = TRUE,
                 showWarnings = FALSE)
    
    x %<>%
      lubridate::as_date() %>%
      format("%Y%m%d")
    
    parquet_out <- file.path(parquet_dir, paste0(x,".parquet"))
    
    if(
      !file.exists(parquet_out)
    ){
      
      usdm_url <-
        paste0("https://droughtmonitor.unl.edu/data/shapefiles_m/USDM_",x,"_M.zip")
      
      curl::curl_download(url = usdm_url,
                          destfile = 
                            file.path(raw_dir, basename(usdm_url)))
      
      suppressMessages({
        
        sf::read_sf(
          file.path("/vsizip", out_dir, "raw", basename(usdm_url))
        ) %>%
          dplyr::select(DM) %>%
          # sf::st_cast("MULTIPOLYGON") %>%
          # sf::st_cast("POLYGON", warn = FALSE, do_split = TRUE) %>%
          sf::st_make_valid() %>%
          sf::st_cast("MULTIPOLYGON") %>%
          sf::st_cast("POLYGON", warn = FALSE, do_split = TRUE) %>%
          sf::st_make_valid() %>%
          sf::st_transform("ESRI:102003") %>%
          # sf::st_cast("MULTIPOLYGON") %>%
          # sf::st_cast("POLYGON", warn = FALSE, do_split = TRUE) %>%
          sf::st_make_valid() %>%
          # dplyr::group_by(DM) %>%
          # dplyr::summarise() %>%

          dplyr::transmute(usdm_class = factor(paste0("D", DM),
                                               levels = c("None", paste0("D", 0:4)),
                                               ordered = TRUE)) %>%
          # Group by date and class, and generate multipolygons
          dplyr::group_by(usdm_class) %>%
          dplyr::summarise(.groups = "drop",
                           is_coverage = TRUE) %>%
          sf::st_cast("MULTIPOLYGON", warn = FALSE) %>%
          sf::st_make_valid() %>%
          sf::st_cast("MULTIPOLYGON", warn = FALSE) %>%
          dplyr::arrange(usdm_class) %>%
          dplyr::mutate(date = lubridate::as_date(x)) %>%
          dplyr::select(date, usdm_class) %>%
          # sf::`st_agr<-`("constant") %>%
          # sf::st_intersection(
          #   get_oconus(rotate = FALSE) %>%
          #     sf::st_geometry()
          # ) %>%
          sf::st_transform("OGC:CRS84") %>%
          # sf::st_cast("MULTIPOLYGON", warn = FALSE) %>%
          # sf::st_make_valid() %>%
          sf::st_cast("MULTIPOLYGON", warn = FALSE) %>%
          sf::st_cast("POLYGON", warn = FALSE, do_split = TRUE) %>%
          sf::st_make_valid() %T>%
          {sf::sf_use_s2(FALSE)} %>%
          dplyr::group_by(usdm_class, date) %>%
          dplyr::summarise(.groups = "drop",
                           is_coverage = TRUE) %T>%
          {sf::sf_use_s2(TRUE)} %>%
          sf::st_cast("MULTIPOLYGON", warn = FALSE) %>%
          sf::write_sf(parquet_out,
                       layer_options = c("COMPRESSION=BROTLI",
                                         "GEOMETRY_ENCODING=GEOARROW",
                                         "WRITE_COVERING_BBOX=NO"),
                       driver = "Parquet")
      })
      
    }
    
    if(!return_sf){
      return(parquet_out)
    }
    
    if(rotate){
      return(
        sf::read_sf(parquet_out) %>%        
          dplyr::mutate(
            usdm_class = 
              factor(usdm_class,
                     levels = c("None", paste0("D", 0:4)),
                     ordered = TRUE)
          ) %>%
          sf::st_cast("MULTIPOLYGON",
                      warn = FALSE) %>%
          sf::st_cast("POLYGON",
                      warn = FALSE) %>%
          tigris::shift_geometry() %>%
          sf::st_make_valid() %>%
          dplyr::group_by(date, usdm_class) %>%
          dplyr::summarise(.groups = "drop") %>%
          sf::st_cast("MULTIPOLYGON", warn = FALSE)
      )
    }
    
    return(
      sf::read_sf(parquet_out) %>%
        dplyr::mutate(
          usdm_class = 
            factor(usdm_class,
                   levels = c("None", paste0("D", 0:4)),
                   ordered = TRUE)
        )
    )
  }

get_usdm_dates <-
  function(as_of = lubridate::today()){
    as_of %<>%
      lubridate::as_date()
    
    usdm_dates <-
      seq(lubridate::as_date("20000104"), lubridate::today(), "1 week")
    
    usdm_dates <- usdm_dates[(as_of - usdm_dates) >= 2]
    
    return(usdm_dates)
  }
