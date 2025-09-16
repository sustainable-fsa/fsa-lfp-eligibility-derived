update_droughtlook <-
  function(
    out_dir = "usdm-archive/droughtlook"){
    
    raw_dir <-
      file.path(out_dir, "raw") %T>%
      dir.create(recursive = TRUE,
                 showWarnings = FALSE)
    
    parquet_dir <-
      file.path(out_dir, "parquet") %T>%
      dir.create(recursive = TRUE,
                 showWarnings = FALSE)
    
    png_dir <-
      file.path(out_dir, "png") %T>%
      dir.create(recursive = TRUE,
                 showWarnings = FALSE)
    
    droughtlooks <-
      httr::GET("https://ftp.cpc.ncep.noaa.gov/GIS/droughtlook") %>%
      httr::content() %>%
      xml2::xml_find_all(".//a") %>%
      xml2::xml_attr("href") %>%
      stringr::str_subset("sdo|mdo") %>%
      stringr::str_subset("latest", negate = TRUE) %>%
      # There is something invalid about this particular shapefile, 
      # so we're dropping it.
      stringr::str_subset("20240930.*sdo|sdo.*20240930", negate = TRUE)
    
    ## Download all raw droughtlooks
    plan(future.callr::callr, workers = parallel::detectCores())
    
    droughtlooks %<>%
      furrr::future_map_chr(
        \(x){
          if(file.exists(
            file.path(raw_dir, x)
          ))
            return(file.path(raw_dir, x))
          
          curl::curl_download(url = file.path("https://ftp.cpc.ncep.noaa.gov/GIS/droughtlook", x),
                              destfile = 
                                file.path(raw_dir, x))
        },
        .options = furrr::furrr_options(seed = TRUE)
      )
    
    plan(sequential)
    
    
    ## process all raw droughtlooks
    plan(future.callr::callr, workers = parallel::detectCores())
    
    suppressMessages({
      droughtlooks %<>%
        furrr::future_map_chr(
          \(x){
            
            droughtlook_scale <- ifelse(stringr::str_detect(x, "mdo"), "monthly", "seasonal")
            droughtlook_date <- stringr::str_extract(x, stringr::regex("([0-9]{8})"))
            
            target_date <-
              ifelse(stringr::str_detect(x, "mdo"),
                     droughtlook_date %>%
                       lubridate::as_date() %>%
                       lubridate::add_with_rollback(months(1)),
                     droughtlook_date %>%
                       lubridate::as_date() %>%
                       lubridate::add_with_rollback(months(3))
              )
            
            parquet_out <-
              file.path(parquet_dir, paste0(droughtlook_date, "_", droughtlook_scale,".parquet"))
            
            if(!file.exists(parquet_out)){
              # sf::sf_use_s2(FALSE)
              
              extraction <-
                tempdir()
              
              archive::archive_extract(x, extraction)
              
              file.path(extraction, "DO_Merge_Clip.shp") %>%
                sf::read_sf() %>%
                sf::st_make_valid() %>%
                sf::st_transform("ESRI:102003") %>%
                dplyr::transmute(
                  date = droughtlook_date %>%
                    lubridate::as_date(), 
                  target_date = target_date %>%
                    lubridate::as_date(), 
                  FID_Remove, 
                  FID_improv, 
                  FID_persis, 
                  FID_dev) %>%
                # sf::st_cast("POLYGON", warn = FALSE) %>%
                # sf::st_make_valid() %>%
                # sf::st_transform("OGC:CRS84") %>%
                # sf::st_make_valid() %>%
                tidyr::pivot_longer(!c(date, target_date, geometry), 
                                    names_to = "outlook",
                                    values_to = "state") %>%
                dplyr::filter(state == 1) %>%
                dplyr::transmute(date,
                                 outlook = factor(outlook,
                                                  levels = c("FID_Remove",
                                                             "FID_improv",
                                                             "FID_persis",
                                                             "FID_dev"),
                                                  labels = c("Removal",
                                                             "Improvement",
                                                             "Persistence",
                                                             "Development"),
                                                  ordered = TRUE),
                                 target_date) %>%
                dplyr::group_by(date, outlook, target_date) %>%
                dplyr::summarise(.groups = "drop") %>%
                # sf::`st_agr<-`("constant") %>%
                # sf::st_make_valid() %>%
                # sf::st_intersection(
                #   get_oconus(rotate = FALSE) %>%
                #     sf::st_geometry()
                # ) %>%

                dplyr::transmute(date,
                                 scale = droughtlook_scale,
                                 outlook,
                                 target_date) %>%
                sf::st_make_valid() %>%
                sf::st_transform("OGC:CRS84") %>%
                sf::st_make_valid() %>%
                sf::st_cast("MULTIPOLYGON", warn = FALSE) %>%
                sf::write_sf(dsn = parquet_out,
                             layer_options = c("COMPRESSION=BROTLI",
                                               "GEOMETRY_ENCODING=GEOARROW",
                                               "WRITE_COVERING_BBOX=NO"),
                             driver = "Parquet")
              
              # sf::sf_use_s2(TRUE)
              unlink(file.path(tempdir(), tools::file_path_sans_ext(basename(x))), recursive = TRUE)
            }
            
            return(parquet_out)
            
          },
          .options = furrr::furrr_options(seed = TRUE)
          
        )
    })
    
    plan(sequential)
    
    
    ## plot the droughtlooks
    plan(future.callr::callr, 
         workers = parallel::detectCores())
    
    droughtlooks %<>%
      furrr::future_map_chr(
        \(x){
          outfile <- stringr::str_replace_all(x, "parquet", "png")
          
          if(!file.exists(outfile)){
            # sf::sf_use_s2(FALSE)
            droughtlook <-
              x %>%
              sf::read_sf() %>%
              sf::st_transform("ESRI:102003") %>%
              sf::st_make_valid() %>%
              st_set_agr("constant") %>%
              # sf::st_cast("POLYGON", warn = FALSE) %>%
              # sf::st_make_valid() %>%
              # dplyr::group_by(outlook, date, scale, target_date) %>%
              # dplyr::summarise(.groups = "drop") %>%
              # sf::st_cast("POLYGON", warn = FALSE) %>%
              sf::st_intersection(
                get_oconus(rotate = FALSE) %>%
                  rmapshaper::ms_simplify() %>%
                  sf::st_geometry() %>%
                  sf::st_transform("ESRI:102003") %>%
                  sf::st_make_valid()
              ) %>%
              sf::st_cast("MULTIPOLYGON", warn = FALSE) %>%
              st_set_agr("constant") %>%
              sf::st_cast("POLYGON", warn = FALSE) %>%
              sf::st_make_valid() %>%
              tigris::shift_geometry() %>%
              sf::st_make_valid() %>%
              dplyr::group_by(outlook, date, scale, target_date) %>%
              dplyr::summarise(.groups = "drop") %>%
              sf::st_cast("MULTIPOLYGON", warn = FALSE) %>%
              dplyr::mutate(outlook = factor(outlook,
                                             levels = c("Removal",
                                                        "Improvement",
                                                        "Persistence",
                                                        "Development"),
                                             labels = c("Removal",
                                                        "Improvement",
                                                        "Persistence",
                                                        "Development"),
                                             ordered = TRUE))# %>%
              # sf::st_intersection(               
              #   get_oconus() %>%
              #     sf::st_geometry()
              # ) %>%
              # sf::st_cast("MULTIPOLYGON", warn = FALSE)
            
            date <- droughtlook$date[[1]]
            
            p <-
              ggplot(get_oconus()) +
              geom_sf(fill = "gray80",
                      color = NA,
                      show.legend = FALSE) +
              geom_sf(aes(fill = outlook),
                      data = droughtlook,
                      color = "white",
                      size = 0.1,
                      show.legend = T) +
              geom_sf(data = get_oconus_states(),
                      color = "white",
                      alpha = 0,
                      show.legend = FALSE,
                      size = 0.2) +
              scale_fill_manual(values = c("#A4A056",
                                           "#D6C8AF",
                                           "#884F39",
                                           "#FED750"),
                                drop = FALSE,
                                name = paste0("US Drought Outlook\nthrough ",
                                              format(droughtlook$target_date[[1]], "%b %e, %Y") %>% 
                                                stringr::str_squish()),
                                guide = guide_legend(direction = "vertical",
                                                     title.position = "top") ) +
              usdm_layout(attribution = "The U.S. Drought Outlook is produced by the National Oceanic and Atmospheric\nAdministration's Climate Prediction Center based on data from the\nNational Drought Mitigation Center at the University of Nebraska-Lincoln.\nMap data courtesy of NDMC and CPC. Map courtesy of the Montana Climate Office.",
                          footnote = paste0("Data released ", format(lubridate::as_date(date), "%B %e, %Y")) %>% stringr::str_squish())
            
            gt <- ggplot_gtable(ggplot_build(p))
            gt$layout$clip[gt$layout$name == "panel"] <- "off"
            
            grid::grid.draw(gt) %>%
              ggsave(plot = .,
                     filename = outfile,
                     device = ragg::agg_png,
                     width = 10,
                     height = 5.14,
                     # height = 6.86,
                     bg = "white",
                     dpi = 600)
            
            # sf::sf_use_s2(TRUE)
          }
          
          return(outfile)
        },
        .options = furrr::furrr_options(seed = TRUE)
      )
    
    plan(sequential)
    
    invisible({
      list.files(file.path(out_dir, "png"),
                 full.names = TRUE,
                 pattern = "monthly") %>%
        sort() %>%
        dplyr::last() %>%
        file.copy(to = file.path(out_dir, "latest_monthly.png"),
                  overwrite = TRUE)
    })
    
    invisible({
      list.files(file.path(out_dir, "png"),
                 full.names = TRUE,
                 pattern = "seasonal") %>%
        sort() %>%
        dplyr::last() %>%
        file.copy(to = file.path(out_dir, "latest_seasonal.png"),
                  overwrite = TRUE)
    })
    
    return(droughtlooks)
    
  }
