get_oconus_counties <-
  function(outfile = "usdm-archive/oconus/oconus_counties.parquet",
           rotate = TRUE){
    
    dir.create(dirname(outfile),
               recursive = TRUE,
               showWarnings = FALSE)
    
    if (!file.exists(outfile)) {
      
    dl_3dhp <- 
      tempfile(fileext = ".zip")
      
      download.file("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/3DHP/Annual/GPKG/3dhp_all_GPKG_FY25_CONUS_20250313/3dhp_all_CONUS_20250313_GPKG.zip",
                    dl_3dhp)
      
      # tigris::counties() %>%
      #   dplyr::left_join(
      #     tigris::counties(cb = TRUE) %>%
      #       sf::st_drop_geometry()
      #   ) %>%
      tigris::counties(cb = TRUE) %>%
        dplyr::filter(!(STATE_NAME %in% c(
          # "Puerto Rico",
          "American Samoa",
          # "Alaska",
          # "Hawaii",
          "Guam",
          "Commonwealth of the Northern Mariana Islands",
          "United States Virgin Islands"
        ))) %>%
        sf::st_make_valid() %>%
        tidyr::unite(col = "COUNTY_FIPS", STATEFP, COUNTYFP, sep = "", remove = FALSE) %>%
        # rmapshaper::ms_simplify(keep_shapes = TRUE) %>%
        sf::st_transform("OGC:CRS84") %>%
        dplyr::transmute(COUNTY_FIPS,
                         NAME, 
                         STATE_NAME,
                         STATEFP,
                         COUNTYFP,
                         AREA = sf::st_area(geometry)
        ) %>%
        sf::st_cast("MULTIPOLYGON", warn = FALSE) %T>%
        sf::write_sf(outfile,
                     layer_options = c("COMPRESSION=SNAPPY",
                                       "GEOMETRY_ENCODING=GEOARROW",
                                       "WRITE_COVERING_BBOX=NO"),
                     driver = "Parquet") %>%
        # rmapshaper::ms_simplify(keep_shapes = TRUE) %>%
        sf::st_cast("MULTIPOLYGON", warn = FALSE) %>%
        sf::st_cast("POLYGON", warn = FALSE, do_split = TRUE) %>%
        sf::st_make_valid() %>%
        tigris::shift_geometry() %>%
        dplyr::group_by(dplyr::across(dplyr::everything())) %>%
        dplyr::summarise(.groups = "drop",
                         is_coverage = TRUE) %>%
        sf::st_cast("MULTIPOLYGON", warn = FALSE) %T>%
        sf::write_sf(stringr::str_replace(outfile, ".parquet", "_rotated_simple.parquet"),
                     layer_options = c("COMPRESSION=SNAPPY",
                                       "GEOMETRY_ENCODING=GEOARROW",
                                       "WRITE_COVERING_BBOX=NO"),
                     driver = "Parquet")
      
    }
    
    if(rotate)
      outfile %<>%
      stringr::str_replace(".parquet", "_rotated_simple.parquet")
    
    return(sf::read_sf(outfile))
  }

get_oconus_states <-
  function(outfile = "usdm-archive/oconus/oconus_states.parquet",
           rotate = TRUE){
    
    dir.create(dirname(outfile),
               recursive = TRUE,
               showWarnings = FALSE)
    
    if (!file.exists(outfile)) {
      get_oconus_counties(rotate = FALSE) %>%
        dplyr::group_by(STATE_NAME, STATEFP) %>%
        dplyr::summarise(.groups = "drop",
                         is_coverage = TRUE) %>%
        sf::st_cast("MULTIPOLYGON", warn = FALSE) %>%
        sf::write_sf(outfile,
                     layer_options = c("COMPRESSION=SNAPPY",
                                       "GEOMETRY_ENCODING=GEOARROW",
                                       "WRITE_COVERING_BBOX=NO"),
                     driver = "Parquet")
      
      get_oconus_counties() %>%
        dplyr::group_by(STATE_NAME, STATEFP) %>%
        dplyr::summarise(.groups = "drop",
                         is_coverage = TRUE) %>%
        sf::st_cast("MULTIPOLYGON", warn = FALSE) %>%
        sf::write_sf(stringr::str_replace(outfile, ".parquet", "_rotated_simple.parquet") ,
                     layer_options = c("COMPRESSION=SNAPPY",
                                       "GEOMETRY_ENCODING=GEOARROW",
                                       "WRITE_COVERING_BBOX=NO"),
                     driver = "Parquet")
    }
    
    if(rotate)
      outfile %<>%
      stringr::str_replace(".parquet", "_rotated_simple.parquet")
    
    return(sf::read_sf(outfile))
  }

get_oconus <-
  function(outfile = "usdm-archive/oconus/oconus.parquet",
           rotate = TRUE){
    
    dir.create(dirname(outfile),
               recursive = TRUE,
               showWarnings = FALSE)
    
    if (!file.exists(outfile)) {
      get_oconus_states(rotate = FALSE) %>%
        dplyr::summarise(.groups = "drop",
                         is_coverage = TRUE) %>%
        sf::st_cast("MULTIPOLYGON", warn = FALSE) %>%
        sf::write_sf(outfile,
                     layer_options = c("COMPRESSION=SNAPPY",
                                       "GEOMETRY_ENCODING=GEOARROW",
                                       "WRITE_COVERING_BBOX=NO"),
                     driver = "Parquet")
      
      get_oconus_states() %>%
        dplyr::summarise(.groups = "drop",
                         is_coverage = TRUE) %>%
        sf::st_cast("MULTIPOLYGON", warn = FALSE) %>%
        sf::write_sf(stringr::str_replace(outfile, ".parquet", "_rotated_simple.parquet") ,
                     layer_options = c("COMPRESSION=SNAPPY",
                                       "GEOMETRY_ENCODING=GEOARROW",
                                       "WRITE_COVERING_BBOX=NO"),
                     driver = "Parquet")
    }
    
    if(rotate)
      outfile %<>%
      stringr::str_replace(".parquet", "_rotated_simple.parquet")
    
    return(sf::read_sf(outfile))
  }

get_oconus_counties()
get_oconus_states()
get_oconus()

