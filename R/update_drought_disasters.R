update_drought_disasters <-
  function(out_dir = "usdm-archive/disasters"){
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
    
    ## Disasters
    disaster_files <-
      httr::modify_url(url = "https://www.fsa.usda.gov",
                       path = c("resources",
                                "disaster-assistance-program",
                                "disaster-designation-information")) %>%
      xml2::read_html()  %>%
      xml2::xml_find_all(".//a") %>%
      {tibble::tibble(year = xml2::xml_text(.) %>%
                        as.integer(),
                      # title = xml2::xml_attr(.,"title"),
                      href = xml2::xml_attr(.,"href")#,
                      # `data-jcrpath` = xml2::xml_attr(.,"data-jcrpath")
      )} %>%
      dplyr::filter(!is.na(year),
                    stringr::str_detect(href, "/documents/"),
                    stringr::str_detect(href, "federalregister", negate = TRUE)) %>%
      dplyr::mutate(
        type = dplyr::case_when(
          stringr::str_detect(href, "_SEC_|-sec-|/disaster_cty") ~ "Secretarial",
          stringr::str_detect(href, "_PRES|-pres|/disaster_cty") ~ "Presidential",
          stringr::str_detect(href, "_CROP_CY|_Crop_CY|/crop-cy|disaster_map_") ~ "Secretarial Map",
          stringr::str_detect(href, "_DROUGHT_CY|_Drought_CY|/drought-cy|drought_sec") ~ "Secretarial Map (Drought)",
          stringr::str_detect(href, "_CROP_CoList|/crop-colist|fast_trk_prim") ~ "County List PDF",
          stringr::str_detect(href, "_DROUGHT_CoList|/drought-colist|drought_disaster_cty") ~ "County List PDF (Drought)"
        ),
        href = stringr::str_replace(href, "All", "ALL")) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        href = ifelse(stringr::str_detect(href, "\\.x|\\.p"),
                      href,
                      httr::modify_url(url = "https://www.fsa.usda.gov",
                                       path = href) %>%
                        xml2::read_html()  %>%
                        xml2::xml_find_all(".//a") %>%
                        {.[xml2::xml_has_attr(.,"download")]} %>%
                        xml2::xml_attr("href") %>%
                        httr::parse_url() %$%
                        path %>%
                        file.path("", .)
        )
      )
    
    
    ## Download all raw disaster information from
    ## https://www.fsa.usda.gov/resources/disaster-assistance-program/disaster-designation-information
    
    get_href <-
      function(x){
        raw_path <- 
          file.path(raw_dir, basename(x))
        
        if(!file.exists(raw_path))
          curl::curl_download(url = file.path("https://www.fsa.usda.gov", x),
                              destfile = 
                                raw_path)
        
        return(raw_path)
      }
    
    disaster_files %<>%
      dplyr::mutate(raw_path = get_href(href))
    
    ## Parse disaster files and produce Parquets
    secretarial <-
      disaster_files$raw_path[disaster_files$type == "Secretarial"] %>%
      magrittr::set_names(.,.) %>%
      purrr::map(\(x){
        readxl::read_excel(
          x,
          col_type = "text"
        )}) %>%
      dplyr::bind_rows() %>%
      dplyr::select(!dplyr::starts_with("...")) %>%
      dplyr::mutate(
        `CROP DISASTER YEAR` = ifelse(is.na(`CROP DISASTER YEAR`), `Crop Year`, `CROP DISASTER YEAR`),
        `CROP DISASTER YEAR` = ifelse(is.na(`CROP DISASTER YEAR`), `CALENDAR DISASTER YEAR`, `CROP DISASTER YEAR`),
        `Designation Code` = ifelse(is.na(`Designation Code`), Desig_Cd, `Designation Code`),
        `Designation Code` = ifelse(is.na(`Designation Code`), `Designation Code\n(1 = primary, 2 = contiguous)`, `Designation Code`),
        `Designation Number` = ifelse(is.na(`Designation Number`), `Desig. No.`, `Designation Number`),
        `Description of disaster` = ifelse(is.na(`Description of disaster`), `Description of Disaster`, `Description of disaster`),
        `Designation Code`= factor(`Designation Code`,
                                   levels = 1:2,
                                   labels = c("Primary",
                                              "Contiguous"),
                                   ordered = TRUE),
        `Begin Date` = dplyr::case_when(
          `Begin Date` == "11/19/1024" ~ lubridate::as_date("2024-11-19"),
          .default = lubridate::as_date(as.integer(`Begin Date`),
                                        origin = "1899-12-30")
        ),
        `End Date` = dplyr::case_when(
          `End Date` == "N/A" ~ lubridate::as_date(NA),
          .default = lubridate::as_date(as.integer(`End Date`),
                                        origin = "1899-12-30")
        ),
        `Approval date` = dplyr::case_when(
          `Approval date` == "0-28-20" ~ lubridate::as_date("2020-02-28"),
          .default = lubridate::as_date(as.integer(`Approval date`),
                                        origin = "1899-12-30")
        )
      ) %>%
      dplyr::select(!c(Desig_Cd, 
                       `Desig. No.`, 
                       `Designation Code\n(1 = primary, 2 = contiguous)`, 
                       `Description of Disaster`,
                       `Crop Year`,
                       `CALENDAR DISASTER YEAR`
      )) %>%
      dplyr::select(
        `CROP DISASTER YEAR`,
        COUNTY_FIPS = FIPS,
        State, 
        County,
        `Description of disaster`,
        `Designation Code`,
        `Designation Number`,
        `Approval date`,
        `Begin Date`,
        `End Date`,
        dplyr::everything()
      ) %>%
      tidyr::pivot_longer(!c(`CROP DISASTER YEAR`,
                             COUNTY_FIPS,
                             State, 
                             County,
                             `Description of disaster`,
                             `Designation Code`,
                             `Designation Number`,
                             `Approval date`,
                             `Begin Date`,
                             `End Date`),
                          names_to = "Disaster") %>%
      dplyr::filter(value == "1") %>%
      dplyr::select(!value) %>%
      dplyr::select(
        `CROP DISASTER YEAR`,
        `Designation Number`,
        Disaster,
        `Description of disaster`,
        `Approval date`,
        `Begin Date`,
        `End Date`,
        COUNTY_FIPS,
        State, 
        County,
        `Designation Code`
      ) %>%
      dplyr::arrange(dplyr::desc(`CROP DISASTER YEAR`), `Designation Number`, COUNTY_FIPS) %T>%
      arrow::write_parquet(sink = file.path(parquet_dir, "secretarial.parquet"),
                           version = "latest",
                           compression = "snappy",
                           use_dictionary = TRUE)
    
    
    presidential <-
      disaster_files$raw_path[disaster_files$type == "Presidential"] %>%
      magrittr::set_names(.,.) %>%
      purrr::map(\(x){
        readxl::read_excel(
          x,
          col_type = "text"
        )}) %>%
      dplyr::bind_rows() %>%
      dplyr::select(!dplyr::starts_with("...")) %>%
      dplyr::rename(`Designation Code` = `Designation Code\r\n(1 = primary, 2 = contiguous)`) %>%
      dplyr::mutate(
        `CROP DISASTER YEAR` = ifelse(is.na(`CROP DISASTER YEAR`), `DISASTER YEAR`, `CROP DISASTER YEAR`),
        `Designation Code`= factor(`Designation Code`,
                                   levels = 1:2,
                                   labels = c("Primary",
                                              "Contiguous"),
                                   ordered = TRUE),
        `End Date` = dplyr::case_when(
          `End Date` == "continuing" ~ lubridate::as_date(NA),
          .default = lubridate::as_date(as.integer(`End Date`),
                                        origin = "1899-12-30")
        )
      ) %>%
      dplyr::select(!c(`DISASTER YEAR`
      )) %>%
      dplyr::select(
        `CROP DISASTER YEAR`,
        COUNTY_FIPS = FIPS,
        State, 
        `County/Tribal Government`,
        `Description of disaster`,
        `Designation Code`,
        `Declaration Number`,
        `Amendment No.`,
        `Approval date`,
        `Begin Date`,
        `End Date`,
        dplyr::everything()
      ) %>%
      tidyr::pivot_longer(!c(`CROP DISASTER YEAR`,
                             COUNTY_FIPS,
                             State, 
                             `County/Tribal Government`,
                             `Description of disaster`,
                             `Designation Code`,
                             `Declaration Number`,
                             `Amendment No.`,
                             `Approval date`,
                             `Begin Date`,
                             `End Date`),
                          names_to = "Disaster") %>%
      dplyr::filter(value == "1") %>%
      dplyr::select(!value) %>%
      dplyr::select(
        `CROP DISASTER YEAR`,
        `Declaration Number`,
        `Amendment No.`,
        Disaster,
        `Description of disaster`,
        `Approval date`,
        `Begin Date`,
        `End Date`,
        COUNTY_FIPS,
        State, 
        `County/Tribal Government`,
        `Designation Code`
      ) %>%
      dplyr::arrange(dplyr::desc(`CROP DISASTER YEAR`),         
                     `Declaration Number`,
                     `Amendment No.`,
                     COUNTY_FIPS) %T>%
      arrow::write_parquet(sink = file.path(parquet_dir, "presidential.parquet"),
                           version = "latest",
                           compression = "snappy",
                           use_dictionary = TRUE)
    
    
    latest_secretarial <-
      disaster_files %>%
      dplyr::ungroup() %>%
      dplyr::filter(type == "Secretarial",
                    year == max(year)) %$%
      raw_path %>%
      unz("docProps/core.xml") %>%
      xml2::read_xml() %>%
      xml2::xml_find_all("dcterms:modified") %>%
      xml2::xml_text() %>%
      lubridate::as_date()
    
    disasters <-
      secretarial %>%
      dplyr::filter(`CROP DISASTER YEAR` == max(`CROP DISASTER YEAR`),
                    Disaster == "DROUGHT") %>%
      dplyr::select(COUNTY_FIPS, `Designation Code`) %>%
      dplyr::distinct() %>%
      dplyr::arrange(COUNTY_FIPS) %>%
      dplyr::group_by(COUNTY_FIPS) %>%
      dplyr::summarise(`Designation Code` = min(`Designation Code`)) %>%
      dplyr::left_join(get_oconus_counties()) %>%
      sf::st_as_sf()
    
    p <-
      ggplot(get_oconus()) + 
      geom_sf(data = get_oconus(),
              fill = "gray80",
              color = NA,
              show.legend = FALSE) +
      geom_sf(data = disasters,
              aes(fill = `Designation Code`),
              color = "white",
              size = 0.05,
              show.legend = T) +
      geom_sf(data = get_oconus_states(),
              color = "white",
              alpha = 0,
              show.legend = FALSE,
              size = 0.2) +
      scale_fill_manual(
        values = 
          c(
            "Primary" = "#DC0005",
            "Contiguous" = "#FD9A09"
          ),
        na.value = NA,
        drop = FALSE,
        na.translate = FALSE,
        name = paste0(max(secretarial$`CROP DISASTER YEAR`), " USDA Secretarial\nDisaster Designations\nfor Drought"),
        guide = guide_legend(direction = "vertical",
                             title.position = "top",
                             ncol = 1) ) +
      usdm_layout(attribution = "The Secretary of Agriculture is authorized to designate counties\nas disaster areas for emergency loan and assistance programs,\nsuch as Farm Service Agency (FSA) disaster assistance programs.\nMap data courtesy of the FSA. Map courtesy of the Montana Climate Office.",
                  footnote = paste0("Data updated ", format(lubridate::as_date(latest_secretarial), "%B %e, %Y")) %>% stringr::str_squish())
    
    gt <- ggplot_gtable(ggplot_build(p))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    
    grid::grid.draw(gt) %>%
      ggsave(plot = .,
             filename = file.path(png_dir, "drought_disasters.png"),
             device = ragg::agg_png,
             width = 10,
             height = 5.14,
             # height = 6.86,
             bg = "white",
             dpi = 600)
    
    return(file.path(png_dir, "drought_disasters.png"))
    
  }
