plot_usdm <-
  function(x,
           out_dir = file.path("usdm-archive", "usdm")){
    
    x %<>%
      lubridate::as_date() %>%
      format("%Y%m%d")
    
    outfile <- 
      file.path(out_dir, "png", paste0(x,".png"))
    
    if(file.exists(outfile))
      return(outfile)
    
    dir.create(dirname(outfile),
               recursive = TRUE,
               showWarnings = FALSE)
    
    usdm <-
      get_usdm(x,
               return_sf = TRUE,
               rotate = TRUE) %>%
      dplyr::mutate(usdm_class = 
                      forcats::fct_drop(usdm_class, only = "None") %>%
                      forcats::fct_recode("Abnormally Dry: D0" = "D0",
                                          "Moderate Drought: D1" = "D1",
                                          "Severe Drought: D2" = "D2",
                                          "Extreme Drought: D3" = "D3",
                                          "Exceptional Drought: D4" = "D4"))
    
    p <-
      ggplot(get_oconus()) +
      geom_sf(data = get_oconus(),
              fill = "gray80",
              color = NA,
              show.legend = FALSE) +
      geom_sf(aes(fill = usdm_class),
              data = usdm,
              color = "white",
              linewidth = NA,
              show.legend = T) +
      geom_sf(data = get_oconus_states() %>%
                rmapshaper::ms_innerlines() %>%
                sf::st_cast("MULTILINESTRING"),
              color = "white",
              # alpha = 0,
              show.legend = FALSE,
              linewidth = 0.2) +
      scale_fill_manual(
        values = c("#ffff00",
                   "#fcd37f",
                   "#ffaa00",
                   "#e60000",
                   "#730000"),
        drop = FALSE,
        name = paste0("US Drought Monitor\n",
                      format(lubridate::as_date(x), "%B %e, %Y") %>% 
                        stringr::str_squish()),
        guide = guide_legend(direction = "vertical",
                             title.position = "top",
                             override.aes = list(linewidth = 0.5)) 
      ) +
      usdm_layout()
    
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
    
    
    return(outfile)
  }

update_usdm_video <-
  function(as_of = lubridate::today(),
           out_dir = file.path("usdm-archive", "usdm")){
    plan(
      strategy = future.callr::callr, 
      workers = parallel::detectCores()
    )
    
    out <-
      get_usdm_dates(as_of = as_of) %>%
      furrr::future_map_chr(plot_usdm,
                            out_dir = out_dir,
                            .options = furrr::furrr_options(seed = TRUE))
    
    plan(sequential)
    
    invisible({
      list.files(file.path(out_dir, "png"),
                 full.names = TRUE,
                 pattern = "\\d") %>%
        sort() %>%
        dplyr::last() %>%
        file.copy(to = file.path(out_dir, "usdm.png"),
                  overwrite = TRUE)
    })
    
    if(
      !file.exists(file.path(out_dir, "usdm.mp4")) ||
      av::av_video_info(file.path(out_dir, "usdm.mp4"))$video$frames <
      length(list.files(file.path(out_dir, "png")))
    ){
      system2(
        command = "ffmpeg",
        args = paste0(
          " -r 15",
          " -pattern_type glob -i '", file.path(out_dir, "png","[0-9]*.png'"),
          " -s:v 3000x2058",
          " -c:v libx265",
          " -crf 28",
          " -preset fast",
          " -tag:v hvc1",
          " -pix_fmt yuv420p10le",
          " -an",
          " ", file.path(out_dir, "usdm.mp4"),
          " -y"),
        wait = TRUE
      )
      
      system2(
        command = "ffmpeg",
        args = paste0(
          "-i ", file.path(out_dir, "usdm.mp4"),
          " -c:v libvpx-vp9",
          " -crf 30",
          " -b:v 0",
          " -row-mt 1",
          " ", file.path(out_dir, "usdm.webm"),
          " -y"),
        wait = TRUE
      )
    }
    
    return(out)
  }
