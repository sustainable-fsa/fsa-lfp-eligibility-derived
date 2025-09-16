update_usdm_change <- 
  function(){
    
    format_date <-
      function(x){
        lubridate::stamp("Jan 4, 2000", orders = "%b %d, %Y")(x) %>%
          stringr::str_replace(" 0", " ")
      }
    
    
    change <- 
      sf::st_union(
        get_usdm("2024-11-26") %>%
          tidyr::pivot_wider(names_from = date,
                             values_from = usdm_class),
        get_usdm("2024-01-02") %>%
          tidyr::pivot_wider(names_from = date,
                             values_from = usdm_class),
        is_coverage = TRUE
      ) %>%
      dplyr::mutate(dplyr::across(c(X2024.11.26,  X2024.01.02), as.integer)) %>%
      dplyr::transmute(usdm_class = 
                         factor(X2024.11.26 - X2024.01.02,
                                levels = -5:5,
                                labels = c(
                                  paste0(5:1, " Class Improved"),
                                  "No Change",
                                  paste0(1:5, " Class Degraded")
                                ),
                                ordered = TRUE))
    
    date <- lubridate::as_date(names(tifs))
    
    outfile <- file.path("png", "latest-change.png")
    
    oconus <- 
      get_states()
    
    p <-
      ggplot() + 
      geom_sf(data = dplyr::summarise(oconus),
              fill = "gray80",
              color = NA,
              show.legend = FALSE) +
      geom_sf(data = change,
              aes(fill = usdm_class),
              color = "white",
              size = 0.1,
              show.legend = T) +
      geom_sf(data = oconus,
              color = "white",
              alpha = 0,
              show.legend = FALSE,
              size = 0.2) +
      scale_fill_manual(values = rev(cols4all::c4a("tableau.classic_orange_blue", n = 11)),
                        drop = FALSE,
                        name = "US Drought Monitor\nClass Change\nStart of Calendar Year",
                        guide = guide_legend(direction = "vertical",
                                             title.position = "top",
                                             ncol = 2) ) +
      usdm_layout(footnote = paste0("Data released ", format_date(date[[2]]))) +
      theme(legend.text = element_text(size = 10),
            legend.key.height = unit(0.17, "inches"),
            legend.key.width = unit(0.17, "inches"),
            legend.position.inside = c(0.15,0.9))
    
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
    
    # tifs <- 
    #   list.files("tif",
    #              full.names = TRUE,
    #              pattern = ".tif$") %>%
    #   terra::rast()
    # 
    # usdm_history <-
    #   list(
    #     `D0–D4` = mean(tifs > 0),
    #     `D1–D4` = mean(tifs > 1),
    #     `D2–D4` = mean(tifs > 2),
    #     `D3–D4` = mean(tifs > 3),
    #     `D4` = mean(tifs > 4)
    #   )
    # 
    # plot_history <- 
    #   function(x, y){
    #     outfile <- file.path("png", paste0("latest-history-", y, ".png"))
    #     
    #     p <-
    #       ggplot() + 
    #       geom_sf(data = dplyr::summarise(oconus),
    #               fill = "gray80",
    #               color = NA,
    #               show.legend = FALSE) +
    #       geom_raster(data = as.data.frame(x, xy = TRUE),
    #                   mapping = aes(x = x,
    #                                 y = y,
    #                                 fill = mean * 100)) +
    #       
    #       geom_sf(data = oconus,
    #               color = "white",
    #               alpha = 0,
    #               show.legend = FALSE) +
    #       cols4all::scale_fill_binned_c4a_seq(palette = "brewer.reds",
    #                                           breaks = seq(0,100,10),
    #                                           limits = c(0,100),
    #                                           # drop = FALSE,
    #                                           name = paste0("US Drought Monitor\n% of Weeks in Class ",y,"\n",format_date(lubridate::as_date(min(names(tifs))))," – ",
    #                                                         format_date(lubridate::as_date(max(names(tifs))))),
    #       ) +
    #       guides(
    #         fill = 
    #           guide_colorsteps(direction = "horizontal",
    #                            title.position = "top",
    #                            ncol = 2,
    #                            show.limits = TRUE)
    #       ) +
    #       usdm_layout(footnote = paste0("Data released ", format_date(date[[2]]))) +
    #       theme(legend.text = element_text(size = 12),
    #             legend.text.position = "bottom",
    #             legend.key.height = unit(0.3, "inches"),
    #             legend.key.width = unit(0.6, "inches"),
    #             legend.position.inside = c(0.15,0.9),
    #             legend.title = element_text(size = 16, 
    #                                         face = "bold", 
    #                                         hjust = 1)
    #       )
    #     
    #     
    #     gt <- ggplot_gtable(ggplot_build(p))
    #     gt$layout$clip[gt$layout$name == "panel"] <- "off"
    #     
    #     grid::grid.draw(gt) %>%
    #       ggsave(plot = .,
    #              filename = outfile,
    #              device = ragg::agg_png,
    #              width = 10,
    #              height = 5.14,
    #              # height = 6.86,
    #              bg = "white",
    #              dpi = 600)
    #     
    #     return(outfile)
    #   }
    # 
    # usdm_history %>%
    #   purrr::iwalk(~plot_history(x = terra::trim(.x), y = .y))
    
  }
