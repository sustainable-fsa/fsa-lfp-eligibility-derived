update_usdm <-
  function(as_of = lubridate::today()){
    
    plan(future.callr::callr, 
         workers = parallel::detectCores())
    
    out <-
      get_usdm_dates(as_of = as_of) %>%
      furrr::future_map_chr(get_usdm,
                            .options = furrr::furrr_options(seed = TRUE))
    
    plan(sequential)
    
    return(out)
    
  }
