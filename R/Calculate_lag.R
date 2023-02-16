Calculate_lag = function(GCs, offset_control = 0.02){
  
  # offset_control sets the change in OD required for a curve to not considered flat
  
  # Calculates OD_adj
  # the minimum OD_adj gives you the lagtime
  # the maximum OD_adj gives you the cutoff time (= time where stationary phase begins)
  
  cut_off_times = GCs %>% 
    dplyr::filter(Time < cutoff_time) %>% 
    dplyr::select(RRPPRCC, Time, cutoff_time) %>% 
    dplyr::mutate(Time = round(Time, 1))
  
  GCs_lag = GCs %>% 
    dplyr::mutate(Time = round(Time, 1)) %>%
    dplyr::group_by(RRPPRCC, Time) %>%
    dplyr::summarise(OD = median(OD)) %>% 
    dplyr::arrange(Time) %>%
    dplyr::group_by(RRPPRCC) %>% 
    dplyr::mutate(OD_adj = OD + OD[dplyr::n()] * offset_control * (Time[dplyr::n()]-Time)) %>% 
    dplyr::ungroup()
  
  # # Filter the minimum OD_adj on each curve
  # # # The corresponding time is the end of lag phase
  Lag_times = GCs_lag %>%
    dplyr::left_join(cut_off_times, by = c("RRPPRCC", "Time")) %>% 
    dplyr::group_by(RRPPRCC) %>%
    dplyr::filter(Time < cutoff_time) %>%
    dplyr::filter(OD_adj == min(OD_adj)) %>%
    dplyr::mutate(lag_time = round(mean(Time), digits = 1)) %>%
    dplyr::ungroup() %>%
    dplyr::select(RRPPRCC, lag_time)
  # # 
  # # Join raw OD values with time cutoff
  out_lags = dplyr::left_join(GCs, Lag_times, by = "RRPPRCC")
  out_lags
}