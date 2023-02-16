Detect_spikes = function(CGs, ODs_offset = 0){
  
  #' Create data frame with the ODc0 values of t0 and lag time of each curve
  lag_ODs_df = CGs %>% 
    dplyr::group_by(RRPPRCC) %>% 
    dplyr::filter(lag_time == floor(Time) | Time < 1) %>% 
    dplyr::select(RRPPRCC, ODc0, Time) %>% 
    dplyr::mutate(tmp_OD = row_number(Time), 
                  tmp_OD = if_else(tmp_OD == 1, "ODc0_t0", "ODc0_lag")) %>%
    dplyr::ungroup()  %>%
    dplyr::select(-Time) %>% 
    tidyr::pivot_wider(values_from = ODc0, names_from = tmp_OD) 
  
  #' Join with original df to determine if a point is a potential spike:
  #' Spike if ODc0 is higher than in lag time. Optionally, an offset can be
  #' added to the lag OD value to modify how sensitive the detection is. 
  #' The higher the offset value, the less sensitive the detection of spikes
  Spikes_df = lag_ODs_df %>% 
    dplyr::left_join(CGs, ., by = "RRPPRCC") %>% 
    dplyr::mutate(Spike = dplyr::if_else((floor(Time) < lag_time) &
                                           (ODc0 > (ODc0_lag + ODs_offset)),
                                         "Yes", "No")) %>% 
    dplyr::select(-c(ODc0_t0, ODc0_lag))
  
  # Return
  Spikes_df
}