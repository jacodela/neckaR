#' Function that adds extra columns in the master data frame prior further QC
Adjust_OD = function(curves_df, control_factor, control_level, offset_control){
  
  #' NOTE
  #' !!rlang::sym(var_name) converts a string to a variable name to be used in a dplyr verb
  
  #' scaling of OD values
  GCs_raw = curves_df %>%
    dplyr::mutate(RRPP = as.numeric(substr(RRPPRCC, 1, nchar(RRPPRCC)-3))) %>% #' add a column for run and plate: RRPP
    dplyr::filter(!is.na(OD)) %>% #' remove NAs
    dplyr::mutate(Control = (!!rlang::sym(control_factor) == control_level)) %>%  #' flag control wells
    dplyr::group_by(RRPP) %>% #' zero all curves with respect to the control condition for each plate of each run (see RRPP above)
    dplyr::mutate(ODc0 = OD - median(OD[Control & Time < 0.5 ])) %>% #' Remove median OD of controls at time 0 from all OD measurements
    dplyr::ungroup()
  
  #' Control curves
  GCs_controls = GCs_raw %>% 
    dplyr::filter(Control == TRUE) %>% #' Retain controls
    dplyr::mutate(Time = round(Time, 0)) %>% #' Round time to no decimal places
    dplyr::group_by(RRPP, Time) %>%
    dplyr::summarise(ODc0 = median(ODc0)) %>% #' Calculate median OC of controls on each plate of each run at each time point
    dplyr::ungroup() %>% 
    dplyr::arrange(Time) %>%
    dplyr::group_by(RRPP) %>% 
    dplyr::mutate(ODc0_adj = ODc0 + (ODc0[dplyr::n()] * offset_control * (Time[dplyr::n()]-Time))) %>% #' Adjust the ODc0 by a scaling factor 
    dplyr::ungroup()
  #' ODc0_adj equals the the scaled ODc0 plus the maximum ODc0 of the plate/run
  #' (ODc0[dplyr::n()]) times a scaling factor (offset_control) times the remaining
  #' time (Time[dplyr::n()]-Time)
  #' The scaling factor accounts for a small increase in OD during the stationary phase
  
  #' Determine the cutoff time of each plate of each run
  #' Value is the maximum adjusted OD of control of the plate
  time_cutoffs = GCs_controls %>%
    dplyr::group_by(RRPP) %>% 
    dplyr::filter(ODc0_adj == max(ODc0_adj)) %>%
    dplyr::group_by(RRPP) %>% 
    dplyr::mutate(cutoff_time = round(mean(Time), digits = 0)) %>% 
    dplyr::select(RRPP, cutoff_time) %>% 
    dplyr::ungroup() %>% 
    unique()
  
  #' Join raw OD values with time cutoff
  GCs = dplyr::left_join(GCs_raw, time_cutoffs, by = "RRPP")
  GCs
  
}