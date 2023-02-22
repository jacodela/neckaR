#'	@title Calculate_lag
#'
#'	@description Determine the time of transition from lag phase to
#'	exponential phase of the growth curve.
#'
#'	@param curves_df Master data frame containing adjusted OD measurements, cut-off
#'	values and	Runs and Layout data.
#'	@param offset_control Scaling factor used to account for small increases in
#'	 OD during stationary phase.
#'
#'	@return A data frame object that adds end of lag phase time to the input
#'	 master data frame.
#'	@examples
#'	  # PENDING
#'	@export
Calculate_lag = function(curves_df, offset_control = 0.02){

  # offset_control sets the change in OD required for a curve to not considered flat

  # Calculates OD_adj
  # the minimum OD_adj gives you the lagtime
  # the maximum OD_adj gives you the cutoff time (= time where stationary phase begins)

  cut_off_times = curves_df %>%
    dplyr::filter(Time < cutoff_time) %>%
    dplyr::select(RRPPRCC, Time, cutoff_time) %>%
    dplyr::mutate(Time = round(Time, 1))

  curves_df_lag = curves_df %>%
    dplyr::mutate(Time = round(Time, 1)) %>%
    dplyr::group_by(RRPPRCC, Time) %>%
    dplyr::summarise(OD = median(OD)) %>%
    dplyr::arrange(Time) %>%
    dplyr::group_by(RRPPRCC) %>%
    dplyr::mutate(OD_adj = OD + OD[dplyr::n()] * offset_control * (Time[dplyr::n()]-Time)) %>%
    dplyr::ungroup()

  # # Filter the minimum OD_adj on each curve
  # # # The corresponding time is the end of lag phase
  Lag_times = curves_df_lag %>%
    dplyr::left_join(cut_off_times, by = c("RRPPRCC", "Time")) %>%
    dplyr::group_by(RRPPRCC) %>%
    dplyr::filter(Time < cutoff_time) %>%
    dplyr::filter(OD_adj == min(OD_adj)) %>%
    dplyr::mutate(lag_time = round(mean(Time), digits = 1)) %>%
    dplyr::ungroup() %>%
    dplyr::select(RRPPRCC, lag_time)

  # Join raw OD values with time cutoff
  full_lags = dplyr::left_join(curves_df, Lag_times, by = "RRPPRCC")

  # Set lag time of controls within plate and strain equal to median
  control_lags = full_lags %>%
  	dplyr::filter(Control == TRUE) %>%
  	dplyr::group_by(RRPP, Strain) %>%
  	dplyr::mutate(lag_time = round(median(lag_time))) %>%
  	ungroup()

  non_control_lags = full_lags %>%
  	dplyr::filter(Control == FALSE)

  # Combine lag times of controls and non controls
  out_lags = dplyr::bind_rows(control_lags, non_control_lags) %>%
  	dplyr::arrange(RRPPRCC)
  out_lags
}
