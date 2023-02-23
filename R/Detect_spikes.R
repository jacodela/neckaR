#'	@title Detect_spikes
#'
#'	@description Identify whether an OD measurement is an spike, that is,
#'		an unexpected increase that is seen only once in the early time points.
#'		Here, a potential spiked is called when ODc0 is higher than the OD of the
#'		point corresponding to the end of lag phase. Optionally, an offset can be
#'		added to the lag OD value to modify how sensitive the detection is.
#'		The higher the offset value, the less sensitive the detection of spikes
#'
#'	@param curves_df Master data frame containing adjusted OD measurements, cut-off
#'		values and Runs and Layout data. Note that the end of lag phase time should be included.
#'	@param offset_control Value added to the OD of end of lag phase to adjust
#'		how sensitive the spike detection is.
#'
#'	@return A data frame object that a variable marking whether a point is a potential
#'		spike
#'
#'	@seealso [Mark_artefacts()] for other method of spike detection.
#'
#'	@export
Detect_spikes = function(curves_df, offset_control = 0){

  ## Create data frame with the ODc0 values of t0 and lag time of each curve
  lag_ODs_df = curves_df %>%
    dplyr::group_by(RRPPRCC) %>%
    dplyr::filter(lag_time == floor(Time) | Time < 1) %>%
    dplyr::select(RRPPRCC, ODc0, Time) %>%
    dplyr::mutate(tmp_OD = dplyr::row_number(Time),
                  tmp_OD = dplyr::if_else(tmp_OD == 1, "ODc0_t0", "ODc0_lag")) %>%
    dplyr::ungroup()  %>%
    dplyr::select(-Time) %>%
    tidyr::pivot_wider(values_from = ODc0, names_from = tmp_OD)

  ## Join with original df to determine if a point is a potential spike:
  ## Spike if ODc0 is higher than in lag time. Optionally, an offset can be
  ## added to the lag OD value to modify how sensitive the detection is.
  ## The higher the offset value, the less sensitive the detection of spikes
  Spikes_df = lag_ODs_df %>%
    dplyr::left_join(curves_df, ., by = "RRPPRCC") %>%
    dplyr::mutate(Spike = dplyr::if_else((floor(Time) < lag_time) &
                                           (ODc0 > (ODc0_lag + offset_control)),
                                         "Yes", "No")) %>%
    dplyr::select(-c(ODc0_t0, ODc0_lag))

  # Return
  Spikes_df
}
