#' @title Calculate_AUC_single
#' @description Calculate area under the curve (AUC) of a single growth curve
#' using the trapezoidal method. The curve is broken down into a series of
#' rectangles of height equal to the mean OD of two adjacent measurements
#' and width equal to the time difference between such measurements.
#' The sum of the area of the rectangles is the AUC.
#' @param OD Vector with optical density measurements over time
#' @param Time Time values corresponding to the OD measurements
#' @return Numeric value of AUC
#' @export
Calculate_AUC_single <- function(OD, Time) {
  ## Number of measurements
  N <- length(OD)

  ## AUC cannot be calculated with < 2 measurements
  if (N<2) return(NA)

  ## Vectors of indices of adjacent measurements
  s1 <- 1:(N-1)
  s2 <- s1 + 1

  ## Calculate vectors of heights and widths
  ## Return sum of rectangles as AUC
  sum( (OD[s1] + OD[s2])/2 * (Time[s2]-Time[s1]) )
}

#' @title Calculate_AUC
#' @description Calculate normalized area under the curve (AUC) of a series of
#' growth curves after re-calculating optical density (OD) values by either
#' shifting or shifting and readjusting.
#' Before the AUC is calculated the OD values must be adjusted because
#' even though the median starting ODs of controls are set to 0, values of
#' individual wells may deviate from this. The `Calculate_AUC()` function uses
#' two methods:
#'
#' The first method assumes that curves are shifted by a constant across all points,
#' therefore the minimum OD of the curve is subtracted from each point. This causes
#' the minimum value to become zero. The second method assumes that the magnitude of
#' the shift of earlier time points is larger than that of later points, therefore
#' all values are first shifted by the minimum OD of the curve and then re-scaled so
#' that OD values that already had a value of 1 remain as such after the correction.
#' Using both OD values are used to calculate AUCs, which are then normalized to the
#' median AUC of control wells of a given plate and run. Finally, the AUC closest to
#' 1 is selected.
#' @param curves_df Data frame containing adjusted OD measurements after marking and
#' filtering abnormal growth curves
#' @return A data frame object that contains, for each curve, the normalized AUC
#' value selected, as well as raw and re-calculated OD values, detected abnormalities,
#' run, plate, replicate and other curve data.
#' @export
Calculate_AUC = function(curves_df){

  ## Recalculate ODc01 after cut-off
  blanked_df = curves_df %>%
  	dplyr::filter(Time < (cutoff_time + 0.5)) %>%
    dplyr::group_by(RRPP, Strain) %>%
    dplyr::mutate(ODc01 = ODc0 / median(ODc0[Time == max(Time) & Control])) %>%
    dplyr::ungroup()

  ## Shift and scale OD measurement
  ## Before calculating AUC, the OD is recalculated by either shifting or
  ## re-scaling each curve individually.
  ##
  ## To calculate shifted OD, the minimum OD is substracted from all time points
  ## of the growth curve so that the minimum is zero
  ##
  ## To re-scale OD,  a constant shift is substracted as above. Then the
  ## curve is scaled to a [0 - 1] range such that a time point with an uncorrected
  ## OD of 1 also had an OD of 1 after correction.
  shifted_scaled_df = blanked_df %>%
    dplyr::group_by(RRPPRCC, Strain) %>%
    dplyr::mutate(ODc01b_shifted = ODc01 - min(ODc01),
    							ODc01b_scaled = (ODc01 - min(ODc01)) / abs(1 - min(ODc01))) %>%
    dplyr::ungroup()

  ## Calculate AUC
  ##
  ## To calculate the AUC the shifted and re-scaled OD values are used and an
  ## AUC is calculated for each.
  ##
  ## Next, the value is normalized to the control wells of a given plate and run
  AUCs_df = shifted_scaled_df %>%
    dplyr::group_by(RRPPRCC, RRPP, Control, Strain) %>%
    dplyr::summarise(AUC_scaled = Calculate_AUC_single(ODc01b_scaled, Time),
                     AUC_shifted = Calculate_AUC_single(ODc01b_shifted, Time)) %>%
    dplyr::group_by(RRPP, Strain) %>%
    dplyr::mutate(normAUC_scaled = AUC_scaled / median(AUC_scaled[Control]),
                  normAUC_shifted = AUC_shifted / median(AUC_shifted[Control])) %>%
    dplyr::ungroup()

  ## Remove time and OD data to retain only conditions df
  info_df = curves_df %>%
    dplyr::select(!dplyr::matches(c("OD", "Time"))) %>%
    dplyr::distinct()

  ## The final normalized AUC selected is the one that is closest to 1 from
  ## the two areas calculated with the shifted and the re-scaled AUC
  ## Add conditions info
  Norm_AUCs_df = AUCs_df %>%
    dplyr::left_join(info_df, by = dplyr::join_by(RRPPRCC, RRPP, Control, Strain)) %>%
    dplyr::mutate(normAUC = dplyr::if_else(abs(normAUC_scaled - 1) < abs(normAUC_shifted - 1),
                                    normAUC_scaled, normAUC_shifted),
                  PP = substr(RRPP, nchar(RRPP) - 1, nchar(RRPP))) %>%
    dplyr::group_by(RRPPRCC, Strain) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
  	dplyr::relocate(RRPPRCC, normAUC)

  ## Return
  Norm_AUCs_df


}
