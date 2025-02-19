#' @title Adjust_OD
#' @description Baseline-adjust raw OD measurements by subtracting
#' the median OD of the controls at time 0 in a given plate from of all other
#' measurements and determine the time of transition from exponential to
#' stationary phase of the growth curve.
#' @param curves_df Master data frame containing raw OD measurements and
#' Runs and Layout data.
#' @param control_factor Name of column in master data frame containing
#' treatment information.
#' @param control_level Label of the control treatment.
#' (e.g. "control", "DMSO", "water").
#' @param offset_control Scaling factor used to account for small increases in
#' OD during stationary phase.
#' @return A data frame object that adds base-line adjusted OD and cut-off times
#' to the input master data frame.
#' @export
Adjust_OD = function(curves_df, control_factor, control_level, offset_control){

  ## NOTE
  ## !!rlang::sym(var_name) converts a string to a variable name to be used in a dplyr verb

  ## scaling of OD values
  GCs_raw = curves_df %>%
    dplyr::mutate(RRPP = as.numeric(substr(RRPPRCC, 1, nchar(RRPPRCC)-3))) %>% ## add a column for run and plate: RRPP
    dplyr::filter(!is.na(OD)) %>% ## remove NAs
    dplyr::mutate(Control = (!!rlang::sym(control_factor) == control_level)) %>%  ## flag control wells
    dplyr::group_by(RRPP, Strain) %>% ## zero all curves with respect to the control condition for each plate of each run (see RRPP above) per strain
    dplyr::mutate(ODc0 = OD - median(OD[Control & Time < 0.5 ])) %>% ## Remove median OD of controls at time 0 from all OD measurements
    dplyr::ungroup()

  ## Control curves
  GCs_controls = GCs_raw %>%
    dplyr::filter(Control == TRUE) %>% ## Retain controls
    dplyr::mutate(Time = round(Time, 0)) %>% ## Round time to no decimal places
    dplyr::group_by(RRPP, Time, Strain) %>%
    dplyr::summarise(ODc0 = median(ODc0)) %>% ## Calculate median OC of controls on each plate of each run at each time point
    dplyr::ungroup() %>%
    dplyr::arrange(Time) %>%
    dplyr::group_by(RRPP, Strain) %>%
    dplyr::mutate(ODc0_adj = ODc0 + (ODc0[dplyr::n()] * offset_control * (Time[dplyr::n()]-Time))) %>% ## Adjust the ODc0 by a scaling factor
    dplyr::ungroup()
  ## ODc0_adj equals the the scaled ODc0 plus the maximum ODc0 of the plate/run
  ## (ODc0[dplyr::n()]) times a scaling factor (offset_control) times the remaining
  ## time (Time[dplyr::n()]-Time)
  ## The scaling factor accounts for a small increase in OD during the stationary phase

  ## Determine the cutoff time of each plate of each run
  ## Value is the maximum adjusted OD of control of the plate
  time_cutoffs = GCs_controls %>%
    dplyr::group_by(RRPP) %>%
    dplyr::filter(ODc0_adj == max(ODc0_adj)) %>%
    dplyr::group_by(RRPP, Strain) %>%
    dplyr::mutate(cutoff_time = round(mean(Time), digits = 0)) %>%
    dplyr::select(RRPP, cutoff_time, Strain) %>%
    dplyr::ungroup() %>%
    unique()

  ## Join raw OD values with time cutoff
  GCs = dplyr::left_join(GCs_raw, time_cutoffs, by = dplyr::join_by("RRPP", "Strain"))
  GCs

}
