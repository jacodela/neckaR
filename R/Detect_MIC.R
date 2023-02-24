#' @title Detect_MIC_single
#' @description Calculate minimum inhibitory concentration of a single bacterial
#' growth curve
#' @param curves_df Master data frame containing raw OD measurements and
#' Runs and Layout data.
#' @param threshold Area under the curve (AUC) below which a bacterial strain
#' is considered to be inhibited by a treatment.
#' @param concentration_var Variable in curves_df containing concentration data
#' @param group_var variables used to group curves
#' @return A data frame object with a variable stating whether the MIC of a given
#' treatment is below ("<"), equal to ("=") or greater than(">") the tested concentration
#' @export
Detect_MIC_single <- function(curves_df, threshold, concentration_var, group_var) {
  dt <- curves_df %>%
    dplyr::filter(normAUCmon <= threshold)

  min_conc = curves_df %>%
  	dplyr::pull(concentration_var) %>%
    min()

  max_conc = curves_df %>%
  	dplyr::pull(concentration_var) %>%
    max()

  if (nrow(dt) == nrow(curves_df)) {
    # all below threshold
    result <- tibble::data_frame(qualifier = "<", Conc = min_conc)
  } else if (nrow(dt) == 0) {
    # all above threshold
    result <- tibble::data_frame(qualifier = ">", Conc = max_conc)
  } else {
    # have an MIC!
    result <- tibble::data_frame(qualifier = "=", Conc = min_conc)
  }

  result
}

#' @title Detect_MIC
#' @description Calculate minimum inhibitory concentration of a series of bacterial
#' growth curves
#' @param curves_df Master data frame containing raw OD measurements and
#' Runs and Layout data.
#' @param threshold Area under the curve (AUC) below which a bacterial strain
#' is considered to be inhibited by a treatment.
#' @param concentration_var Variable in curves_df containing concentration data.
#' @param group_var Variables in curves_df used to group curves.
#' @param control_var Variable in curves_df stating whether a curve corresponds
#' to a control or a not.
#' @param AUC_var Variable in curves_df with AUC data.
#' @return A data frame object with a variable stating whether the MIC of a given
#' treatment is below ("<"), equal to ("=") or greater than(">") the tested concentration
#' @export
Detect_MIC = function(curves_df, threshold, concentration_var, group_var, control_var = "Control", AUC_var = "normAUC"){

  # Calculate median AUC per concentration and grouping variables
  median_grouping_vars = c(concentration_var, group_var)
  medianAUCs_A = curves_df %>%
    dplyr::filter(!!rlang::sym(control_var) == FALSE) %>%
    dplyr::group_by_at(median_grouping_vars) %>%
    dplyr::summarise(normAUC_med = median(!!rlang::sym(AUC_var))) %>%
    dplyr::ungroup()

  medianAUCs_B = medianAUCs_A %>%
    dplyr::group_by_at(group_var) %>%
    dplyr::arrange(concentration_var) %>%
    dplyr::mutate(normAUCmon = cumulmax(normAUC_med)) %>%
    dplyr::ungroup()

  # Calculate MIC per group
  MICs = medianAUCs_B %>%
  	dplyr::group_by_at(group_var) %>%
  	dplyr::do(Detect_MIC_single(., threshold = threshold, concentration_var = concentration_var)) %>%
  	dplyr::ungroup()

  # Return
  MICs
  # list(MICs, medianAUCs_A, medianAUCs_B)

}




