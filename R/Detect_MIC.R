#' @title Detect_MIC_single
#' @description Calculate minimum inhibitory concentration of a single bacterial
#' growth curve
#' @param curves_df Master data frame containing raw OD measurements and
#' Runs and Layout data.
#' @param threshold Area under the curve (AUC) below which a bacterial strain
#' is considered to be inhibited by a treatment.
#' @param concentration_var Variable in curves_df containing concentration data
#' @return A data frame object with a variable stating whether the MIC of a given
#' treatment is below ("<"), equal to ("=") or greater than(">") the tested concentration
#' @export
Detect_MIC_single <- function(curves_df, threshold = 0.75, concentration_var = Concentration) {
	less_equal_df = curves_df %>%
		dplyr::mutate(equal_min_con = {{concentration_var}} <= min({{concentration_var}})) %>%
		dplyr::filter(normAUCmon <= threshold) %>%
		dplyr::slice(1) %>%
		dplyr::ungroup() %>%
		dplyr::mutate(qual = dplyr::if_else(equal_min_con == TRUE, "<", "=")) %>%
		dplyr::select(qual, {{concentration_var}})

	greater_df = curves_df %>%
		dplyr::mutate(equal_max_con = Concentration >= max({{concentration_var}})) %>%
		dplyr::filter(normAUCmon > threshold & equal_max_con == TRUE) %>%
		dplyr::ungroup() %>%
		dplyr::mutate(qual = ">") %>%
		dplyr::select(qual, {{concentration_var}})

	dplyr::bind_rows(less_equal_df, greater_df)
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
Detect_MIC = function(curves_df, threshold, group_var, control_var = Control, AUC_var = normAUC, concentration_var = Concentration){

	# Calculate median AUC per concentration and grouping variables
	# Use `deparse(substitute(concentration_var)` to convert the name of
	# concentration_var to a string
	group_var_A = 	c(deparse(substitute(concentration_var)), group_var)
	medianAUCs_A = curves_df %>%
		dplyr::filter({{control_var}} == FALSE) %>%
		dplyr::group_by_at(group_var_A) %>%
		dplyr::summarise(normAUC_med = median({{AUC_var}})) %>%
		dplyr::ungroup()

	medianAUCs_B = medianAUCs_A %>%
		dplyr::group_by_at(group_var) %>%
		dplyr::arrange({{concentration_var}}) %>%
		dplyr::mutate(normAUCmon = cumulmax(normAUC_med)) %>%
		dplyr::ungroup()
	medianAUCs_B

	# Calculate MIC per group
	MICs = medianAUCs_B %>%
		dplyr::group_by_at(group_var) %>%
		dplyr::do(Detect_MIC_single(., threshold = threshold,
																concentration_var = {{concentration_var}})) %>%
		dplyr::ungroup()

	# Return
	MICs
}
