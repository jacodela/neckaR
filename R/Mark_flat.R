#' @title Mark_flat_single
#' @description Determine whether a single growth curve is flat
#' @param curves_df Master data frame containing adjusted OD measurements and
#' Runs and Layout data.
#' @param OD_col Name of column in master data frame containing
#' OD measurements. Can be raw or adjusted OD.
#' @param OD_diff_cutoff Average difference between each of four break points in the
#' growth curve and and the first measurement. If this value is  below a user-defined
#' threshold, the curve will be flagged as flat. Smaller values make the marking
#' more strict.
#' @param last_OD Minimum OD value that a curve should reach in last measurement
#' so that the 'flat' label is removed. Smaller values make the marking more strict.
#' @return A logical value of whether a curve is considered flat or not.
#' @export
Mark_flat_single = function(curves_df, OD_col, OD_diff_cutoff, last_OD){

	# Select positions corresponding to
	# first, 0.25, 0.5, 0.75 and last measurements
	five_num = fivenum(1:nrow(curves_df)) %>%
		round()

	# Select third OD measurement
	# Third one minimizes chances of bubble peaks at the start
	init_measurement = (five_num[1] + 2)
	origin = unlist(curves_df[init_measurement, OD_col])
	last_measurement = unlist(curves_df[five_num[5], OD_col])

	## Select measurements to compare with
	## Distinct in case of repeats (very few measurements)
	## Not counting origin
	quartiles = five_num[-1] %>%
		unique()

	## Filter curves_df to quartile measurements
	points_df = curves_df %>%
		dplyr::slice(quartiles) %>%
		dplyr::select(Time, dplyr::all_of(OD_col)) %>%
		dplyr::mutate(Difference = !!rlang::sym(OD_col) - origin)

	mean_diff = mean(points_df$Difference)

	## Determine whether mean difference is above threshold.
	## If mean difference is below threshold but last measurement
	## greater than user-defined value, consider non-flat
	is_flat = dplyr::case_when(mean_diff > OD_diff_cutoff ~ FALSE,
														 (mean_diff <= OD_diff_cutoff & last_measurement > last_OD) == TRUE ~ FALSE,
														 (mean_diff <= OD_diff_cutoff & last_measurement < last_OD) == TRUE ~ TRUE)
	data.frame(flat = is_flat)

}

#' @title Mark_flat
#' @description Determine whether each curve in a data frame is flat.
#' @param curves_df Master data frame containing adjusted OD measurements and
#' Runs and Layout data.
#' @param OD_col Name of column in master data frame containing
#' OD measurements. Can be raw or adjusted OD.
#' @param OD_diff_cutoff Average difference between each of four break points in the
#' growth curve and and the first measurement. If this value is  below a user-defined
#' threshold, the curve will be flagged as flat. Smaller values make the marking
#' more strict.
#' @param group_var Name of variable to be used for grouping before checking if curve is flat.
#' @param last_OD Minimum OD value that a curve should reach in last measurement
#' so that the 'flat' label is removed. Smaller values make the marking more strict.
#' @return A data frame object with the result of the test of whether the curve
#' is flat added to the input master data frame.
#' @export
Mark_flat = function(curves_df, OD_col = "ODc01", OD_diff_cutoff = 0.2, group_var = "RRPPRCC", last_OD = 1) {
	curves_df %>%
		dplyr::group_by(!!rlang::sym(group_var)) %>%
		dplyr::do(Mark_flat_single(.,
															 OD_col = OD_col,
															 OD_diff_cutoff = OD_diff_cutoff,
															 last_OD = last_OD)) %>%
	left_join(curves_df, .)
}
