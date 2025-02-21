#' @title Check_cutoff
#' @description Check the cutoff values in a data frame of curves. It identifies
#' control curves with cutoff times less than 3 hours and those that match
#' user-specified cutoff values. If any curves have cutoff times below 3 hours,
#'  a warning is issued.
#' @param curves_df Master data frame containing adjusted OD measurements, cut-off
#' values and Runs and Layout data.
#' @param inspect_values A numeric vector of user-specified cutoff values to
#' inspect. Default is NaN, which means only values < 3 hours are checked
#' @return A data frame containing distinct rows of curves that either have
#' cutoff times less than 3 hours or match the user-specified cutoff values.
#' @export
Check_cutoff = function(curves_df, inspect_value = NaN) {
	# Check values below 3
	# which give error in the Mark_artifacts function
	minimum_co_df = curves_df %>%
		dplyr::filter(Control == TRUE, cutoff_time <=2) %>%
		dplyr::select(RRPP, Strain, cutoff_time) %>%
		dplyr::distinct()

	# User, specified cutoff values
	specified_co_df = curves_df %>%
		dplyr::filter(Control == TRUE, cutoff_time <= inspect_value) %>%
		dplyr::select(RRPP, Strain, cutoff_time) %>%
		dplyr::distinct()

	# Show warning
	if (nrow(minimum_co_df) > 0) {
		warning("Some curves have cut-off times < 3 h, please adjust or remove to avoid downstream issues")
	}

	check_df = dplyr::bind_rows(minimum_co_df, specified_co_df) %>%
		dplyr::distinct() %>%
		dplyr::arrange(cutoff_time)

	# Return
	check_df
}
