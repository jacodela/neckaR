#' @title Fit_splines_single
#' @description Fit two splines to a single growth curve with the
#' degrees of freedom specified by the user. Spline fits can be used to later
#' detect outliers based on the difference of the fits.
#' @param curves_df Master data frame containing OD measurements and
#' Runs and Layout data.
#' @param Time_col Variable in curves_df with time points of the curve
#' @param OD_col Variable in curves_df with OD measurements of the curve
#' @param degrees_freedom Vector of length two with the degrees of freedom
#' of each of the splines to be fitted.
#' @return PENDING
#' @export
# Function to fit spline and add results to original df
# Fits two splines based on the degrees of freedom of input
# Can be used to later compare fits
Fit_splines_single = function(curves_df, Time_col, OD_col, degrees_freedom){
	# Extract data
	x_val = dplyr::pull(curves_df, Time_col)
	y_val = dplyr::pull(curves_df, OD_col)
	# Fit splines
	spln_1 =  smooth.spline(x_val, y_val, df = degrees_freedom[1])
	spln_2 =  smooth.spline(x_val, y_val, df = degrees_freedom[2])
	#Extract fit and residuals and output single df
	out_tmp = broom::augment(spln_1, curves_df) %>%
		dplyr::rename("fitted_1" = ".fitted", "residual_1" = ".resid")
	out = broom::augment(spln_2, out_tmp) %>%
		dplyr::rename("fitted_2" = ".fitted", "residual_2" = ".resid") %>%
		dplyr::mutate(spline_diff = abs(fitted_1 - fitted_2))
	out
}

#' @title Fit_splines
#' @description Fit two splines to a series of bacterial growth curves with the
#' degrees of freedom specified by the user. Spline fits can be used to later
#' detect outliers based on the difference of the fits.
#' @param curves_df Master data frame containing OD measurements and
#' Runs and Layout data.
#' @param Time_col Variable in curves_df with time points of the curve
#' @param OD_col Variable in curves_df with OD measurements of the curve
#' @param degrees_freedom Vector of length two with the degrees of freedom
#' of each of the splines to be fitted.
#' @param group_var Variables in curves_df used to group curves.
#' @return PENDING
#' @export
Fit_splines = function(curves_df, Time_col = "Time", OD_col = "OD", degrees_freedom = c(4,10), group_var = "RRPPRCC") {
  splines_df = curves_df %>%
    dplyr::group_split(!!rlang::sym(group_var)) %>%
    purrr::map_df(function(df) Fit_splines_single(df, Time_col = Time_col, OD_col = OD_col,  degrees_freedom = degrees_freedom))

  splines_df
}

