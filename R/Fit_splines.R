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

# The above runs in a single curve
# Run in the combined data frame with all runs in all plates
Fit_splines = function(curves_df, Time_col = "Time", OD_col = "OD", degrees_freedom = c(4,10), group_var = "RRPPRCC") {
  splines_df = curves_df %>%
    dplyr::group_split(!!rlang::sym(group_var)) %>%
    purrr::map_df(function(df) Fit_splines_single(df, Time_col = Time_col, OD_col = OD_col,  degrees_freedom = degrees_freedom))

  splines_df
}

