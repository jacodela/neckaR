#'	@title Fix_cutoff
#'
#'	@description Manually replace end of lag or exponential phase cut-off values
#'  	after automatic method.
#'
#'	@param curves_df Master data frame containing adjusted OD measurements, cut-off
#'		values and Runs and Layout data. Note that the end of lag phase time should be included.
#'	@param offset_control Value added to the OD of end of lag phase to adjust
#'		how sensitive the spike detection is.
#'
#'	@return A data frame object that a variable marking whether a point is a potential
#'		spike
#'	@seealso [Adjust_OD()] and [Calculate_lag()] for determination of end of
#'		lag and exponential phases
#'   @export
Fix_cutoff = function(df, RRPPs_cutoff, fix_lag = FALSE){

  # Create out df from input df
  df_new = df

  # Iterate over each element of list of RRPP/New time pairs
  for(i in 1:length((RRPPs_cutoff))){
    RRPP_index = RRPPs_cutoff[[i]][1]
    new_time =  RRPPs_cutoff[[i]][2]

    # Replace values
    df_new = df_new %>%
      dplyr::mutate(cutoff_time = dplyr::if_else(RRPP == RRPP_index, new_time, cutoff_time))
  }
  # Return
  df_new
}
