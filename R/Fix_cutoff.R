# Function to replace cut-off values
Fix_cutoff = function(df, RRPPs_cutoff, fix_lag = FALSE){
  
  # Create out df from input df
  df_new = df
  
  # Iterate over each element of list of RRPP/New time pairs
  for(i in 1:length((RRPPs_cutoff))){
    RRPP_index = RRPPs_cutoff[[i]][1]
    new_time =  RRPPs_cutoff[[i]][2]
    
    # Replace values
    df_new = df_new %>% 
      dplyr::mutate(cutoff_time = if_else(RRPP == RRPP_index, new_time, cutoff_time))
  }
  # Return
  df_new
}