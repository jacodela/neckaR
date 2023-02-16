# Function to calculate the MIC
Detect_MIC_single <- function(df, threshold, concentration_var, grouping_vars) {
  dt <- df %>% 
    dplyr::filter(normAUCmon <= threshold)
  
  min_conc = df %>% 
    pull(concentration_var) %>% 
    min()
  
  max_conc = df %>% 
    pull(concentration_var) %>% 
    max()
  
  if (nrow(dt) == nrow(df)) {
    # all below threshold
    result <- data_frame(qualifier = "<", Conc = min_conc)
  } else if (nrow(dt) == 0) {
    # all above threshold
    result <- data_frame(qualifier = ">", Conc = max_conc)
  } else {
    # have an MIC!
    result <- data_frame(qualifier = "=", Conc = min_conc)
  }
  
  result
}


# Calculate
Detect_MIC = function(AUCs, threshold, concentration_var, grouping_vars, control_var = "Control", AUC_var = "normAUC"){
  
  
  # Calculate median AUC per concentration and grouping variables
  median_grouping_vars = c(concentration_var, grouping_vars)
  medianAUCs_A = AUCs %>% 
    dplyr::filter(!!rlang::sym(control_var) == FALSE) %>% 
    dplyr::group_by_at(median_grouping_vars) %>%
    dplyr::summarise(normAUC_med = median(!!rlang::sym(AUC_var))) %>% 
    dplyr::ungroup()
  
  medianAUCs_B = medianAUCs_A %>% 
    dplyr::group_by_at(grouping_vars) %>% 
    dplyr::arrange(concentration_var) %>%
    dplyr::mutate(normAUCmon = cumulmax(normAUC_med)) %>% 
    dplyr::ungroup()
  
  # Calculate MIC per group
  MICs = medianAUCs_B %>% 
    group_by_at(grouping_vars) %>% 
    do(Detect_MIC_single(., threshold = threshold, concentration_var = concentration_var)) %>% 
    ungroup()
  
  # Return
  MICs
  # list(MICs, medianAUCs_A, medianAUCs_B)
  
}




