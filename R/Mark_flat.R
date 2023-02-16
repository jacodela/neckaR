# Function to determine whether a growth curve is flat
Mark_flat_single = function(df, OD_col, OD_diff_cutoff){
  
  # Select positions corresponding to 
  # first, 0.25, 0.5, 0.75 and last measurements
  five_num = fivenum(1:nrow(df)) %>% 
    round()
  
  # Select second OD measurement
  # Second one minimizes chances of bubble peaks at the start
  init_measurement = (five_num[1] + 1)
  origin = unlist(df[init_measurement, OD_col])
  
  # Select measurements to compare with
  # Distinct in case of repeats (very few measurements)
  # Not counting origin
  quartiles = five_num[-1] %>% 
    unique()
  
  # Filter df to quartile measurements
  points_df = df %>%
    dplyr::slice(quartiles) %>%
    dplyr::select(Time, all_of(OD_col)) %>%
    dplyr::mutate(Difference = !!rlang::sym(OD_col) - origin)
  
  mean_diff = mean(points_df$Difference)
  
  is_flat = dplyr::case_when(mean_diff <= 0 ~ "decay",
                             dplyr::between(mean_diff, 0, OD_diff_cutoff) ~ "flat", 
                             mean_diff > OD_diff_cutoff ~ "not-flat")
  is_flat
  
}

# The above runs in a single curve
# Run in the combined data frame with all runs in all plates
Mark_flat = function(df, OD_col = "ODc01", OD_diff_cutoff = 0.2, group_var = "RRPPRCC") {
  df %>% 
    dplyr::group_split(!!rlang::sym(group_var)) %>% 
    purrr::map_df(function(x) mutate(x, is_flat = Mark_flat_single(x, OD_col = OD_col, OD_diff_cutoff = OD_diff_cutoff)))
}
