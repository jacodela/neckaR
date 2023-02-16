# Function to detect outliers based on residuals of spline fit
Detect_outliers_single = function(dat_frame, gesd_alpha, gesd_max_anoms, iqr_alpha, iqr_max_anoms){
  # Detect outliers of residuals using gesd and IQR methods
  gesd_out_raw = anomalize::gesd(x = dat_frame$residual_1, 
                                 alpha = gesd_max_anoms,
                                 max_anoms = gesd_max_anoms, 
                                 verbose = TRUE)
  
  iqr_out_raw = anomalize::iqr(x = dat_frame$residual_1, 
                               alpha = iqr_alpha, 
                               max_anoms = iqr_max_anoms, 
                               verbose = TRUE)
  
  # Create out df
  gesd_out = gesd_out_raw$outlier_report %>% 
    dplyr::select(index, gesd_outlier = outlier)
  
  iqr_out = iqr_out_raw$outlier_report %>% 
    dplyr::select(index, iqr_outlier = outlier)
  
  outlier_out = dplyr::left_join(gesd_out, iqr_out, by = c("index"))
  
  # is outlier and by which methods
  index_df = dat_frame %>%
    dplyr::mutate(index = row_number()) %>%
    dplyr::left_join(outlier_out, by = c("index")) %>% 
    dplyr::mutate(iqr_outlier = if_else(is.na(iqr_outlier), "No", iqr_outlier), 
                  gesd_outlier = if_else(is.na(gesd_outlier), "No", gesd_outlier), 
                  Outlier = case_when(iqr_outlier == "Yes" & gesd_outlier == "Yes" ~ "gesd + iqr", 
                                      iqr_outlier == "No" & gesd_outlier == "Yes" ~ "gesd", 
                                      iqr_outlier == "Yes" & gesd_outlier == "No" ~ "iqr",
                                      TRUE ~ "No"))
  index_df
}

# The above runs in a single curve
# Run in the combined data frame with all runs in all plates
Detect_outliers = function(dat_frame, gesd_alpha = 0.1, gesd_max_anoms = 0.2, 
                           iqr_alpha = 0.1, iqr_max_anoms = 0.2,
                           group_var = "RRPPRCC") {
  
  outlier_df = dat_frame %>% 
    dplyr::group_split(!!rlang::sym(group_var)) %>%
    map_df(function(x) Detect_outliers_single(x, gesd_alpha = gesd_alpha, gesd_max_anoms = gesd_max_anoms, 
                                              iqr_alpha = iqr_alpha, iqr_max_anoms = iqr_max_anoms))
  
  outlier_df
}

