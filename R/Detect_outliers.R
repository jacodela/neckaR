#' @title Detect_outliers_single
#' @description Detect outlier OD measurements based on residuals of spline fit of
#' a single growth curve
#' @param curves_df Master data frame containing OD measurements and
#' Runs and Layout data.
#' @param gesd_alpha PENDING
#' @param gesd_max_anoms PENDING
#' @param iqr_alpha PENDING
#' @param iqr_max_anoms PENDING
#' @return PENDING
#' @export
Detect_outliers_single = function(curves_df, gesd_alpha, gesd_max_anoms, iqr_alpha, iqr_max_anoms){
  # Detect outliers of residuals using gesd and IQR methods
  gesd_out_raw = anomalize::gesd(x = curves_df$residual_1,
                                 alpha = gesd_max_anoms,
                                 max_anoms = gesd_max_anoms,
                                 verbose = TRUE)

  iqr_out_raw = anomalize::iqr(x = curves_df$residual_1,
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
  index_df = curves_df %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    dplyr::left_join(outlier_out, by = c("index")) %>%
    dplyr::mutate(iqr_outlier = dplyr::if_else(is.na(iqr_outlier), "No", iqr_outlier),
                  gesd_outlier = dplyr::if_else(is.na(gesd_outlier), "No", gesd_outlier),
                  Outlier = dplyr::case_when(iqr_outlier == "Yes" & gesd_outlier == "Yes" ~ "gesd + iqr",
                                      iqr_outlier == "No" & gesd_outlier == "Yes" ~ "gesd",
                                      iqr_outlier == "Yes" & gesd_outlier == "No" ~ "iqr",
                                      TRUE ~ "No"))
  index_df
}

#' @title Detect_outliers
#' @description Detect outlier OD measurements based on residuals of spline fit of
#' of a series of bacterial growth curves.
#' @param curves_df Master data frame containing OD measurements and
#' Runs and Layout data.
#' @param gesd_alpha PENDING
#' @param gesd_max_anoms PENDING
#' @param iqr_alpha PENDING
#' @param iqr_max_anoms PENDING
#' @param group_var Variables in curves_df used to group curves.
#' @return PENDING
#' @export
Detect_outliers = function(curves_df, gesd_alpha = 0.1, gesd_max_anoms = 0.2,
                           iqr_alpha = 0.1, iqr_max_anoms = 0.2,
                           group_var = "RRPPRCC") {

  outlier_df = curves_df %>%
    dplyr::group_split(!!rlang::sym(group_var)) %>%
    purrr::map_df(function(x) Detect_outliers_single(x, gesd_alpha = gesd_alpha, gesd_max_anoms = gesd_max_anoms,
                                              iqr_alpha = iqr_alpha, iqr_max_anoms = iqr_max_anoms))

  outlier_df
}

