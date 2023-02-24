#' @title Make_curve_plots
#' @description Plot all technical replicates from the same run-plate-strain-biological replicate combination
#' @param curves_df Master data frame containing raw OD measurements and
#' Runs and Layout data.
#' @param save_plots Should plots saved as files?
#' @param plots_dir Path to folder where plots will be saved.
#' @param replicate_variable Name of column containing replicate experiment number
#' @return A single ggplot2 plot of raw OD over time, faceted by run-plate-strain-biological
#' replicate combination.
#' @export
Make_curve_plots = function(curves_df, save_plots = FALSE, plots_dir = NA, replicate_variable = NA){

  # Split master data frame into individual dfs
  # Should the data be split by replicate?
  if(is.na(replicate_variable)){
    split_df = curves_df %>%
      dplyr::mutate(.rep = "NA") %>%
      dplyr::group_split(Run, Plate, Strain)
  } else {
    split_df = curves_df %>%
      dplyr::rename(".rep" = replicate_variable) %>%
      dplyr::group_split(Run, Plate, Strain, .rep)
  }


  # Iterate over each df
  Plot_list = purrr::map(split_df, function(df){

    # Extract data to generate plot label
    Plot_label  = df %>%
      dplyr::select(Run, Plate, Strain, .rep) %>%
      dplyr::distinct() %>%
      unlist()

    Plot_title = stringr::str_glue("Run {rn} - Plate {pt} - Strain {st} - Replicate {rp}",
                                   rn = Plot_label["Run"],
                                   pt = Plot_label["Plate"],
                                   st = Plot_label["Strain"],
                                   rp = Plot_label[".rep"])
    # Create plot
    out_plot = ggplot2::ggplot(df, aes(x = Time, y = OD)) +
      ggplot2::geom_point(shape = ".") +
      ggplot2::facet_wrap(~ID, nrow=8,ncol=12) +
      ggplot2::theme_light() +
      ggplot2::labs(title = Plot_title)

    # Optional: save plot to pdf
    if(save_plots == TRUE){
      # File name
      plot_filename = stringr::str_glue("Run-{rn}_Plate-{pt}_Strain-{st}_Replicate-{rp}.pdf",
                                        rn = Plot_label["Run"],
                                        pt = Plot_label["Plate"],
                                        st = Plot_label["Strain"],
                                        rp = Plot_label[".rep"])
      # Save
      ggplot2::ggsave(filename=file.path(plots_dir, plot_filename), plot = out_plot, device = grDevices::cairo_pdf, width = 297, height = 210, units = "mm") #save the growth curves
    }

    # Return map
    out_plot
  })

  # return main function
  Plot_list
}
