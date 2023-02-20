#'	@title Make_filtered_plot
#'
#'	@description Plot all technical replicates from the same run-plate-strain-biological replicate combination
#'
#'	@param curves_df Master data frame containing OD measurements and whether a
#'		curve was marked as abnormal.
#'	@param save_plots Should plots saved as files?
#'	@param plots_dir Path to folder where plots will be saved.
#'	@param replicate_variable Name of column containing replicate experiment number
#'
#'	@return A list of ggplot2 plots of OD over time. Each plot contains all the
#'		curves from a given run-plate-strain-biological replicate combination, with
#'		curves marked as abnormal highlighted.
#'
#'	@examples
#'	# PENDING
#'	@export
#'
# Function to plot all technical replicates from the same run-plate-strain-biological replicate combination
Make_filtered_plot = function(curves_df, save_plots = FALSE, plots_dir = NA, replicate_variable = NA){
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

  # # Iterate over each df
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
    out_plot = ggplot2::ggplot(df, aes(Time, ODc01, group = RRPPRCC)) +
      ggplot2::geom_line(data = df, color = "grey",  alpha = 0.25) +
      ggplot2::geom_line(data = dplyr::filter(df, discard_conc), color = 'red', size = 1) +
      ggplot2::scale_x_continuous(expand=c(0, 2)) +
      ggplot2::theme_light() +
      ggplot2::labs(title = Plot_title)


    # Optional: save plot to pdf
    if(save_plots == TRUE){
      # File name
      plot_filename = stringr::str_glue("Run-{rn}_Plate-{pt}_Strain-{st}_BioReplicate-{rp}.pdf",
                                        rn = Plot_label["Run"],
                                        pt = Plot_label["Plate"],
                                        st = Plot_label["Strain"],
                                        rp = Plot_label[".rep"])
      # Save
      ggplot2::ggsave(filename=file.path(plots_dir, plot_filename), plot = out_plot, device = cairo_pdf, width = 297, height = 210, units = "mm") #save the growth curves
    }
    # Return map
    out_plot

  })
  Plot_list
}
