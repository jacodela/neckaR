Make_flat_plot = function(Master_df, save_plots = FALSE, plots_dir = NA, replicate_variable = NA){
  # Split master data frame into individual dfs
  # Should the data be split by replicate?
  if(is.na(replicate_variable)){
    split_df = Master_df %>%
      dplyr::mutate(.rep = "NA") %>%
      dplyr::group_split(Run, Plate, Strain)
  } else {
    split_df = Master_df %>%
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
      ggplot2::geom_line(data = dplyr::filter(df , flat == TRUE), color = 'orange2', size = 1) +
      ggplot2::scale_x_continuous(expand=c(0, 2)) +
      ggplot2::theme_light() +
      ggplot2::labs(title = Plot_title)


    # Optional: save plot to pdf
    if(save_plots == TRUE){
      # File name
      plot_filename = stringr::str_glue("Flats_Run-{rn}_Plate-{pt}_Strain-{st}_eplicate-{rp}.pdf",
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
  Plot_list
}
