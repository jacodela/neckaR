# Function to plot curves with cutoff values
Make_cutoff_plots = function(curves_df, save_plots = FALSE, 
                             plots_dir = NA, vline = "cutoff", 
                             scales = "fixed", plot_name = "Control_cutoff"){
  
  cutoff_plot = curves_df %>%
    dplyr::filter(Control == TRUE) %>%
    ggplot2::ggplot(aes(Time, OD, group = Position)) +
    ggplot2::geom_line(alpha = 0.5) + 
    ggplot2::coord_cartesian(ylim = c(0,1)) +
    ggplot2::facet_wrap(~RRPP, scales = scales) + 
    ggplot2::theme(legend.position = "none") +
    ggplot2::theme_light()
  
  # Which line to plot
  if(vline == "cutoff"){
    cutoff_plot = cutoff_plot + 
      ggplot2::geom_vline(aes(xintercept = cutoff_time))
  } else if (vline == "lag"){
    cutoff_plot = cutoff_plot + 
      ggplot2::geom_vline(aes(xintercept = lag_time))
  } else if (vline == "both"){
    cutoff_plot = cutoff_plot + 
      ggplot2::geom_vline(aes(xintercept = cutoff_time)) +
      ggplot2::geom_vline(aes(xintercept = lag_time))
  }
  
  # Optional: save plot to pdf
  if(save_plots == TRUE){
    # File name
    plot_filename = paste0(plot_name, ".pdf")
    # Save
    ggplot2::ggsave(filename=file.path(plots_dir, plot_filename), 
                    plot = cutoff_plot, device = cairo_pdf, 
                    width = 297, height = 210, units = "mm") #save the growth curves
  } 
  # Return
  cutoff_plot
}


# Function to plot all technical replicates from the same run-plate-strain-biological replicate combination
Make_lag_plots = function(curves_df, OD_col = "OD", 
                          save_plots = FALSE, plots_dir = NA,
                          replicate_variable = NA){

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
  purrr::map(split_df, function(df){
    
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
      ggplot2::geom_line(alpha = 0.5) + 
      ggplot2::theme(legend.position = "none") +
      ggplot2::geom_vline(aes(xintercept = lag_time)) +
      ggplot2::facet_wrap(~RRPPRCC, nrow=8,ncol=12) +
      ggplot2::theme_light() +
      ggplot2::labs(title = Plot_title)
    
    # Print
    print(out_plot)
    
    # Optional: save plot to pdf
    if(save_plots == TRUE){
      # File name
      plot_filename = stringr::str_glue("Lag-time-{rn}_Plate-{pt}_Strain-{st}_Replicate-{rp}.pdf",
                                        rn = Plot_label["Run"], 
                                        pt = Plot_label["Plate"], 
                                        st = Plot_label["Strain"], 
                                        rp = Plot_label[".rep"])
      # Save
      ggplot2::ggsave(filename=file.path(plots_dir, plot_filename), plot = out_plot, device = cairo_pdf, width = 297, height = 210, units = "mm") #save the growth curves
    }
  })
}