# Function to plot all technical replicates from the same run-plate-strain-biological replicate combination
Make_spline_plots = function(Master_df, OD_col = "OD", print_plots = FALSE, 
                             save_plots = FALSE, plots_dir = NA,
                             show_outliers = FALSE, replicate_variable = NA){
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
  
  # Iterate over each df
  purrr::map(split_df, function(df){
    
    # Extract data to generate plot label
    Plot_label  = df %>% 
      dplyr::select(Run, Plate, Strain, .rep) %>% 
      dplyr::distinct() %>% 
      unlist() 
    
    Plot_title = stringr::str_glue("Splines {rn} - Plate {pt} - Strain {st} - Replicate {rp}",
                                   rn = Plot_label["Run"], 
                                   pt = Plot_label["Plate"], 
                                   st = Plot_label["Strain"], 
                                   rp = Plot_label[".rep"])
    # Create plot
    out_plot = ggplot2::ggplot(df, aes(x = Time, y = OD)) +
      ggplot2::geom_line(aes(y = fitted_1), color = "steelblue", size = 0.5) +
      ggplot2::geom_line(aes(y = fitted_2), color = "forestgreen", size = 0.5) +
      ggplot2::geom_point(pch='.') +
      ggplot2::facet_wrap(~RRPPRCC, nrow=8,ncol=12) +
      ggplot2::theme_light() +
      ggplot2::labs(title = Plot_title)
    
    if(show_outliers == TRUE){
      outlier_colors= c("No" = "black", "gesd + iqr" = "Red", "iqr" = "tan2", "gesd" = "darkorchid1")
      out_plot = out_plot +
        geom_point(aes(color = Outlier)) +
        scale_color_manual(values = outlier_colors)
        
    }
    
    # Optional: print plot
    if(print_plots == TRUE){
      print(out_plot)
    }
    
    # Optional: save plot to pdf
    if(save_plots == TRUE){
      # File name
      plot_filename = stringr::str_glue("Splines-{rn}_Plate-{pt}_Strain-{st}_Replicate-{rp}.pdf",
                                        rn = Plot_label["Run"], 
                                        pt = Plot_label["Plate"], 
                                        st = Plot_label["Strain"], 
                                        rp = Plot_label[".rep"])
      # Save
      ggplot2::ggsave(filename=file.path(plots_dir, plot_filename), plot = out_plot, device = cairo_pdf, width = 297, height = 210, units = "mm") #save the growth curves
    }
  })
}