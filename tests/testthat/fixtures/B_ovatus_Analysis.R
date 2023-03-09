# Load libraries ----------------------------------------------------------
devtools::load_all()
# library(neckaR)
library(tidyverse)
library(conflicted)

#  List file locations ----------------------------------------------------
data_dir = system.file("extdata", package = "neckaR")

# List raw plate reader files
Raw_Data_files = list.files(data_dir, pattern = "RUN")

# Tab1
Runs = file.path(data_dir, "Tab1.xlsx")

# Tab2
Layout = file.path(data_dir, "Tab2.xlsx")


# Create master data frame ------------------------------------------------
master_df = Make_master_df(Data_folder = data_dir,
													 Data_files = Raw_Data_files,
													 Runs_path = Runs,
													 Layout_path = Layout,
													 Design_tab_col = "Design")


# Plot raw curves ---------------------------------------------------------
Master_plots = Make_curve_plots(master_df,
                                save_plots = FALSE,
                                replicate_variable = "Replicate")


# Adjust OD and calculate lag and exponential phase end -------------------
adjusted_df_cutoff = Adjust_OD(curves_df = master_df,
												control_factor = "Drug",
												control_level = "control",
												offset_control = 0.02)

adjusted_df = Calculate_lag(adjusted_df_A, offset_control = 0.02)


# Plot end of exponential phase cut off -----------------------------------
cutoff_plot = Make_cutoff_plots(curves_df = adjusted_df,
																save_plots = FALSE,
																plots_dir = fig_dir,
																vline = "cutoff")

# Fix cutoffs manually ----------------------------------------------------
# Create list
replace_values = list(c(306, 11))

# replace values
readjusted_df = Fix_cutoff(adjusted_df, replace_values)


# Curve quality control ---------------------------------------------------
marked_df = Mark_artefacts(adjusted_df,
													 sum_inc = 3,
													 increased_sd = 2,
													 t0_sd = 3,
													 p_delta = 1e-3,
													 p_delta2 = 1e-3)

# Plot marked curves ------------------------------------------------------
filtered_plots = Make_filtered_plot(marked_df,
																		save_plots = FALSE,
																		replicate_variable = "Replicate")

# Detect flat curves ------------------------------------------------------
marked_flat_df = Mark_flat(marked_df,
													 OD_diff_cutoff = 0.2,
													 last_OD = 0.2)


# Plot flat curves --------------------------------------------------------
flat_plots = Make_flat_plot(marked_flat_df,
														save_plots = TRUE,
														plots_dir = fig_dir,
														replicate_variable = "Replicate")

# Calculate AUC -----------------------------------------------------------
AUC_df = Calculate_AUC(filtered_marked_df)
