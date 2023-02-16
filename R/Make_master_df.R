#' @title Make_master_df
#'
#' @description Combines OD measurements with with run and layout information
#'	of an experiment
#'
#' @param Data_folder Path to the folder containing Excel files from TECAN plate
#'	reader
#' @param Data_files Path to Excel files from TECAN plate reader.
#' @param Runs_path Path to Runs (a.k.a. Tab1) table
#' @param Layout_path Path to Layout (a.k.a. Tab2) table
#' @param Duration Maximum number of time points measured in the experiment
#' @param Design_tab_col Name of column with design information in Runs and
#'	Layout tables
#'
#' @return A data frame object that contains the OD measurements from a
#'	all the plates from all runs included, combined with the Runs and Layout
#'	information
#' @examples
#' # List file locations
#' data_dir <- "./user_data/"
#' # List raw plate reader files
#' Raw_Data_files <- list.files(data_dir, pattern = "RUN")
#' # Tab1
#' Runs <- file.path(data_dir, "Tab1.xlsx")
#' # Tab2
#' Layout <- file.path(data_dir, "Tab2.xlsx")
#' master_df <- Make_master_df(Data_folder = data_dir, Data_files = Raw_Data_files, Runs_path = Runs, Layout_path = Layout, Design_tab_col = "Design")
#'
#' @export
Make_master_df = function(Data_folder,
													Data_files,
													Runs_path,
													Layout_path,
													Duration = 20,
													Design_tab_col = "Design"){

	## Load Runs and Layout files
	Runs_df = readxl::read_excel(path = Runs_path, sheet = 1)

	Layout_df = readxl::read_excel(path = Layout_path, sheet = 1)

	## Prepare table names to work with functions
	colnames(Runs_df) = fix_table_names(colnames(Runs_df))
	colnames(Layout_df) = fix_table_names(colnames(Layout_df))

	## Determine number of runs
	number_runs = Runs_df %>%
		dplyr::count(Run) %>%
		nrow()

	## Check that number of sheets in excel files matches the number of runs stated in the Tab1 file
	if(length(Data_files) == number_runs) ## Number of runs should match number of excel sheets
	{
		Plates_run_grid = Runs_df %>%
			dplyr::select(Run, Plate)

		##Iterate over each run-Plate combination and read excel file
		OD_df = purrr::map2_df(Plates_run_grid$Run, Plates_run_grid$Plate, function(rn, plt){

			print(stringr::str_glue("Run {run} - Plate {plate}",
															run = rn,
															plate = plt))

			## find out index of lines to retrieve info on Plate design, strain and replicate
			index = which(Runs_df$Run == rn & Runs_df$Plate == plt)

			## Read excel file from each run-Plate combination and modify columns
			## Combine with layout info from Tab2
			Read_OD_96(filename = file.path(Data_folder, Data_files[rn]),
								 sheet = plt, duration = Duration) %>%
				dplyr::mutate(RRPPRCC = rn * 100000 + ID, ## Add identifier (Run, Run, Plate, Plate, Row, Column, Column)
											Position = as.numeric(substr(ID, nchar(ID)-2, nchar(ID)))) %>%
				dplyr::bind_cols(slice(Runs_df, index)) %>%  ## Add corresponding data from Runs table
				dplyr::left_join(., Layout_df, by = c("Position", Design_tab_col)) %>%
				dplyr::select(OD, Time, Position, Run, Plate,
											Design, Strain, Replicate, Drug,
											Drug_name, ID, RRPPRCC)
		}
		)

	}else
	{warning('Runs in overview and number of excel files are not matching')}


	## Return
	OD_df
}
