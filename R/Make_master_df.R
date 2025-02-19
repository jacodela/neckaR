#' @title Make_master_df
#' @description Combines OD measurements with with run and layout information
#' of an experiment
#' @param Data_folder Path to the folder containing Excel files from TECAN plate
#' reader
#' @param Data_files Path to Excel files from TECAN plate reader.
#' @param Runs_path Path to Runs (a.k.a. Tab1) table
#' @param Layout_path Path to Layout (a.k.a. Tab2) table
#' @param Duration Maximum number of time points measured in the experiment
#' @param Design_tab_col Name of column with design information in Runs and
#' Layout tables
#' @param Plate_type Type of plate. Possible values are "24-well" and "96-well"
#' @return A data frame object that contains the OD measurements from a
#' all the plates from all runs included, combined with the Runs and Layout
#' information. Columns correspond to those included in Runs and Layout tables.
#' In addition, `Make_master_df()` creates a series of variables to identify
#' each curve and each measurement. They are:
#' * `ID`: numeric value where the thousands and tens of thousands positions
#' correspond to the plate number [1 to # of plates]. Hundreds position is
#' the row number [1 to 8]. Tens and units are the columns [01 to 12]. In the
#' case of 24-well plates, the aforementioned values range from [1 to 4] for
#' rows and from [1 to 6] for the columns.
#' * `RRPPRCC`: Unique curve identifier, representing
#' (Run, Run, Plate, Plate, Row (in plate [1 to 8]), Column (in plate [01 to 12])).
#' In the case of 24-well plates, the aforementioned values range from [1 to 4]
#' for rows and from [1 to 6] for the columns.
#' @export
Make_master_df = function(Data_folder,
													Data_files,
													Runs_path,
													Layout_path,
													Duration = 20,
													Design_tab_col = "Design",
													Plate_type = "96-well"){

	## Load Runs and Layout files
	Runs_df = readxl::read_excel(path = Runs_path, sheet = 1)

	Layout_df = readxl::read_excel(path = Layout_path, sheet = 1)

	## Prepare table names to work with functions
	colnames(Runs_df) = Fix_table_names(colnames(Runs_df))
	colnames(Layout_df) = Fix_table_names(colnames(Layout_df))

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
			if(Plate_type == "96-well"){
				OD_from_excel = Read_OD_96(filename = file.path(Data_folder, Data_files[rn]),
													 sheet = plt, duration = Duration)
			} else if (Plate_type == "24-well"){
				OD_from_excel = Read_OD_24(filename = file.path(Data_folder, Data_files[rn]),
													 sheet = plt, duration = Duration)
			} else {
				warning('Please specify a valid plate type')
			}

			OD_from_excel %>%
				dplyr::mutate(RRPPRCC = rn * 100000 + ID, ## Add identifier (Run, Run, Plate, Plate, Row, Column, Column)
											Position = as.numeric(substr(ID, nchar(ID)-2, nchar(ID)))) %>%
				dplyr::bind_cols(dplyr::slice(Runs_df, index)) %>%  ## Add corresponding data from Runs table
				dplyr::left_join(., Layout_df, by = c("Position", Design_tab_col))
		}
		)

	} else {
		warning('Runs in overview and number of excel files are not matching')
		}


	## Return
	OD_df
}
