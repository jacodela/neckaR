#' @title Read_OD_24
#' @description Reads excel files from TECAN plate reader and converts the data
#' into a long data frame object. It assumes that each sheet corresponds to
#' a single 24-well plate. Time is converted to hours instead of days. An identifier
#' field is created, where digits corresponding to thousands and tens of thousands
#' indicate the plate number [1 to #plates]; hundreds indicates the row number [1 to 4];
#' tens and units are the columns [1 to 6].
#' @param filename Full path to an Excel file with OD measurements from
#' TECAN plate reader
#' @param sheet Number of sheet to be read from Excel
#' @param duration Number of time points measured to include from the loaded tables.
#' To load all measurements, use `duration = "all"`.
#' @return A data frame object that contains the OD measurements from a
#' single sheet in long format
#' @export
Read_OD_24 = function(filename, sheet, duration){
	Raw_results = suppressMessages(readxl::read_excel(path = filename, sheet = sheet))
	## Second column, where the important data is
	Second_col = unname(unlist(Raw_results[,2]))
	## Row number where plate data is stored
	Plate_index = grep("^Plate\\s\\d+", Second_col)[1]
	## ID of the plate
	## Only retain plate number
	Plate_number = as.numeric(sub("Plate ", "", Second_col[Plate_index]))

	## Times
	Time_index = grep("Time", Second_col, ignore.case = FALSE)
	## Values of time measurements
	if(duration == "all"){
		Time_vector = Raw_results[(Time_index[1]+1):nrow(Raw_results), 2]
		Time_vector = suppressWarnings(as.numeric(unlist(Time_vector)))
		Time_vector = Time_vector[1:(which(is.na(Time_vector))[1] - 1)]
		Time_hours_zeros = Time_vector * 24
	} else {
		Time_vector = Raw_results[(Time_index[1]+1):(Time_index[1]+1+duration),2]# Find out the timepoints of your experiment
		Time_hours_zeros = suppressWarnings(as.numeric(unlist(Time_vector))) * 24
	}
	## Time vector as hours and only the nonzero ones (except the first can be zero)
	Time_hours_headless = Time_hours_zeros[-1]
	Time_hours = c(Time_hours_zeros[1], Time_hours_headless[Time_hours_headless > 0])

	## Well ID
	## Thousands and tens of thousands are the plate number [1 to #plates]. Hundreds is the row number [1 to 4]. Tens and units are the columns [1 to 6].
	Identifier = vector(mode="numeric", length=0) # Create identifiers: first and second digit: plate number, third digit: row  forth and fifth digit: column

	for (i in c(1:4)){
		for (j in c(1:6)){
			Identifier <- c(Identifier, (Plate_number*1000+i*100+j))
		}
	}
	Identifier

	## Create matrix of OD measurements
	OD = Raw_results[(Time_index[1]+1):(Time_index[1]+length(Time_hours)),4:27]
	OD2 = as.data.frame(dplyr::mutate(OD, dplyr::across(dplyr::everything(), as.numeric)))

	## Add times and IDs to OD wide data frame
	colnames(OD2) = Identifier
	rownames(OD2) = Time_hours

	## Create table in long format with OD and corresponding ID
	OD_tmp = tibble::rownames_to_column(OD2, "Time")
	OD_tmp = tidyr::pivot_longer(OD_tmp, -Time, names_to = "ID", values_to = "OD")
	OD_tmp = dplyr::mutate(OD_tmp, Time = as.numeric(Time), ID = as.integer(ID))
	OD_tmp = dplyr::arrange(OD_tmp, ID)
	OD_df = as.data.frame(OD_tmp)

	# Return
	OD_df
}
