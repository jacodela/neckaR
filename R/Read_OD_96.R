#' @title Read_OD_96
#'
#' @description Reads excel files from TECAN plate reader and converts the data
#'	into a long data frame object. It assumes that each sheet corresponds to
#'	a single 96-well plate. Time is converted to hours instead of days. An identifier
#'	field is created, where digits corresponding to thousands and tens of thousands
#'	indicate the plate number [1 to #plates]; hundreds indicates the row number [1 to 8];
#'	tens and units are the columns [1 to 12].
#'
#' @param filename Full path to an Excel file with OD measurements from
#'	TECAN plate reader
#' @param sheet Number of sheet to be read from Excel
#' @param duration Maximum number of time points measured in the experiment
#'
#' @return A data frame object that contains the OD measurements from a
#'	single sheet in long format
#' @examples
#' plate_path <- "./user_data/example.xlsx"
#' output_table <- Read_OD_96(filename = plate_path, sheet = 1, duration = 20)
#' @export
Read_OD_96 = function(filename, sheet, duration){
  Raw_results = suppressMessages(readxl::read_excel(path = filename, sheet = sheet))
  ## Second column, where the important data is
  Second_col = unname(unlist(Raw_results[,2]))
  ## Row number where plate data is stored
  Plate_index = grep('Plate', Second_col)[1]
  ## ID of the plate
  ## Only retain plate number
  Plate_number = as.numeric(sub("Plate ", "", Second_col[Plate_index]))

  ## Times
  Time_index = grep("Time", Second_col, ignore.case = FALSE)
  ## Values of time measurements
  Time_vector = Raw_results[(Time_index[1]+1):(Time_index[1]+1+duration),2]# Find out the timepoints of your experiment
  ## Convert to numeric vector, remove NAs and convert to hours instead of days
  Time_hours_zeros = as.numeric(unlist(Time_vector))*24
  ## Time vector as hours and only the nonzero ones (except the first can be zero)
  Time_hours_headless = Time_hours_zeros[-1]
  Time_hours = c(Time_hours_zeros[1], Time_hours_headless[Time_hours_headless > 0])

  ## Well ID
  ## Thousands and tens of thousands are the plate number [1 to #plates]. Hundreds is the row number [1 to 8]. Tens and units are the columns [1 to 12].
  Identifier = vector(mode="numeric", length=0) # Create identifiers: first and second digit: plate number, third digit: row  forth and fifth digit: column

  for (i in c(1:8)){
    for (j in c(1:12)){
      Identifier <- c(Identifier, (Plate_number*1000+i*100+j))
    }
  }
  Identifier

  ## Create matrix of OD measurements
  OD = Raw_results[(Time_index[1]+1):(Time_index[1]+length(Time_hours)),4:99]
  OD2 = as.data.frame(dplyr::mutate(OD, across(everything(), as.numeric)))

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
