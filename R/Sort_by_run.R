#' @title Sort_by_run
#' @description sorts a vector of file names based on the substring that contains
#' the run information in a case-insensitive manner.
#' @param file_list a character vector of file names to be sorted based on the
#' RUN number.
#' @return A character vector of file names sorted by the RUN number.
#' @export
# Define a function to sort filenames based on the RUN number (case-insensitive)
# file_list: a character vector of filenames to be sorted
# Returns a character vector of filenames sorted by RUN number
Sort_by_run = function(file_list) {
	# Extract the RUN number from each element of the vector
	run_nums = gsub(".*RUN([0-9]+).*", "\\1", file_list, ignore.case = TRUE)

	# Convert the run numbers to numeric values
	run_nums = as.numeric(run_nums)

	# Sort the vector based on the run numbers
	file_list_sorted = file_list[order(run_nums)]

	# Return the sorted vector
	return(file_list_sorted)
}

# filelist <- c("11_RUN01_LM16-133_NT5054.xlsx", "22_run03_LM16-135_NT5054.xlsx", "33_RuN02_LM16-134_NT5054.xlsx")
# sort_filenames_by_run(filelist)
