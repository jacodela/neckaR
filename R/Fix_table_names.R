#'	@title Fix_table_names
#'
#'	@description Fix column names in Runs and Layout tables. All column names start
#'	with uppercase, replace spaces with underscores and executes universal name
#'	repair.
#'
#'	@param col_names Vector of column names to repair
#'
#'	@return A vector of same length as `col_names` with repaired names
#'   @export
Fix_table_names = function(col_names){
  fixed_names = col_names %>%
    stringr::str_replace(" ", "_") %>%
    stringr::str_to_title() %>%
    vctrs::vec_as_names(repair = "universal")
  fixed_names
}
