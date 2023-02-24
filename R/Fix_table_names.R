#' @title Fix_table_names
#' @description Fix column names in Runs and Layout tables. All column names start
#' with uppercase, replace spaces with underscores and executes universal name
#' repair.
#' @param col_names Vector of column names to repair
#' @return A vector of same length as `col_names` with repaired names
#' @examples
#' colnames(mtcars)
#' # [1] "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear" "carb"
#' Fix_table_names(colnames(mtcars))
#' # [1] "Mpg"  "Cyl"  "Disp" "Hp"   "Drat" "Wt"   "Qsec" "Vs"   "Am"   "Gear" "Carb"
#' @export
Fix_table_names = function(col_names){
  fixed_names = col_names %>%
    stringr::str_replace(" ", "_") %>%
    stringr::str_to_title() %>%
    vctrs::vec_as_names(repair = "universal")
  fixed_names
}
