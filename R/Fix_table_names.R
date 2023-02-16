# Function to fix names in tables
# Repair names in case of missing values or other variables
# All start with uppercase
# Replace spaces with underscore
fix_table_names = function(col_names){
  fixed_names = col_names %>% 
    stringr::str_replace(" ", "_") %>% 
    stringr::str_to_title() %>% 
    vctrs::vec_as_names(repair = "universal")
  fixed_names
}