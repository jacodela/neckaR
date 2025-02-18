test_that("Sort_by_run returns a sorted vector", {
	unsorted_vector = c("11_RUN01_LM16-133_NT5054.xlsx", "22_run10_LM16-135_NT5054.xlsx", "33_RuN02_LM16-134_NT5054.xlsx")
	sorted_vector = c("11_RUN01_LM16-133_NT5054.xlsx", "33_RuN02_LM16-134_NT5054.xlsx",  "22_run10_LM16-135_NT5054.xlsx")
	expect_equal(Sort_by_run(unsorted_vector), sorted_vector)
})

test_that("Sort_by_run returns a character vector", {
	unsorted_vector = c("11_RUN01_LM16-133_NT5054.xlsx", "22_run10_LM16-135_NT5054.xlsx", "33_RuN02_LM16-134_NT5054.xlsx")
	sorted_vector = c("11_RUN01_LM16-133_NT5054.xlsx", "33_RuN02_LM16-134_NT5054.xlsx",  "22_run10_LM16-135_NT5054.xlsx")
	expect_vector(Sort_by_run(unsorted_vector), ptype = character(), size = length(unsorted_vector))
})
