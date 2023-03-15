# Create a data frame of simulated bacterial growth curves for use in tests.
# The generated data frame is based on a single sigmoid curve. This is curve is
# used as a simulated 'control' treatment. Other curves are just modifications
# of this original curve: further controls are multiplied by a factor close to 1,
# while treatments are multiplied by factors ranging from 0.01 to ~0.99 to simulate
# various growth patterns.

# *Note* that this data is generated to have a easily reproducible set of curves
# with a straightforward interpretation to be used in the programming tests of the
# functions of the package. They are not to be taken as resembling any biological
# phenomenon.

# Define sigmoid function
Sigmoid = function(x) {
	1 / (1 + exp(-x))
}

# Define simulate curves
Simulate_curves = function(Range,
													 Runs = 4,
													 Run_shift = c(0.85, 1.25, 1.1)){


	# Create Run 1 for one plate
	# Use simoid function as base to construct other curves
	# Multiply Control_1 for an arbitrary factor so curve ranges from values
	# different to [0 - 1]
	# Control values are close to original curve
	# Treatments are also multiplied by different factors to simulate
	# various growth patterns
	# Time are positive values
	Sim_OD = data.frame(Time = Range) %>%
		dplyr::mutate(Control_1 = Sigmoid(Range),
									Control_1 =  Control_1 * 21.12,
									Control_2 = Control_1*1.03,
									Control_3 = Control_1*0.97,
									Treat_1 = Control_1 * 0.985,
									Treat_2 = Control_1*0.75,
									Treat_3 = Control_1*0.25,
									Treat_4 = Control_1*0.01,
									Time = (1:nrow(.)) - 0.01,
									Run = 1,
									Plate = 1) %>%
		tidyr::pivot_longer(-c(Time, Run, Plate), names_to = "Treatment", values_to = "OD")

	# Additional runs are modified from Run 1, where OD is multiplied by
	# a user-defined factor
	Additional_runs = purrr::map(2:Runs, function(x) {
		dplyr::mutate(Sim_OD, Run = x, OD = OD*Run_shift[x - 1])
	})

	# Combine data frames and add Replicate and Position information
	# Measures distributed along the plate
	# Add noise to a single curve
	Noise = withr::with_seed(2112, code = abs(rnorm(length(Range), mean = 10, sd = 1)/10))

	# Noise is included differently in lower and higher OD values
	# So not only higher values are affected
	Final_ODs = dplyr::bind_rows(list(Sim_OD, Additional_runs)) %>%
		dplyr::mutate(Replicate = Run,
									Position = purrr::rep_along(along = 1:nrow(.), c(101, 202, 303, 404, 505, 606, 707)),
									Noise = purrr::rep_along(along = 1:nrow(.), Noise),
									OD = dplyr::case_when(Run == 4 & Treatment == "Treat_2" & OD < 1 ~ OD+(Noise*2),
																				Run == 4 & Treatment == "Treat_2" & OD >= 1 ~ OD*Noise,
																				TRUE ~ OD)) %>%
		dplyr::select(-Noise)
}


# Simulate ODs and export test data
Simulated_ODs = Simulate_curves(Range = -10:10)

# Simulated_ODs %>%
# 	# dplyr::filter(Run == 4 & Treatment == "Treat_2") %>%
# 	ggplot2::ggplot(aes(x = Time, y = OD, color = Treatment)) +
# 	ggplot2::geom_point() +
# 	ggplot2::theme_light() +
# 	ggplot2::facet_wrap(~Run)

usethis::use_data(Simulated_ODs, internal = TRUE)
