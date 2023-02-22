#'	@title Mark_artefacts
#'
#'	@description Calculates a normalized OD value based on the maximum growth observed
#'		in the controls of a given plate. Then, uses the normalized OD to determine whether
#'		a curve shows an abnormal behavior. The function perform several tests to determine
#'		whether a curve is abnormal and marks them as such if at least one test is failed.
#'
#'		Before identifying faulty curves, the function truncates curves at the time of
#'		transition from exponential to stationary phase. Then, it calculates a normalized
#'		OD, called `ODc01`. This normalized OD ranges from 0 to 1, where values
#'		represent the median growth of the controls at time 0 and the the start of
#'		stationary phase, in other words, they are the minimum and maximum observed
#'		ODs of controls in the plate.
#'
#'		Then, for each curve, the following possible abnormalities are evaluated:

#'		* High staring OD: The mean and SD values of `ODc01` of control curves at
#'		time 0 are calculated. The OD at time 0 of each curve is compared to the mean
#'		of the controls and those that are above a certain number of standard
#'		deviations (`t0_sd`) will be flagged as abnormal. The value of the SD threshold
#'		can be specified by the user. Lower `t0_sd` values make the filtering more strict.

#'		* Spike measurements: certain curves may have an abnormally increased OD in
#'		the earlier time points that then go down. However, we assume that OD values are
#'		increasing in time (or at least not decreasing). To detect these points, the
#'		function determines whether the OD at a given time is greater that the minimum
#'		OD in the remaining section of the curve, plus an offset value which corresponds
#'		to n times the SD of controls at time 0 (`increased_sd`). Then the number of
#'		spikes is added up and curves with a number above a certain threshold (`sum_inc`)
#'		are marked as abnormal. Smaller `increased_sd` and `sum_inc` values make the
#'		filtering more strict.

#'		* Distribution of deltas: the difference between two consecutive measurements
#'		(`delta`) and two consecutive deltas (`delta2`) is used to find abnormal curves.
#'		It assumes that that `delta` and `delta2` follow exponential distributions
#'		within a given strain. A parameter of the exponential distribution is obtained
#'		and the the probability of observing a value that is equal to or greater than it
#'		is calculated. A curve is marked as an abnormality if the smallest observed
#'		`delta` and `delta2` have a probability below a threshold specified by the user.
#'		The interpretation of this detection can be briefly explained as follows:
#'		for a given curve, if the minimum `delta` and `delta2` values have a high value
#'		in comparison to the overall distribution within all curves from the strain, then
#'		the probability of observing it will be low, therefore the curve will be marked as
#'		an abnormality. The value of these thresholds can be changed according to how
#'		stringent we want to make the abnormality detection: smaller `p_delta` and
#'		`p_delta2` values make the filtering more lenient, higher values are more strict.
#'
#'	@param curves_df Master data frame containing raw OD measurements and
#'		Runs and Layout data.
#'	@param sum_inc Maximum number of spikes allowed in a given curve. Curves with >= `sum_inc` spikes are marked as abnormal. Smaller values make the detection more strict.
#'	@param increased_sd  Number of times the SD of controls at time 0 for the detection of spikes in OD. Smaller values make the detection more strict.
#'	@param t0_sd Number of OD standard deviations of controls at time 0 above which the starting OD of a given curve is marked as abnormal. Smaller values make the detection more strict.
#'	@param p_delta Probability delta values of two consecutive OD measurements equal to or greater than the observed according to an exponential distribution. Higher values make the detection more strict.
#'	@param p_delta2 Probability delta values of two consecutive delta values equal to or greater than the observed according to an exponential distribution. Higher values make the detection more strict.
#'	@return A data frame object with the normalized OD, the result of each of the
#'		tests and whether a given curve is marked as abnormal to the input master
#'		data frame.
#'	@examples
#'	# PENDING
#'	@export
Mark_artefacts = function(curves_df, sum_inc = 3, increased_sd = 2, t0_sd = 3,
                          p_delta = 1e-3, p_delta2 = 1e-3) {

  ## Determine is a given time point is below than cutoff time
  ## And truncate curves at cutoff
  GCsf_A = curves_df %>%
  	dplyr::filter(Time < (cutoff_time + 0.5)) %>%
  	dplyr::arrange(Time) %>%
    dplyr::mutate(leq_cutoff = Time < (cutoff_time + 0.5))

  ## Calculate ODc01
  ## ODc01 is a normalized OD value
  ## value of 1 represents the “maximum growth” observed in the plate, where 0 is no growth

  GCsf_B = GCsf_A %>%
    dplyr::group_by(RRPP) %>%
    dplyr::mutate(ODc01 = ODc0 / median(ODc0[ Time == max(Time[leq_cutoff]) & Control ])) %>%
    dplyr::relocate(ODc01) %>%
    dplyr::ungroup()


  control_start_OD = GCsf_B %>%
  dplyr::filter(Time < 0.5,
                Control == TRUE,
                !is.na(ODc01),
                !is.infinite(ODc01)) %>%
    dplyr::pull(ODc01)

  control_start_OD = control_start_OD[ abs(control_start_OD) < 1 ]
  mean_control_start_OD = mean(control_start_OD)
  sd_control_start_OD = sd(control_start_OD)


  GCsf_C = GCsf_B %>%
    dplyr::group_by(RRPPRCC) %>%
    dplyr::mutate(delta = ODc0 - c(NA, ODc0[1:(dplyr::n()-1)])) %>% # Delta equals ODc0_time(n) - ODc0_time(n-1)
    dplyr::mutate(delta2 = delta - c(NA, delta[1:(dplyr::n()-1)])) %>% # Delta2 equals Delta_time(n) - Delta_time(n-1)
    dplyr::group_by(Strain) %>%
    dplyr::mutate(pdelta = pexp( delta, fitdistrplus::fitdist( delta[ !is.na(delta) & (delta > 0) ] , "exp")$estimate, lower.tail = FALSE ), # Fit exponential distribution to deltas vector per strain
                  pdelta2 = pexp( delta2, fitdistrplus::fitdist( abs(delta2[ !is.na(delta2) ]), "exp")$estimate, lower.tail = FALSE ))


  GCsf_D = GCsf_C %>%
    dplyr::group_by(RRPPRCC) %>%
    dplyr::mutate(increased = (ODc01 > (increased_sd * sd_control_start_OD + cumulmin(ODc01))) & (Time < max(Time[leq_cutoff])/2),
                  # sum_inc = sum(increased),
                  is_increased = (sum(increased) > sum_inc),
                  abnormal_t0 = !(min(ODc01[Time < 1.5]) < t0_sd * sd_control_start_OD),
                  signif_delta = (min(pdelta, na.rm=T) < p_delta),
                  signif_delta2 = (min(pdelta2, na.rm=T) < p_delta2),
                  discard_conc = (is_increased | abnormal_t0 | signif_delta | signif_delta2)) %>%
    dplyr::ungroup()

  # Return
  GCsf_D
}
