# Function to mark artefacts on growth curves
# Slightly cleaned from original functions
# Otherwise, inner working is unchanged
Mark_artefacts = function(GCs, sum_inc = 3, increased_sd = 2, t0_sd = 3,
                          p_delta = 1e-3, p_delta2 = 1e-3) {
  
  #' Determine is a given time point is below than cutoff time
  #' And truncate curves at cutoff
  GCsf_A = GCs %>% 
  	dplyr::filter(Time < (cutoff_time + 0.5)) %>% 
    dplyr::mutate(leq_cutoff = Time < (cutoff_time + 0.5)) %>% 
    dplyr::arrange(Time)
  
  #' Calculate ODc01
  #' ODc01 is a normalized OD value
  #' value of 1 represents the “maximum growth” observed in the plate, where 0 is no growth
  
  GCsf_B = GCsf_A %>%
    dplyr::group_by(RRPP) %>%
    dplyr::mutate(ODc01 = ODc0 / median(ODc0[ Time == max(Time[leq_cutoff]) & Control ])) %>%
    dplyr::relocate(ODc01) %>% 
    dplyr::ungroup()
  
  
  #' Flag abnormal curves:

  #' Calculate mean and sd values of ODc01 of controls
  #' Curves with `abnormal_t0` are those with a high t0 OD values the mean and
  #' SD values of controls

  #' Flag abnormalities by determining points that seem to be spikes:
  #' It assumes that OD values are constantly increasing or at least not decreasing
  #' Determine if a point is `increased`, i.e. whether the OD at a given time
  #' is greater that the minimum OD value in the remaining section of the curve
  #' plus an offset value, which corresponds to n times the SD of controls at 
  #' time 0. Then the number of spikes is added up and curves with a number above
  #' a certain threshold are marked as abnormal

  #' Flag abnormalities according distribution of deltas:
  #' The mark artifacts function uses the difference between two consecutive 
  #' measurements (delta) and two consecutive deltas (delta2) to find abnormal
  #' curves.
  #' It assumes that that delta and delta2 follow an exponential distribution 
  #' within a given strain. The `fitdistrplus::fitdist` function, fits an 
  #' exponential distribution to the observed set of deltas and extracts the rate
  #' (a.k.a lambda) from that particular fit. Using the `pexp` function, for each
  #' delta and delta2, it calculates the probability of observing a value equals 
  #' to or greater than it (*note* the lower.tail = FALSE) using an exponential
  #' distribution with the calculated rate (a.k.a. lambda or `estimate`).
  
  #' Finally, it marks an abnormality if, for a given curve, the smallest observed
  #' delta and delta2 have a probability below the given thresholds. 
  #' The value of these thresholds can be changed according to how stringent
  #' the abnormality detection wants to be: Smaller p_delta and p_delta2 values
  #' make the filtering more lenient, higher values are more strict
  
  #' The interpretation of this detection can be briefly explained as follows: 
  #' for a given curve, if the minimum delta and delta2 values have a high value
  #' in comparison to the overall distribution within a given strain
  #' then the probability of observing it will be low, therefore the curve will 
  #' be marked as an abnormality.

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
                  sum_inc = sum(increased), 
                  is_increased = (sum(increased) > sum_inc), 
                  abnormal_t0 = !(min(ODc01[Time < 1.5]) < t0_sd * sd_control_start_OD),
                  signif_delta = (min(pdelta, na.rm=T) < p_delta),
                  signif_delta2 = (min(pdelta2, na.rm=T) < p_delta2), 
                  discard_conc = (is_increased | abnormal_t0 | signif_delta | signif_delta2)) %>% 
    dplyr::ungroup()
  
  # Return
  GCsf_D
}