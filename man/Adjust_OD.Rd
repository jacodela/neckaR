% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Adjust_OD.R
\name{Adjust_OD}
\alias{Adjust_OD}
\title{Adjust_OD}
\usage{
Adjust_OD(curves_df, control_factor, control_level, offset_control)
}
\arguments{
\item{curves_df}{Master data frame containing raw OD measurements and
Runs and Layout data.}

\item{control_factor}{Name of column in master data frame containing
treatment information.}

\item{control_level}{Label of the control treatment.
(e.g. "control", "DMSO", "water").}

\item{offset_control}{Scaling factor used to account for small increases in
OD during stationary phase.}
}
\value{
A data frame object that adds base-line adjusted OD and cut-off times
to the input master data frame.
}
\description{
Baseline-adjust raw OD measurements by subtracting
the median OD of the controls at time 0 in a given plate from of all other
measurements and determine the time of transition from exponential to
stationary phase of the growth curve.
}
