% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Fit_splines.R
\name{Fit_splines}
\alias{Fit_splines}
\title{Fit_splines}
\usage{
Fit_splines(
  curves_df,
  Time_col = "Time",
  OD_col = "OD",
  degrees_freedom = c(4, 10),
  group_var = "RRPPRCC"
)
}
\arguments{
\item{curves_df}{Master data frame containing OD measurements and
Runs and Layout data.}

\item{Time_col}{Variable in curves_df with time points of the curve}

\item{OD_col}{Variable in curves_df with OD measurements of the curve}

\item{degrees_freedom}{Vector of length two with the degrees of freedom
of each of the splines to be fitted.}

\item{group_var}{Variables in curves_df used to group curves.}
}
\value{
PENDING
}
\description{
Fit two splines to a series of bacterial growth curves with the
degrees of freedom specified by the user. Spline fits can be used to later
detect outliers based on the difference of the fits.
}
