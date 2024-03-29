% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Fit_splines.R
\name{Fit_splines_single}
\alias{Fit_splines_single}
\title{Fit_splines_single}
\usage{
Fit_splines_single(curves_df, Time_col, OD_col, degrees_freedom)
}
\arguments{
\item{curves_df}{Master data frame containing OD measurements and
Runs and Layout data.}

\item{Time_col}{Variable in curves_df with time points of the curve}

\item{OD_col}{Variable in curves_df with OD measurements of the curve}

\item{degrees_freedom}{Vector of length two with the degrees of freedom
of each of the splines to be fitted.}
}
\value{
PENDING
}
\description{
Fit two splines to a single growth curve with the
degrees of freedom specified by the user. Spline fits can be used to later
detect outliers based on the difference of the fits.
}
