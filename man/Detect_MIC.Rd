% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Detect_MIC.R
\name{Detect_MIC}
\alias{Detect_MIC}
\title{Detect_MIC}
\usage{
Detect_MIC(
  curves_df,
  threshold,
  group_var,
  control_var = Control,
  AUC_var = normAUC,
  concentration_var = Concentration
)
}
\arguments{
\item{curves_df}{Master data frame containing raw OD measurements and
Runs and Layout data.}

\item{threshold}{Area under the curve (AUC) below which a bacterial strain
is considered to be inhibited by a treatment.}

\item{group_var}{Variables in curves_df used to group curves.}

\item{control_var}{Variable in curves_df stating whether a curve corresponds
to a control or a not.}

\item{AUC_var}{Variable in curves_df with AUC data.}

\item{concentration_var}{Variable in curves_df containing concentration data.}
}
\value{
A data frame object with a variable stating whether the MIC of a given
treatment is below ("<"), equal to ("=") or greater than(">") the tested concentration
}
\description{
Calculate minimum inhibitory concentration of a series of bacterial
growth curves
}
