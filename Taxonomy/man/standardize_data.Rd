% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardize_data.R
\name{standardize_data}
\alias{standardize_data}
\title{Simple function for standardizes the survey data}
\usage{
standardize_data(dataset)
}
\arguments{
\item{dataset}{\code{data.frame}\cr
The survey dataframe that needs to be standardized}
}
\value{
\code{dataset}\cr
The z-transformed survey data frame.
}
\description{
This function takes the survey data frame as input and returns a z-transformed survey data frame.
}
\seealso{
Other Helper: \code{\link{clean_data}},
  \code{\link{discard_meaningless_features}},
  \code{\link{discard_nonrelevant_features}},
  \code{\link{exclude_empirical_correlating_features}},
  \code{\link{exclude_meaningless_nonrelevant_features}},
  \code{\link{get_numeric}},
  \code{\link{logical_to_factor}},
  \code{\link{logical_to_numerical}},
  \code{\link{ordered_to_factor}},
  \code{\link{ordered_to_integer}},
  \code{\link{ordered_to_numeric}},
  \code{\link{remove_attributes}}, \code{\link{tblFun}}
}
