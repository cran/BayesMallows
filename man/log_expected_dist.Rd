% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{log_expected_dist}
\alias{log_expected_dist}
\title{Compute the logarithm of the expected distance of metrics for a Mallows rank model}
\usage{
log_expected_dist(alpha, n_items, cardinalities, metric)
}
\arguments{
\item{alpha}{The value of the alpha parameter.}

\item{n_items}{Number of items.}

\item{cardinalities}{Number of occurrences for each unique distance.
Applicable for Footrule and Spearman distance.}

\item{metric}{A string. Available options are \code{"ulam"}, \code{"footrule"} and \code{"spearman"}.}
}
\value{
A scalar, the logarithm of the partition function.
}
\description{
Compute the logarithm of the expected distance of metrics for a Mallows rank model
}
\keyword{internal}
