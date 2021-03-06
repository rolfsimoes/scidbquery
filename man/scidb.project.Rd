% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/operators.R
\name{scidb.project}
\alias{scidb.project}
\title{scidb operators functions}
\usage{
scidb.project(a, attr_names)
}
\arguments{
\item{a}{A valid array name or derived AFL command.}

\item{attr_names}{A vector with all attribute names.}
}
\value{
AFL string command
}
\description{
This function wraps the scidb \code{"project"} operator. 
              The operator selects attributes to be returned.
}
\author{
Rolf Simoes, \email{rolf.simoes@inpe.br}
}
