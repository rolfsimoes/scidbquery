% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/script.R
\name{scidb.types_convert}
\alias{scidb.types_convert}
\title{scidb stream functions}
\usage{
scidb.types_convert(x, expected_names = NULL, expected_types = NULL,
  err_file = NULL)
}
\arguments{
\item{x}{Any tibble data}

\item{expected_names}{A string vector of \code{field_name} containing all expected fields. 
If any field is missing on \code{x} data, an error is throwed and reported. 
If \code{NULL}, no field name is verified (Default \code{NULL}).}

\item{expected_types}{A string vector of valid scidb types containing all expected 
fields' type. The valid types are \code{"int32"}, \code{"double"}, and \code{"string"}.
If any type is incompatible with scidb stream, an error is throwed and reported. 
If \code{NULL}, no type is verified (Default \code{NULL}).}

\item{err_file}{A file path where the script can report any errors. If \code{NULL}, any
error will be reported on \code{stderr} only (Default \code{NULL}).}
}
\value{
A new tibble with all fields corresponding those expected_names and expected_types
}
\description{
This function get all script arguments, decodefies them and return a named list.
}
\author{
Rolf Simoes, \email{rolf.simoes@inpe.br}
}
