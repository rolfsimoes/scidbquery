#' @title Magrittr Pipe
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @description Magrittr compound assignment pipe-operator.
#' @export
#' @param lhs,rhs A visualisation and a function to apply to it
NULL

#' @title scidb global variable
#' @name .scidb.valid_types
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Compatible scidb to R types
.scidb.valid_types <- 
    c("int32", 
      "double", 
      "string")

#' @title scidb global variable
#' @name .scidb.class
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Compatible scidb to R types
.scidb.class <- "scidb_array"

#' @title scidb global variable
#' @name .scidb.class
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Attribute name present in any \code{scidb_array}
.scidb.attrs <- "scidb_attrs"

#' @title scidb global variable
#' @name .scidb.class
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Attribute name present in any \code{scidb_array}
.scidb.dim <- "scidb_dim"

utils::globalVariables(c(".data", "name"))