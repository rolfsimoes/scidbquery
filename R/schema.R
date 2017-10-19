#' @title scidb base functions
#' @name schema
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Generate a new schema string
#' @param a_name       A valid array name. If \code{NULL} all arrays are returned (Default \code{NULL}).
#' @param sch_attrs    A list of pairs \code{attr_name="type"}, where \code{"type"} is any scidb type.
#'                     R compatible types: \code{"int32"}, \code{"double"}, and \code{"string"}.
#' @param sch_dim      A list of pairs \code{dim_name=c(from, to, chunck_size, chunk_overlap)}, where
#'                     \code{from}, \code{to}, \code{chunk_size}, and \code{chunk_overlap} are numbers.
#' @return A schema string
#' @export
schema <- function(a_name, sch_attrs = list(attr1 = "int32"), sch_dim = list(dim1 = c(0, 10, 11, 0))){
    
    if (!is.null(sch_attrs) && (is.null(names(sch_attrs)) || any(sapply(names(sch_attrs), nchar) == 0)))
        stop("scidb.make_schema - all schema attributes must be named")
    if (!is.null(sch_dim) && (is.null(names(sch_dim)) || any(sapply(names(sch_dim), nchar) == 0)))
        stop("scidb.make_schema - all schema dimensions must be named")
    if (any(sapply(sch_dim, length) != 4))
        stop("scidb.make_schema - all schema dimensions must have 4 elements")
    
    attrs <- paste(names(sch_attrs), sch_attrs, sep = ":", collapse=", ")
    dimensions <- paste(names(sch_dim), 
                        sapply(sch_dim, function(d) sprintf("%s:%s,%s,%s", d[1], d[2], d[3], d[4])), 
                        sep = "=", collapse = ", ")
    
    result <- sprintf("%s<%s>[%s]", a_name, attrs, dimensions)
    
    return(result)
}

#' @title scidb base functions
#' @name schema.array
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Generate a new schema string
#' @param a_name       A valid array name. If \code{NULL} all arrays are returned (Default \code{NULL}).
#' @return A schema string
#' @export
schema.array <- function(a_name){
    array.tb <- scidb.list() %>% 
        dplyr::filter(name %in% a_name)
    
    if (nrow(array.tb) == 0)
        stop("schema.array - array does not exist.")
    
    result <- array.tb$schema
    
    return(result)
}

#' @title scidb base functions
#' @name schema.attrs
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Generate a new schema string
#' @param schema       A scidb schema string.
#' @return A list with all array dimensions and its range and chunck propoerties.
#' @export
schema.attrs <- function(schema){
    schema <- gsub(" ", "", schema)
    attr_str <- gsub("<(.+)>\\[(.+)\\]", "\\1", schema)
    attr_str <- strsplit(attr_str, ",")
    attr_names <- lapply(attr_str, function(x) gsub("([^:, ]+):([^:, ]+),?", "\\1", x))[[1]]
    attr_values <- lapply(attr_str, function(x) gsub("([^:, ]+):([^:, ]+),?", "\\2", x))[[1]]
    result <- sapply(attr_values, strsplit, split = "[:,]")
    names(result) <- attr_names
    return(result)
}

#' @title scidb base functions
#' @name schema.dim
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Generate a new schema string
#' @param schema       A scidb schema string.
#' @return A list with all array dimensions and its range and chunck propoerties.
#' @export
schema.dim <- function(schema){
    schema <- gsub(" ", "", schema)
    dim_str <- gsub("<(.+)>\\[(.+)\\]", "\\2", schema)
    dim_str <- regmatches(dim_str, gregexpr("([^ ,=]+)=([0-9]+:[0-9]+,[0-9]+,[0-9]+)", dim_str))
    dim_names <- lapply(dim_str, function(x) gsub("([^ ,=]+)=([0-9]+:[0-9]+,[0-9]+,[0-9]+)", "\\1", x))[[1]]
    dim_values <- lapply(dim_str, function(x) gsub("([^ ,=]+)=([0-9]+:[0-9]+,[0-9]+,[0-9]+)", "\\2", x))[[1]]
    result <- sapply(dim_values, strsplit, split = "[:,]")
    names(result) <- dim_names
    return(result)
}
