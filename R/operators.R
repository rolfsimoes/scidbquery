
#' @title scidb base functions
#' @name scidb.list
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Get all scidb arrays
#' @param element      Type of entity to be listed, one of:
#'                     \code{"aggregates"}, \code{"arrays"}, \code{"functions"}, \code{"instances"}, 
#'                     \code{"libraries"}, \code{"operators"}, \code{"types"}, and \code{"queries"}
#'                     (Default \code{"arrays"}).
#' @return Tibble with AFL result
#' @export
scidb.list <- function(element = "arrays"){
    
    result <- 
        sprintf("list('arrays');", element) %>% 
        scidb.exec()
    
    return(result)
}

#' @title scidb operators functions
#' @name scidb.project
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  This function wraps the scidb \code{"project"} operator. 
#'               The operator selects attributes to be returned.
#' @param a            A valid array name or derived AFL command.
#' @param attr_names   A vector with all attribute names.
#' @return AFL string command
#' @export
scidb.project <- function(a, attr_names){
    
    .bind_attrs_dim(a)
    .check_attrs(a, attr_names, update = TRUE)
    
    result <- 
        sapply(a, function(a){
            sprintf("project(%s, %s)", a, paste(attr_names, collapse = ", "))
        }, USE.NAMES = FALSE)
    
    .copy_attrs(a, result)
    .stamp_class(result, .scidb.class)
    
    return(result)
}

#' @title scidb operators functions
#' @name scidb.between
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  This function wraps the scidb \code{"between"} operator. 
#'               The operator filter dimensions range to be returned.
#' @param a            A valid array name or derived AFL command.
#' @param ...          Pairs of \code{dim_name=c(min, max)} arguments.
#' @param max_len      The max length accepted and used to break apart the informed dimensions ranges
#'                     into small pieces of ranges.
#' @return AFL string command
#' @export
scidb.between <- function(a, ..., max_len){
    
    range <- .range_dimensions(..., max_len = max_len)
    
    result <- 
        sapply(a, function(a){
            sapply(range, function(range){
                sprintf("between(%s, %s)", a, range)
            }, USE.NAMES = FALSE)
        }, USE.NAMES = FALSE)
    
    .stamp_class(result, "scidb_array")
    
    return(result)
}

#' @title scidb operators functions
#' @name scidb.stream
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  This function wraps the scidb \code{"stream"} operator. 
#'               The operator calls a R script and pass as input an array.
#' @param a            A valid array name or derived AFL command.
#' @param script       A R file script to be executed.
#' @param params       Optional. A list of pairs \code{name=value} to be passed as input arguments 
#'                     to the script. The \code{value} argument can be either numeric or string.
#'                     If a vector of those types is passed, each element will be separated by commas.
#' @return AFL string command
#' @export
scidb.stream <- function(a, script, params = NULL){
    #' file_output_fields 
    #' A list of pairs \code{field_name=value}, where \code{value} is any valid 
    #' scidb types compatible with R, i.e. (\code{"int32"}, \code{"double"}, or \code{"string"}).
    # commandArgs() <- function(...){
    #     if (exists())
    # }
    # conn <- source(script)
    file_output_fields <- a
    
    if (!all(file_output_fields %in% .scidb.valid_types))
        stop("scidb.stream - invalid output types")
    if (!is.null(params) && (is.null(names(params)) || any(sapply(names(params), nchar) == 0)))
        stop("scidb.stream - all params must be named")
    if (!is.null(file_output_fields) && (is.null(names(file_output_fields)) || any(sapply(names(file_output_fields), nchar) == 0)))
        stop("scidb.stream - all file output fields must be named")
    
    params <- .script_encode_args(params)
    
    result <- 
        sapply(a, function(a){
            sprintf("stream(%s, 'Rscript %s', 'format=df', 'types=%s', 'names=%s')", 
                    a, 
                    paste(file, paste(names(params), params, sep="=", collapse=" ")), 
                    paste(file_output_fields, collapse = ","),
                    paste(names(file_output_fields), collapse = ","))
        }, USE.NAMES = FALSE)
    
    .stamp_class(result, "scidb_array")
    
    return(result)
}

#' @title scidb operators functions
#' @name scidb.apply
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  This function wraps the scidb \code{"apply"} operator. 
#'               The operator creates new attributes to be returned.
#' @param a            A valid array name or derived AFL command.
#' @param ...          Pairs of \code{attr_name=value} arguments, where \code{value} 
#'                     can be any valid scidb expression.
#' @return AFL string command
#' @export
scidb.apply <- function(a, ...){
    
    dots <- list(...)

    if (!is.null(dots) && (is.null(names(dots)) || any(sapply(names(dots), nchar) == 0)))
        stop("scidb.stream - all params must be named")
    
    result <- 
        sapply(a, function(a){
            sprintf("apply(%s, %s)", a, paste(names(dots), dots, sep=", ", collapse=", "))
        }, USE.NAMES = FALSE)
    
    .stamp_class(result, "scidb_array")
    
    return(result)
}

#' @title scidb operators functions
#' @name scidb.cast
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  This function wraps the scidb \code{"cast"} operator. 
#'               The operator casts the attributes types to be returned.
#' @param a            A valid array name or derived AFL command.
#' @param schema       A valid scidb schema string. Can be generated by \code{scidb.make_schema} 
#'                     function or by \code{scidb.array.schema}. The content syntax is not verified.  
#' @return AFL string command
#' @export
scidb.cast <- function(a, schema){
    
    result <- 
        sapply(a, function(a){
            sprintf("cast(%s, %s)", a, schema)
        }, USE.NAMES = FALSE)
    
    .stamp_class(result, "scidb_array")
    
    return(result)
}

#' @title scidb operators functions
#' @name scidb.store
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  This function wraps the scidb \code{"store"} operator. 
#'               The operator stores the resulting array into a new array name.
#' @param a            A valid array name or derived AFL command.
#' @param a_name       A valid scidb array name string.
#' @return AFL string command
#' @export
scidb.store <- function(a, a_name){
    
    result <- 
        sapply(a, function(a){
            sprintf("store(%s, %s)", a, a_name)
        }, USE.NAMES = FALSE)
    
    .stamp_class(result, "scidb_array")
    
    return(result)
}

#' @title scidb operators functions
#' @name scidb.redimension
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  This function wraps the scidb \code{"redimension"} operator. 
#'               The operator changes the dimensions and attributes of the input array.
#' @param a            A valid array name or derived AFL command.
#' @param schema       A valid scidb schema string. Can be generated by \code{scidb.make_schema} 
#'                     function or by \code{scidb.array.schema}. The content syntax is not verified.  
#' @return AFL string command
#' @export
scidb.redimension <- function(a, schema){
    
    result <- 
        sapply(a, function(a){
            sprintf("redimension(%s, %s)", a, schema)
        }, USE.NAMES = FALSE)
    
    .stamp_class(result, "scidb_array")
    
    return(result)
}
