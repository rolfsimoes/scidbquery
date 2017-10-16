#' @title scidb utilities functions
#' @name .range_dimensions
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Return a set of range of dimensions breaked or not according to \code{max_len} parameter.
#' @param ...          Pairs of \code{dim_name=c(min, max)} arguments.
#' @param max_len      The max length accepted and used to break apart the informed dimensions ranges
#'                     into small pieces of ranges.
#' @return Tibble with all ranges by dimension
.range_dimensions <- function(..., max_len){
    dots = list(...)
    
    result <- 
        lapply(dots, function(dim){
            breaks <- round(seq.int(from=min(dim), to=max(dim), length.out = ceiling(diff(dim) / max_len) + 1))
            purrr::map2(breaks[1:(length(breaks)-1)], breaks[-1:0], function(from, to) c(from, to))
        })
    
    result <- 
        result %>% 
        expand.grid() %>% 
        tibble::as.tibble()
    
    result <- 
        result %>% 
        dplyr::rowwise() %>% 
        dplyr::do(.data %>% (function(row.tb){
            min_range <- 
                row.tb %>% 
                purrr::map(function(dim) min(dim)) %>% 
                paste(collapse = ", ")
            max_range <- 
                row.tb %>% 
                purrr::map(function(dim) max(dim)) %>% 
                paste(collapse = ", ")
            result <- 
                tibble::tibble(range=paste(min_range, max_range, sep = ", "))
            return(result)
        }))
    
    return(result$range)
}

#' @title scidb utilities functions
#' @name .scidb_to_R_types
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Convert scidb types to types compatible with R
#' @param schema       A valid array schema returned by \code{scidb.array}
#' @return Schema string
.scidb_to_R_types <- function(schema){
    schema <- gsub("(bool|char|int8|int16|int32|uint8|uint16|uint32)", "int32", schema)
    schema <- gsub("(datetime|datetimetz|double|float|int64|uint64)", "double", schema)
    schema <- gsub("(string)", "string", schema)
    return(schema)
}

#' @title scidb utilities functions
#' @name .R_to_scidb_types
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Convert R types to types compatible with scidb
#' @param x            Any vector
#' @param expected_type
#'                     A scidb type name string to convert \code{x} vector. If \code{NULL} is
#'                     informed, the default conversion is done.
#' @return Schema string
.R_to_scidb_types <- function(x, expected_type){
    
    if (!is.null(expected_type)){
        if (length(grep("(int32)", expected_type)) > 0){
            if (is.logical(x) || is.numeric(x) || is.factor(x))
                return(as.integer(x))
            stop("Invalid conversion from non-numeric to numeric type.")
        }
        else if (length(grep("(double)", expected_type)) > 0){
            if (is.logical(x) || is.numeric(x) || is.factor(x))
                return(as.numeric(x))
            stop("Invalid conversion from non-numeric to numeric type.")
        }
        else if (length(grep("(string)", expected_type)) > 0)
            return(as.character(x))
        else
            stop(sprintf("`%s` is an invalid expected type.", expected_type))
    }
    
    x_type <- class(x)
    if (length(grep("(integer|numeric|string)", x_type)) > 0)
        return(x)
    if (length(grep("(logical|factor)", x_type)) > 0)
        return(as.integer(x))
    else if (length(grep("(POSIXct|POSIXt)", x_type)) > 0)
        return(as.character(x))
    else
        stop("The type of the vector is incompatible with scidb stream.")
}

#' @title scidb utilities functions
#' @name .Rscript_encode_args
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Encode Rscript arguments to be passed to R scripts
#' @param args          A string vector of argument values.
#' @return Argument vector
.Rscript_encode_args <- function(args){
    args <- sapply(args, function(p){
        p <- gsub(" ", "`spc`", p)
        p <- gsub(" ", "`tab`", p)
        p <- gsub(" ", "`lf`", p)
        p <- paste0(p, collapse = ",")
        return(p)
    })
    return(args)
}

#' @title scidb utilities functions
#' @name .Rscript_decode_args
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Encode Rscript arguments to be passed to R scripts
#' @param args          A string vector of argument values.
#' @return A named list with argument values.
.Rscript_decode_args <- function(args){
    args <- sapply(args, function(arg){
        arg <- gsub("`spc`", " ", arg)
        arg <- gsub("`tab`", "\t", arg)
        arg <- gsub("`lf`", "\n", arg)
        return(arg)
    })
    args_values <- lapply(args, function(arg){
        arg <- unlist(strsplit(arg, "="))
        arg <- unlist(strsplit(arg[2], ","))
        return(arg)
    })
    args_names <- sapply(args, function(arg){
        arg <- unlist(strsplit(arg, "="))
        return(arg[1])
    })
    names(args_values) <- args_names
    return(args_values)
}
