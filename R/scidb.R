#' @import magrittr
#' 

#' @title scidb global variable
#' @name scidb.valid_types
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Compatible scidb to R types
scidb.valid_types <- 
    c("int32", 
      "double", 
      "string")

#' @title scidb global variable
#' @name scidb.env
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Package environment
scidb.env <- new.env()

#' @title scidb global variable
#' @name scidb.path
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Path to iquery binary
scidb.env$scidb.path <- ""

utils::globalVariables(c(".data", "name"))

#' @title scidb base functions
#' @name scidb.set_iquery_path
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Get actual path of iquery binary
#' @param path         Path to iquery binary
#' @return String of path
#' @export
scidb.set_iquery_path <- function(path){
    if (substr(path, nchar(length(path)), nchar(length(path))) != "/")
        path <- paste0(path, "/")
    scidb.env$scidb.path <- path
    invisible(NULL)
}

#' @title scidb base functions
#' @name scidb.iquery_path
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Get actual path of iquery binary
#' @return Path to iquery binary
#' @export
scidb.iquery_path <- function(){
    return(scidb.env$scidb.path)
}

#' @title scidb base functions
#' @name scidb.exec
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Execute AFL instruction
#' @param afl          A valid AFL scidb query
#' @param fetch_data   Logical. Inform if data must be retrieved from \code{afl} query (Default \code{TRUE}).
#' @return Tibble with AFL result
#' @export
scidb.exec <- function(afl, fetch_data = TRUE){
    conn <- pipe(sprintf("%siquery -aq \"%s;\"", 
                         scidb.iquery_path(),
                         afl), open = "r")
    lines <- readLines(conn)
    close(conn)
    
    result <- 
        if (fetch_data){
            lines <- gsub("(\\{(.*)\\} )(.*)", "\\2,\\3", lines)
            lines <- lines[1:(length(lines)-1)]
            lines_split <- strsplit(lines, ",")
            header <- lines_split[[1]]
            data_mask <- lapply(header, function(x) header == x)
            names(data_mask) <- header
            data <- unlist(lines_split[2:length(lines_split)])
            lapply(data_mask, function(x) data[x]) %>% 
                tibble::as_tibble()
        } else {
            lines
        }
    
    return(result)
}

#' @title scidb base functions
#' @name scidb.arrays
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Get all scidb arrays
#' @return Tibble with AFL result
#' @export
scidb.arrays <- function(){
    result <- 
        sprintf("list('arrays');") %>% 
        scidb.exec() %>% 
        dplyr::select("name", "schema")
    return(result)
}

#' @title scidb base functions
#' @name scidb.array
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Get a specific scidb array
#' @param a_name       A valid array name. If \code{NULL} all arrays are returned (Default \code{NULL})
#' @return AFL string command
#' @export
scidb.array <- function(a_name){
    result <- scidb.arrays()
    result <- 
        result %>% 
        dplyr::filter(name == a_name)
    
    if (NROW(result) != 1)
        stop(sprintf("get_array - array '%s' does not exist", a_name))
    
    return(result)
}

#' @title scidb base functions
#' @name scidb.array_schema
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Get a specific scidb array schema
#' @param a_name       A valid array name. If \code{NULL} all arrays are returned (Default \code{NULL}).
#' @param R_types      Logical. Convert scidb types to compatible R types (Default \code{FALSE}).
#' @return AFL string command
#' @export
scidb.array_schema <- function(a_name, R_types = FALSE){
    result <- scidb.array(a_name)$schema
    
    if (R_types)
        result <- .scidb_to_R_types(result)
    
    return(result)
}

#' @title scidb base functions
#' @name scidb.make_schema
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Generate a new schema string
#' @param a_name       A valid array name. If \code{NULL} all arrays are returned (Default \code{NULL}).
#' @param sch_attrs    A list of pairs \code{attr_name="type"}, where \code{"type"} is any scidb type.
#'                     R compatible types: \code{"int32"}, \code{"double"}, and \code{"string"}.
#' @param sch_dim      A list of pairs \code{dim_name=c(from, to, chunck_size, chunk_overlap)}, where
#'                     \code{from}, \code{to}, \code{chunk_size}, and \code{chunk_overlap} are numbers.
#' @return A schema string
#' @export
scidb.make_schema <- function(a_name, sch_attrs = list(attr1 = "int32"), sch_dim = list(dim1 = c(0, 10, 11, 0))){
    
    if (!is.null(sch_attrs) && (is.null(names(sch_attrs)) || any(sapply(names(sch_attrs), nchar) == 0)))
        stop("scidb.make_schema - all schema attributes must be named")
    if (!is.null(sch_dim) && (is.null(names(sch_dim)) || any(sapply(names(sch_dim), nchar) == 0)))
        stop("scidb.make_schema - all schema dimensions must be named")
    if (any(sapply(sch_dim, length) != 4))
        stop("scidb.make_schema - all schema dimensions must have 4 elements")
    
    attrs <- paste(names(sch_attrs), sch_attrs, sep = "=", collapse=", ")
    dimensions <- paste(names(sch_dim), 
                        sapply(sch_dim, function(d) sprintf("%s:%s,%s,%s", d[1], d[2], d[3], d[4])), 
                        sep = "=", collapse = ", ")
    
    result <- sprintf("%s<%s>[%s]", a_name, attrs, dimensions)
    
    return(result)
}
