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

utils::globalVariables(c(".data"))

#' @title scidb base functions
#' @name scidb.exec
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Execute AFL instruction
#' @param afl          A valid AFL scidb query
#' @param fetch_data   Logical. Inform if data must be retrieved from \code{afl} query (Default \code{TRUE}).
#' @param ssh_server   String informing a ssh conection, i.e. 'user@server', to run iquery.
#' @return Tibble with AFL result
#' @export
scidb.exec <- function(afl, fetch_data = TRUE, ssh_server = NULL){
    if (fetch_data){
        if (!is.null(ssh_server))
            result <- system(sprintf("ssh s <<'ENDSSH'\n%s\nENDSSH"), 
                             sprintf("iquery -aq \"%s;\"", afl), intern = TRUE)
        else
            result <- system(sprintf("iquery -aq \"%s;\"", afl), intern = TRUE)
        if (length(result) > 1){
            result <- result[1:(length(result)-1)]
            result <- gsub("[{}]", "", result)
            result <- 
                utils::read.table(text = result, sep = ",", header = TRUE) %>% 
                tibble::as.tibble()
        }
    } else {
        if (!is.null(ssh_server))
            result <- system(sprintf("ssh s <<'ENDSSH'\n%s\nENDSSH"), 
                             sprintf("iquery -aqn \"%s;\"", afl), intern = TRUE)
        else
            result <- system(sprintf("iquery -aqn \"%s;\"", afl), intern = TRUE)
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
        dplyr::filter("name" == a_name)
    
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