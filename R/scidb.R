#' @title scidb global variable
#' @name scidb.env
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Package environment
scidb.env <- new.env()

#' @title scidb global variable
#' @name scidb.path
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Path to iquery binary
scidb.env$scidb.path <- "iquery"

#' @title scidb base functions
#' @name scidb.set_iquery_path
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Get actual path of iquery binary
#' @param path         Path to iquery binary
#' @return String of path
#' @export
scidb.set_iquery_path <- function(path){
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
    
    # create a pipe calling iquery and passing to it an
    # AFL command
    conn <- pipe(sprintf("%s -aq \"%s;\"", 
                         scidb.iquery_path(),
                         afl), open = "r")
    lines <- readLines(conn)
    close(conn)
    
    result <- 
        if (fetch_data){
            # process iquery result in a fast way
            # remove brackets "{...} ..." 
            lines <- gsub("(\\{(.*)\\} )(.*)", "\\2,\\3", lines)
            
            # remove last line
            lines <- lines[1:(length(lines)-1)]
            
            # split string by all commas
            lines_split <- strsplit(lines, ",")
            
            # first vector is header
            header <- lines_split[[1]]
            
            # data_mask to compose final result
            # data_mask is an array of Logical values used to mask
            # the value of one field.
            data_mask <- lapply(header, function(x) header == x)
            names(data_mask) <- header
            
            # transform all data into unique array
            # and apply data_mask to select the right values
            # to the right field.
            data <- unlist(lines_split[2:length(lines_split)])
            
            # transform as a tibble
            lapply(data_mask, function(x) data[x]) %>% 
                tibble::as_tibble()
        } else {
            lines
        }
    
    return(result)
}
