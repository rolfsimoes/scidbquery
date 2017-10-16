#' @title scidb stream functions
#' @name scidb.Rscript_params
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  This function get all script arguments, decodefies them and return a named list.
#' @param required_params
#'                     A list of pairs \code{param_name="type"} containing all required 
#'                     parameters and its respective type. The valid types are \code{"int32"},
#'                     \code{"double"}, and \code{"string"}. If any parameter
#'                     is missing or has a wrong type, an error is throwed and reported. 
#'                     If \code{NULL}, no parameter is required (Default \code{NULL}).
#' @param err_file     A file path where the script can report any errors. If \code{NULL}, any
#'                     error will be reported on \code{stderr} only (Default \code{NULL}).
#' @return Named list with all decoded script arguments
#' @export
scidb.Rscript_params <- function(required_params = NULL, err_file = NULL){
    cerr <- NULL
    if (!is.null(err_file))
        cerr <- file(err_file, "at")

    tryCatch({
        result <- .Rscript_decode_args(commandArgs(TRUE))
        
        if (!is.null(required_params) && !all((names(required_params) %in% names(result))))
            stop("scidb.Rscript_params - Not all required params are present.")
        
        return(result)
        
    }, error = function(err){
        if (!is.null(cerr))
            writeLines(paste(date(), err$message, sep = ": "), cerr)
        else
            message(paste(date(), err$message, sep = ": "))
        stop(err$message)
    }, finally = {
        if (!is.null(cerr))
            close(cerr)
    })
    
}

#' @title scidb stream functions
#' @name scidb.types_convert
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  This function get all script arguments, decodefies them and return a named list.
#' @param x            Any tibble data
#' @param expected_names
#'                     A string vector of \code{field_name} containing all expected fields. 
#'                     If any field is missing on \code{x} data, an error is throwed and reported. 
#'                     If \code{NULL}, no field name is verified (Default \code{NULL}).
#' @param expected_types
#'                     A string vector of valid scidb types containing all expected 
#'                     fields' type. The valid types are \code{"int32"}, \code{"double"}, and \code{"string"}.
#'                     If any type is incompatible with scidb stream, an error is throwed and reported. 
#'                     If \code{NULL}, no type is verified (Default \code{NULL}).
#' @param err_file     A file path where the script can report any errors. If \code{NULL}, any
#'                     error will be reported on \code{stderr} only (Default \code{NULL}).
#' @return A new tibble with all fields corresponding those expected_names and expected_types
#' @export
scidb.types_convert <- function(x, expected_names = NULL, expected_types = NULL, err_file = NULL){
    cerr <- NULL
    if (!is.null(err_file))
        cerr <- file(err_file, "at")
    
    tryCatch({
        if (!is.null(expected_names) && !is.null(expected_types) && (length(expected_names) != length(expected_types)))
            stop("scidb.types_convert - expected fields and expected types vectors must have the same length.")
        if (!is.null(expected_types) && (length(grep("(int32|double|string)", expected_types)) != length(expected_types)))
            stop("scidb.types_convert - invalid type informed.")
        if (!is.null(expected_names) && (!all(expected_names %in% names(x))))
            stop("scidb.types_convert - some expected name isn't in data fields.")
        
        if (!is.null(expected_names))
            x <- dplyr::select(x, expected_names)
        
        x_fields <- names(x)
        if (!is.null(expected_types))
            names(expected_types) <- x_fields
        
        for(f in x_fields)
            x[[f]] <- .R_to_scidb_types(x[[f]], expected_types[[f]])
        
        return(x)
        
    }, error = function(err){
        if (!is.null(cerr))
            writeLines(paste(date(), err$message, sep = ": "), cerr)
        else
            message(paste(date(), err$message, sep = ": "))
        stop(err$message)
    }, finally = {
        if (!is.null(cerr))
            close(cerr)
    })
}

#' @title scidb stream functions
#' @name scidb.map_stream
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  This function get all script arguments, decodefies them and return a named list.
#' @param fun          A function to be called to process the input data which comes from scidb.
#'                     The function must have one argument to receive the chunck data.
#'                     The result of this function is returned to the scidb stream operator.
#' @param err_file     A file path where the script can report any errors. If \code{NULL}, any
#'                     error will be reported on \code{stderr} (Default \code{NULL}).
#' @return Named list with all decoded script arguments
#' @export
scidb.map_stream <- function(fun, err_file = NULL){

    cerr <- NULL
    cout <- NULL
    cin  <- NULL

    .open_pipes <- function(){
        envir <- parent.frame()

        envir$cerr <- NULL
        if (!is.null(err_file))
            envir$cerr <- file(err_file, "at")
        envir$cin  <- file("stdin", "rb")
        envir$cout <- pipe("cat", "wb")
    }

    .close_pipes <- function(terminate = FALSE){
        envir <- parent.frame()

        if (!is.null(envir$cerr))
            close(envir$cerr)
        close(envir$cin)
        close(envir$cout)

        if (terminate)
            quit(save = "no")
    }

    .write_output <- function(){
        envir <- parent.frame()

        writeBin(serialize(c(envir$output), NULL, xdr=FALSE), envir$cout)
        flush(envir$cout)
    }

    tryCatch({
        while(TRUE) {
            output <- list()
            input_data <- unserialize(cin)

            if(length(input_data) == 0) {
                .write_output()
                .close_pipes(terminate = TRUE)
            }

            output <- fun(input_data)

            .write_output()
        }
    }, error = function(err){
        if (!is.null(cerr))
            writeLines(paste(date(), err$message, sep = ": "), cerr)
        else
            message(paste(date(), err$message, sep = ": "))
        .close_pipes(terminate = TRUE)
    }, finally = {
        .close_pipes()
    })

    invisible(NULL)
}
