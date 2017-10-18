#!/usr/bin/env Rscript

# This is an example of a R script to be called by scidb stream operator

library(scidbquery)
library(massits)

# # load all input args into environment
# args <- scidb.Rscript_params(required_params =
#                                  list(t_start    = "integer",
#                                       t_interval = "integer",
#                                       model_path = "string",
#                                       bands      = "string",
#                                       scale      = "numeric",
#                                       displace   = "numeric",
#                                       names      = "string",
#                                       types      = "string"),
#                              err_file = "~/scidb_stream_error.log")
# 
# # define function that process chunks
# classify <- function(chunk){
#     its.predict <- readRDS(file = args["model_path"])
# 
#     result <-
#         chunk %>%
#         its(col_names = c("x", "y", "t")) %>%
#         its.feat(bands      = args["bands"],
#                  time_break = its.t_break(., args["start_t"], args["interval_t"]),
#                  drop_na    = TRUE) %>%
#         its.feat.apply(function(x) x * args["scale"] + args["displace"]) %>%
#         its.predict(factors = TRUE)
# 
#     result <-
#         result %>%
#         scidb.types_convert(expected_names = args["names"], 
#                             expected_types = args["types"], 
#                             err_file       = "~/scidb_stream_error.log")
# 
#     return(result)
# }
# 
# # classify each chunck
# scidb.map_stream(classify, err_file = "~/scidb_stream_error.log")

# define function that process chunks
classify <- function(chunk){
    result <-
        tibble::tibble(a=1, b=2, c=3, d=as.integer(4), e=5)

    return(result)
}

# classify each chunck
scidb.map_stream(classify, err_file = "~/scidb_stream_error.log")
