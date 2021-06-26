#' Import data
#'
#' Imports data.
#'
#' The default behaviour of this function is to get the path to
#' directory in which the data lives from the option `data.directory`
#' with `getOption("data.directory")`. To set this option use
#' `options(data.directory = "path")` where you replace `path` with
#' the actual path. You can put this in your local .Rproject to have
#' it set when you start R.
#'
#' If the `data.directory` option is not set it will fall back to
#' getting the data from your current working directory.
#'
#' You may also provide the path explicitly as the `data.directory`
#' argument.
#' @param data.name Character. The name of the data file. No default.
#' @param data.directory Character or NULL. The path to the directory
#'     where the data is.  Defaults to NULL, see details.
#' @return A tibble.
#' @examples
#' \dontrun{
#' ## If the option data.directory is not set, this will read the file
#' ## my-data.csv from the current working directory
#' data <- import_data("my-data.csv")
#' 
#' ## Read the file my-data.csv from the directory data in the current
#' ## working directory
#' options(data.directory = "data/")
#' data <- import_data("my-data.csv")
#'
#' ## Read the file my-data.csv from the directory ~/data
#' options(data.directory = "~/data/")
#' data <- import_data("my-data.csv")
#'
#' ## Or
#' data <- import_data("my-data.csv", data.directory = "~/data/")
#' }
#' @export

import_data <- function(data.name, data.directory = "C:/Users/DELL/Desktop/PhD/Study_IV/") {
    ## Check arguments
    assert_that(is.character(data.name), length(data.name) == 1)
    assert_that(is.null(data.directory) || (is.character(data.directory) & length(data.directory) == 1),
                msg = "data.directory has to be NULL or a character vector of length 1")
    ## Import data
    if (is.null(data.directory))
        data.directory <- getOption("data.directory")
    full.path <- paste0(data.directory, data.name)
    assert_that(file.exists(full.path), msg = paste0("The file ", full.path, " does not exist"))
    data <- rio::import(full.path) %>% as_tibble()
    message(crayon::green(paste0("Data with ", nrow(data), " rows and ",
                                 ncol(data), " columns was successfully imported!")))
    
     return (data)
}
