#' Complete data on births
#'
#' The complete data file from the CDC is available through the package, as is a random
#' sample of size 1,000,000. These have not been included in the \code{data()} system
#' because they are slow to read in (tens of seconds).
#'
#' To read them, follow the commands in the examples. \code{Natality_2014} will be
#' created by the first command: it is the complete data with 3,998,175 cases.
#' The second command generates \code{Natality_2014_1000k}, a random sample from
#' \code{Natality_2014} of size 1,000,000.
#'
#' @name Larger_natality_data_files
#'
#' @examples
#' load(system.file("Natality_2014.rda", package = "natality2014"))
#' load(system.file("Natality_2014_1000k.rda", package = "natality2014"))

NULL
