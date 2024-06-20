#' LTMRdata: A package for integrating long-term monitoring datasets for the IEP long-term monitoring program review.
#'
#' This package contains internal datasets and a function (\code{\link{data_integrate}}) to bind them all together.
#'
#'
#' @name LTMRdata
"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
