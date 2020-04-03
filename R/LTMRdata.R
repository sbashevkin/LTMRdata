#' LTMRdata: A package for integrating long-term monitoring datasets for the IEP long-term monitoring program review.
#'
#' This package contains internal datasets and a function to bind them all together.
#'
#' @section Data:
#' \itemize{
#'   \item \code{\link{Suisun}}
#'   \item \code{\link{FMWT}}
#'   \item \code{\link{Baystudy}}
#' }
#'
#' @section Functions:
#' \itemize{
#'   \item \code{\link{LTMRpilot}}
#'   }
#'
#' @docType package
#' @name LTMRdata
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
