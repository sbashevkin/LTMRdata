#' Integrate datasets for pilot review
#'
#' Integrate FMWT, Suisun, and Bay study datasets for IEP long-term monitoring program pilot review
#'
#' @seealso \code{\link{Suisun}}, \code{\link{FMWT}}, \code{\link{Baystudy}}
#'
#' @examples
#' Data <- LTMRpilot()
#' @export

LTMRpilot <- function(){
  dplyr::bind_rows(LTMRdata::Suisun, LTMRdata::Baystudy, LTMRdata::FMWT)
}
