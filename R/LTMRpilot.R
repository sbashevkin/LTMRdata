#' Integrate datasets for pilot review
#'
#' Integrate FMWT, Suisun, and Bay study datasets for IEP long-term monitoring program pilot review
#' @param quiet Set \code{quiet=TRUE} to suppress the warning message about length data.
#' @param measured_lengths Only include measured lengths. Does not calculate adjusted length frequencies. Will not represent total fish caught.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @seealso \code{\link{Suisun}}, \code{\link{Suisun_measured_lengths}}, \code{\link{FMWT}}, \code{\link{FMWT_measured_lengths}}, \code{\link{Baystudy}}, \code{\link{Baystudy_measured_lengths}}
#'
#' @examples
#' Data <- LTMRpilot()
#'
#' #Only outputting measured lengths, without frequencies adjusted for counts of unmeasured fish
#' Data <- LTMRpilot(measured_lengths=TRUE)
#' @export

LTMRpilot <- function(quiet=FALSE, measured_lengths=FALSE){

  data<-dplyr::bind_rows(LTMRdata::Suisun, LTMRdata::Baystudy, LTMRdata::FMWT)

  if(measured_lengths){

    lengths<-dplyr::bind_rows(LTMRdata::Suisun_measured_lengths, LTMRdata::Baystudy_measured_lengths, LTMRdata::FMWT_measured_lengths)

    data<-lengths%>%
      dplyr::left_join(data%>%
                  dplyr::select(-.data$Length, -.data$Count, -.data$Notes_catch)%>%
                  dplyr::distinct(),
                by=c("SampleID", "Taxa"))

    if(nrow(lengths)!=nrow(data)){
      stop("Something went wrong and rows were duplicated.")
    }

  }

  if(!quiet){
    message("NOTE: Length data are not consistent across studies. Inspect the help files for Suisun, FMWT, and Baystudy before analysing lengths. Set quiet=TRUE to suppress this message.")
  }

  return(data)
}
