#' Integrate datasets for pilot review
#'
#' Integrate FMWT, Suisun, and Bay study datasets for IEP long-term monitoring program pilot review
#' @param quiet Set \code{quiet=TRUE} to suppress the warning message about length data.
#' @param convert_lengths Should all species with conversion equations have their lengths converted to fork length (or total length if no fork)?
#' @param remove_unconverted_lengths Should all species without a conversion equation be removed? Ignored if \code{convert_lengths=FALSE}.
#' @param measured_lengths Only include measured lengths. Does not calculate adjusted length frequencies. Will not represent total fish caught.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @seealso \code{\link{Suisun}}, \code{\link{Suisun_measured_lengths}}, \code{\link{FMWT}}, \code{\link{FMWT_measured_lengths}}, \code{\link{Baystudy}}, \code{\link{Baystudy_measured_lengths}}
#'
#' @examples
#' # All data, no length conversions
#' Data <- LTMRpilot(convert_lengths=FALSE)
#'
#' # All data, convert lengths to Fork length (or Total length if not fork)
#' # but retain all data, even those that could not be converted
#' Data <- LTMRpilot(convert_lengths=TRUE, remove_unconverted_lengths=TRUE)
#'
#'
#' # Only outputting measured lengths, without frequencies adjusted for counts of unmeasured fish,
#' # with all lengths converted and only species with converted lengths returned
#' Data <- LTMRpilot(measured_lengths=TRUE, convert_lengths=TRUE, remove_unconverted_lengths=TRUE)
#' @export

LTMRpilot <- function(quiet=FALSE,
                      convert_lengths=TRUE,
                      remove_unconverted_lengths=TRUE,
                      measured_lengths=FALSE){

  data<-dplyr::bind_rows(LTMRdata::Suisun, LTMRdata::Baystudy, LTMRdata::FMWT)

  if(measured_lengths){

    lengths<-dplyr::bind_rows(LTMRdata::Suisun_measured_lengths, LTMRdata::Baystudy_measured_lengths, LTMRdata::FMWT_measured_lengths)

    data<-lengths%>%
      dplyr::left_join(data%>%
                  dplyr::select(-.data$Length, -.data$Count, -.data$Notes_catch, -.data$Length_NA_flag)%>%
                  dplyr::distinct(),
                by=c("SampleID", "Taxa"))

    if(nrow(lengths)!=nrow(data)){
      stop("Something went wrong and rows were duplicated.")
    }

  }

  if(convert_lengths){
    data<-dplyr::mutate(data, Species=stringr::str_remove(.data$Taxa, " \\((.*)"))
    Length_conversions<-LTMRdata::Length_conversions
    if(remove_unconverted_lengths){
      data<-dplyr::filter(data, .data$Species%in%unique(Length_conversions$Species))
    }
    data<-data%>%
      dplyr::left_join(Length_conversions, by="Species")%>%
      dplyr::mutate(Length=dplyr::if_else(.data$Source=="Suisun" & .data$Species%in%unique(Length_conversions$Species), .data$Intercept+.data$Slope*.data$Length, .data$Length))%>%
      dplyr::select(-.data$Intercept, -.data$Slope, -.data$Species)
  }

  if(!quiet){
    if(!convert_lengths){
      message("NOTE: Length data are not consistent across studies. Inspect the help files for Suisun, FMWT, and Baystudy before analysing lengths. Set quiet=TRUE to suppress this message.")
    } else{
      if(!remove_unconverted_lengths){
        message(paste("NOTE: Length data are not entirely consistent across studies. Lengths have only been converted to consistent units for:", paste(unique(LTMRdata::Length_conversions$Species), collapse = ", ")))
      }
    }
  }

  return(data)
}
