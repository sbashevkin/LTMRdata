#' Fill in 0 catches
#'
#' Fill in 0s for samples in which a species was not recorded as caught.
#' @param data The input data, likely the output from \code{\link{LTMRpilot}}, \code{\link{fish}}, or some row-bound combination of the internal package datasets.
#' Must contain at least the SampleID, Taxa, Length_NA_flag (if \code{remove_unmeasured_lengths=TRUE}), and Count columns.
#' @param species Character vector of species to include. Set \code{species=NULL} (the default) to include all species. It is recommended to filter to species of interest using this option.
#' @param remove_unknown_lengths Should samples associated with unknown lengths be removed from the data? Unknown lengths refer
#' to rows with \code{Length_NA_flag=="Unknown length"}, corresponding to Suisun samples where unmeasured fish were not a random
#' sample from the same pool fish were selected to be measured, AND the length range could not be estimated from the comments, or
#' no fish within that size range were measured in that sample. The behavior of this option depends on the \code{univariate} parameter.
#' @param univariate Controls behavior of the \code{remove_unknown_lengths} option. If \code{remove_unknown_lengths=FALSE} this parameter is ignored.
#' Will these data be used for univariate analyses (\code{univariate=TRUE})? Or multi-species analyses (\code{univariate=FALSE})?
#' If univariate, when a \code{Length_NA_flag=="Unknown length"} record is found, all records of that taxa from that sample are removed and no 0s are filled in.
#' In effect, this is transforming those records into missing data. If \code{univariate=FALSE}, when a \code{Length_NA_flag=="Unknown length"} record is found,
#' the entire sample is removed and no 0s are filled in, since accurate community data cannot be confirmed for that sample.
#'
#' @details It is recommended to use the \code{species} parameter to filter the data to the species (singular or plural) of interest, otherwise it will create a gigantic dataset.
#' You can fill in 0s for multiple univariate analyses simultaneously by passing the species of interest to the \code{species} parameter and setting \code{univariate=TRUE}.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @seealso \code{\link{LTMRpilot}}
#'
#' @examples
#'
#' # Using all species with converted lengths
#' Data <- LTMRpilot(convert_lengths=TRUE, remove_unconverted_lengths=TRUE)
#'
#' # For a univariate analysis
#' species <- c("Clupea pallasii", "Morone saxatilis", "Parophrys vetulus", "Sardinops sagax")
#' Data_filled <- zero_fill(Data, species=species, remove_unknown_lengths=TRUE, univariate=TRUE)
#'
#' # For a multivariate analysis
#' Data_filled <- zero_fill(Data, remove_unknown_lengths=TRUE, univariate=FALSE)
#'
#' @export

zero_fill <- function(data, species=NULL, remove_unknown_lengths=TRUE, univariate=TRUE){

  if(!is.null(species)){
    if(!all(species%in%unique(data$Taxa))){
      message(paste0("These species were not present in your data: ", paste(setdiff(species, unique(data$Taxa)), collapse=", ")))
    }
    data <- dplyr::filter(data, .data$Taxa%in%species | is.na(.data$Taxa))
  }

  if(nrow(dplyr::filter(data, is.na(.data$Taxa)))==0){
    stop("There are no rows with NA Taxa, indicating there are no empty samples to fill with 0s.
         Did you accidentally exclude these by filtering by 'Taxa' and not explicitly including is.na(Taxa)?
         It is recommended to use the 'species' parameter to filter by species, rather than filtering the data before using this function.
         If you do filter before using this function, be sure to do something like filter(data, Taxa%in%mytaxa | is.na(Taxa)).")
  }

  if(!univariate & remove_unknown_lengths){
    remove<-data%>%
      dplyr::filter(is.na(.data$Length) & .data$Length_NA_flag=="Unknown length")%>%
      dplyr::pull(.data$SampleID)%>%
      unique()

    data<-data%>%
      dplyr::filter(!.data$SampleID%in%remove)
  }

  data_env<-data%>%
    dplyr::select(-.data$Taxa, -.data$Length, -.data$Count, -.data$Notes_catch, -.data$Length_NA_flag)%>%
    dplyr::distinct()

  if(any(duplicated(data_env$SampleID))){
    stop("Something went wrong and rows were duplicated")
  }

  data<-data%>%
    dplyr::select(.data$SampleID, .data$Taxa, .data$Length, .data$Count, .data$Notes_catch, .data$Length_NA_flag)%>%
    tidyr::complete(.data$SampleID, .data$Taxa, fill=list(Count=0))%>%
    dplyr::left_join(data_env, by="SampleID")

  gc()

  if(univariate & remove_unknown_lengths){
    remove<-data%>%
      dplyr::filter(is.na(.data$Length) & .data$Length_NA_flag=="Unknown length")%>%
      dplyr::select(.data$Taxa, .data$SampleID)%>%
      dplyr::distinct()

    gc()

    data<-data%>%
      dplyr::anti_join(remove, by=c("Taxa", "SampleID"))

    gc()
  }

  data<-data%>%
    dplyr::filter(!is.na(.data$Taxa))

  return(data)
}
