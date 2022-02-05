#' Integrate fish datasets
#'
#' Integrates your desired fish datasets according to a number of user-specified parameters
#' @param sources Which data sources would you like included? Options include: "Baystudy", "Suisun", "FMWT", "SKT", "DJFMP", "EDSM", "TMM", "SLS", and "STN".
#' @param zero_fill Should zeros be filled in for samples in which a species was not recorded as caught, using the \code{\link{zero_fill}} function. The \code{remove_unknown_lengths} and \code{univariate} parameters control the behavior of this functionality and these parameters are passed to the \code{\link{zero_fill}} function.
#' @inherit LTMRpilot
#' @inherit zero_fill
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' data<-fish(c("Baystudy", "Suisun", "FMWT", "SKT", "DJFMP", "EDSM"),
#' size_cutoff=40)
#' }
#' @export

fish<-function(sources,
               species=NULL,
               convert_lengths=TRUE,
               size_cutoff=NULL,
               zero_fill=TRUE,
               remove_unknown_lengths=TRUE,
               univariate=TRUE,
               quiet=FALSE){

  if(is.null(sources) | !all(sources%in%c("Baystudy", "Suisun", "FMWT", "SKT", "DJFMP", "EDSM", "TMM", "SLS", "STN"))){
    stop("sources must contain some of the following options: 'Baystudy', 'Suisun', 'FMWT', 'SKT', 'DJFMP', 'EDSM', 'TMM', 'SLS', 'STN'")
  }

  data(list=sources, envir=environment(), package="LTMRdata")
  data<-dplyr::bind_rows(mget(sources))
  rm(list=sources, envir = environment())
  gc()

  if(convert_lengths & "Suisun"%in%sources){
    Length_conversions<-LTMRdata::Length_conversions

    if(any(Length_conversions$Species%in%species) | is.null(species)){
    data<-data%>%
      dplyr::left_join(Length_conversions, by=c("Taxa" = "Species"))%>%
      dplyr::mutate(Length=dplyr::if_else(.data$Source=="Suisun" & .data$Taxa%in%unique(Length_conversions$Species), .data$Intercept+.data$Slope*.data$Length, .data$Length))%>%
      dplyr::select(-.data$Intercept, -.data$Slope)
    }
  }

  if(!is.null(size_cutoff)){
    data<-dplyr::filter(data, is.na(.data$Length) | .data$Length>=size_cutoff)
  }

  if(!quiet){
    if(!convert_lengths & "Suisun"%in%sources){
      message("NOTE: Length data are not consistent across studies. Inspect the help files for Suisun, FMWT, and Baystudy before analysing lengths. Set quiet=TRUE to suppress this message.")
    } else{
      if(convert_lengths & any(unique(data$Taxa %in% unique(LTMRdata::Length_conversions$Species))) & "Suisun"%in%sources){
        message(paste("NOTE: Length data are not entirely consistent across studies. Lengths have only been converted to consistent units for:", paste(unique(LTMRdata::Length_conversions$Species), collapse = ", ")))
      }
    }
  }

  if(zero_fill){
    data<-zero_fill(data, species=species, remove_unknown_lengths=remove_unknown_lengths, univariate=univariate)
  }else{
    if(!is.null(species)){
      if(!all(species%in%unique(data$Taxa))){
        message(paste0("These species were not present in your data: ", paste(setdiff(species, unique(data$Taxa)), collapse=", ")))
      }
      data <- dplyr::filter(data, .data$Taxa%in%species | is.na(.data$Taxa))
    }
  }

  return(data)
}
