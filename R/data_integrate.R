#' Integrate fish data
#'
#' Integrate datasets, add zeroes, divide into fish and survey tables, and export to csv and/or rda.
#'
#' @param data_path Path to the folder where you wish the csv/rda files to be saved.
#' @param sources Character vector of data sources to include
#' @param rda Logical. Should the fish and survey tables also be saved as a combined .rda file?
#' @param write Logical. Should the files be written to disk, or would you just like them returned as an R object? Defaults to TRUE.
#' @param quiet Logical. Set to TRUE if you wish to hide all status messages.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return Invisbly returns a list with the survey and fish tables.
#'
#' @examples
#' \dontrun{
#' data_integrate(data_path=file.path("data-raw", "EDI", "data_objects"))
#' @export

data_integrate<-function(data_path,
                         sources = c("Baystudy", "Suisun", "FMWT", "SKT", "DJFMP", "EDSM", "TMM", "SLS", "STN"),
                         rda=TRUE,
                         write=TRUE,
                         quiet=FALSE){

  survey_cols <- c("Source",
                   "Station",
                   "Latitude",
                   "Longitude",
                   "Date",
                   "Datetime",
                   "Survey",
                   "Depth",
                   "SampleID",
                   "Method",
                   "Tide",
                   "Sal_surf",
                   "Sal_bot",
                   "Temp_surf",
                   "Secchi",
                   "Secchi_estimated",
                   "Tow_duration",
                   "Tow_area",
                   "Tow_volume",
                   "Cable_length",
                   "Tow_direction",
                   "Notes_tow",
                   "Notes_flowmeter")

  fish_cols <- c("SampleID",
                 "Source",
                 "Taxa",
                 "Length",
                 "Count",
                 "Notes_catch" )

  divide_survey <- function(table){

    survey_info <- table %>%
      dplyr::select(tidyselect::any_of(survey_cols)) %>%
      dplyr::distinct()

  }
  divide_fish <- function(table){

    fish_info <- table %>%
      dplyr::select(tidyselect::any_of(fish_cols)) %>%
      dplyr::distinct()
  }

  utils::data(list=sources, envir=environment(), package="LTMRdata")
  dat_l <- mget(sources)

  if(!quiet){cat("Data loaded")}

  res_survey <- lapply(dat_l, divide_survey)

  res_survey <- do.call(dplyr::bind_rows, res_survey)%>%
    dplyr::relocate(tidyselect::any_of(survey_cols))

  if(!quiet){cat("\n\nSurvey table finished")}

  res_fish <- lapply(dat_l, divide_fish)
  res_fish <- do.call(dplyr::bind_rows, res_fish)

  if(!quiet){cat("\n\nFish table extracted")}

  res_fish <- res_fish %>%
    dplyr::select(tidyselect::any_of(fish_cols)) %>%
    dplyr::group_by(.data$Source) %>%
    tidyr::complete(.data$SampleID, .data$Taxa, fill=list(Count=0))%>%
    dplyr::ungroup()%>%
    dplyr::select(-.data$Source)


  res_fish <- res_fish %>%
    dplyr::filter(!is.na(.data$Taxa))

  if(!quiet){
    cat("\n\nFish table finished")
    cat("\n\nWriting csvs...")
  }
  if(write){
    utils::write.csv(LTMRdata::Length_conversions, file.path(data_path, "Length_conversions.csv"), row.names = F)

    if(rda){
      if(!quiet){cat("\n\nWriting rda...")}
      save(res_survey, res_fish, file=file.path(data_path, "fishsurvey_compressed.rda"), compress = "xz")
    }else{
      utils::write.csv(res_survey, file.path(data_path, "survey.csv"), row.names = F)
      utils::write.csv(res_fish, file.path(data_path, "fish.csv"), row.names = F)
    }
  }

  message(paste("\n\nProduced with LTMRdata version", utils::packageVersion('LTMRdata')))

  out<-list(survey=res_survey, fish=res_fish, Length_conversions=LTMRdata::Length_conversions)

  return(invisible(out))

}
