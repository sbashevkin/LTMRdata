library(dplyr)
library(tidyr)
library(tidyselect)
library(stringr)

data_divide<-function(data_path, rda=TRUE){

sources <- c("Baystudy", "Suisun", "FMWT", "SKT", "DJFMP", "EDSM", "TMM", "SLS", "STN")

for (i in seq_along(sources)){
  load(paste0("data/", sources[i],".rda"))
}

load(file.path("data", "Length_conversions.rda"))


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

dat_l <- mget(sources)

res_survey <- lapply(dat_l, divide_survey)
res_fish <- lapply(dat_l, divide_fish)

res_survey <- do.call(dplyr::bind_rows, res_survey)%>%
  dplyr::relocate(any_of(survey_cols))
res_fish <- do.call(dplyr::bind_rows, res_fish)


res_fish <- res_fish %>%
  dplyr::select(tidyselect::any_of(fish_cols)) %>%
  dplyr::group_by(Source) %>%
  tidyr::complete(.data$SampleID, .data$Taxa, fill=list(Count=0))%>%
  dplyr::ungroup()%>%
  dplyr::select(-Source)

res_fish <- res_fish %>%
  dplyr::filter(!is.na(.data$Taxa))

write.csv(res_survey, file.path(data_path, "survey.csv"), row.names = F)
write.csv(res_fish, file.path(data_path, "fish.csv"), row.names = F)
write.csv(Length_conversions, file.path(data_path, "Length_conversions.csv"), row.names = F)

if(rda){
  save(res_survey, res_fish, file=file.path(data_path, "fishsurvey_compressed.rds"), compress = "xz")
}

}
