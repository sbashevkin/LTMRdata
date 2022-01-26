library(dplyr)
library(tidyr)
library(tidyselect)

sources <- c("Baystudy", "Suisun", "FMWT", "SKT", "DJFMP", "EDSM", "TMM", "SLS", "STN")

for (i in seq_along(sources)){
  load(paste0("data/", sources[i],".rda"))
}

divide_survey <- function(table){

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
                   "Temp_surf",
                   "Secchi",
                   "Tow_duration",
                   "Tow_area",
                   "Tow_volume",
                   "Tow_direction")


  survey_info <- table %>%
    select(any_of(survey_cols)) %>%
    distinct()

}
divide_fish <- function(table){

  fish_cols <- c("SampleID",
                 "Source",
                 "Taxa",
                 "Length",
                 "Count",
                 "Length_NA_flag",
                 "Notes_tow" )


  fish_info <- table %>%
    select(any_of(fish_cols)) %>%
    distinct()
}

dat_l <- mget(sources)

res_survey <- lapply(dat_l, divide_survey)
res_fish <- lapply(dat_l, divide_fish)

res_survey <- do.call(bind_rows, res_survey)
res_fish <- do.call(bind_rows, res_fish)


res_fish <- res_fish %>%
  dplyr::select(tidyselect::any_of(c("SampleID", "Taxa", "Length", "Count", "Notes_catch", "Length_NA_flag", "Source"))) %>%
  group_by(Source) %>%
  tidyr::complete(.data$SampleID, .data$Taxa, fill=list(Count=0))

res_fish <- res_fish %>%
  dplyr::filter(!is.na(.data$Taxa))

write.csv(res_survey, "data/merge_split/survey_info.csv", row.names = F)
write.csv(res_fish, "data/merge_split/fish_info.csv", row.names = F)
