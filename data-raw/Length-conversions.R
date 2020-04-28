## code to prepare `Length conversions` dataset goes here

require(readxl)

Length_conversions<-read_excel(file.path("data-raw", "Length conversions.xlsx"))

usethis::use_data(Length_conversions, overwrite = TRUE)
