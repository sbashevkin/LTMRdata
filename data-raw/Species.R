## code to prepare `Species codes` dataset goes here

require(readr)
require(dplyr)
require(tidyr)

Species <- read_csv(file.path("data-raw", "Species codes.csv"),
                    col_types = cols_only(Baystudy_Code = "c", CommonName = "c", SMF_Code = "c", SKT_Code = "c",
                                          FMWT_Code = "i", TMM_Code = "i", ScientificName = "c", Lifestage = "c", USFWS_Code = "c"))%>%
  mutate(Taxa = if_else(!is.na(Lifestage), paste0(ScientificName, " (", Lifestage, ")"), ScientificName))

usethis::use_data(Species, overwrite=TRUE)
