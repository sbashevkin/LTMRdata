## code to prepare `Species codes` dataset goes here

require(readr)
require(dplyr)
require(tidyr)

Species <- read_csv(file.path("data-raw", "Species codes.csv"),
                    col_types = cols_only(Baystudy_Code = "c", CommonName = "c", SMF_Code = "c", SKT_Code = "c",
                                          FMWT_Code = "i", TMM_Code = "i", ScientificName = "c", Lifestage = "c", USFWS_Code = "c"))%>%
  mutate(Taxa = if_else(!is.na(Lifestage), paste0(ScientificName, " (", Lifestage, ")"), ScientificName))

# Binding in the SLS codes
Species <- full_join(Species, read_csv(file.path("data-raw", "SLS", "FishCodes.txt"),
                            col_types =
                              cols(
                                `Common Name` = col_character(),
                                Genus = col_character(),
                                Species = col_character(),
                                Family = col_character(),
                                `Fish Code` = col_integer(),
                                Symbol = col_character(),
                                `TNS Field` = col_character(),
                                `MWT Species Code` = col_double(),
                                `MWT Field` = col_character()
                              )
) %>%
  # Keeping only data rows with fish code from SLS
  filter(!is.na(`Fish Code`)) %>%
  transmute(SLS_Code = `Fish Code`,
            Taxa = case_when(`Fish Code` == 99 ~ "UnID",
                             # Green sturgeon is named Acipenser medirostrus in SLS FishCode
                             `Fish Code` == 23 ~ "Acipenser medirostris",
                             `Fish Code` == 21 ~ "Acipenser",
                             `Fish Code` == 32 ~ "Atheriniformes",
                             `Fish Code` == 46 ~ "Hysterocarpus traskii",
                             `Fish Code` == 52 ~ "Clupea pallasii",
                             `Fish Code` == 60 ~ "Petromyzontiformes",
                             `Fish Code` == 62 ~ "Lampetra ayresii",
                             # In SLS, 72 is listed as Notropis lutrensis, which seems to be the older name
                             `Fish Code` == 72 ~ "Cyprinella lutrensis",
                             `Fish Code` == 75 ~ "Tridentiger",
                             # Might need to change this once Adam updates the ftp database
                             `Fish Code` == 76 ~ "Symphurus atricauda",
                             `Fish Code` == 77 ~ "Micropterus punctulatus",
                             `Fish Code` == 83 ~ "Parophrys vetulus",
                             `Fish Code` == 2813 ~ "Stichaeidae",
                             `Fish Code` == 3127 ~ "Pleuronectidae",
                             # Some of the fish codes above have "Unid" in their name but
                             # case_when will ignore those since priority is given to code appearing first
                             str_detect(`Common Name`, "Unid") ~ Family,
                             # # This is correct though...Change in the 20 mm?
                             # `Fish Code` == 53 ~ "Menidia beryllina"
                             TRUE ~ paste(Genus, Species))),
  by = "Taxa") %>%
  relocate(SLS_Code, .after = TMM_Code)

usethis::use_data(Species, overwrite=TRUE)
