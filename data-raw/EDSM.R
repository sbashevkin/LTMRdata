
###################################################################
## code to prepare `EDSM` dataset as prepared by Sam Bashevkin   ##
###################################################################

require(wql)
require(LTMRdata)
require(readr)
require(dplyr)
require(lubridate)
require(hms)

# downloading data because the dataset is too huge to keep on file

download.file("https://pasta.lternet.edu/package/data/eml/edi/415/5/d468c513fa69c4fc6ddc02e443785f28", file.path(tempdir(), "EDSM_20mm.csv"), mode="wb",method="libcurl")
download.file("https://pasta.lternet.edu/package/data/eml/edi/415/5/4d7de6f0a38eff744a009a92083d37ae", file.path(tempdir(), "EDSM_KDTR.csv"), mode="wb",method="libcurl")


EDSM.20mm <- read_csv(file.path(tempdir(), "EDSM_20mm.csv"),
                  col_types = cols_only(Station = "c", Date = "c", Time = "c", Tide = "c",
                                        StartLong = "d", StopLong = "d", StartLat = "d", StopLat = "d",
                                        TopEC = "d", TopTemp = "d", Scchi = "d", Depth = "d",
                                        Dur = "d", Volume = "d", Dir = "c", GearType = "c",
                                        OrganismCode = "c", ForkLength = "d", SumOfCatchCount = "d")) %>%
  # Station might need species attention because after July 1 of some year, also includes sample year & week
  rename(Temp_surf = TopTemp, Tow_duration = Dur, Tow_volume = Volume, Method = GearType, Secchi = Scchi,
         Tow_direction = Dir, Length = ForkLength, Conductivity = TopEC, Count = SumOfCatchCount) %>%
  # convert Secchi to cm
  mutate(Secchi = Secchi*100) %>%
  mutate(Source = "EDSM",
         Date = parse_date_time(Date, "%Y-%m-%d", tz = "America/Los_Angeles"),
         Time = parse_date_time(Time, "%H:%M:%S", tz = "America/Los_Angeles"),
         Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
         # Removing conductivity data from dates before it was standardized
         # Conductivity = if_else(Date<parse_date_time("2019-06-01", "%Y-%m-%d", tz="America/Los_Angeles"), NA_real_, Conductivity),
         # Method = if_else(Method == "MWTR", "Midwater trawl", "Kodiak trawl"),
         Latitude = (StartLat + StopLat)/2, Longitude = (StartLong + StopLong)/2,
         Tow_direction = if_else(Tow_direction == "U", "Upstream", if_else(Tow_direction == "D", "Downstream", "NA"))) %>%
  mutate(Sal_surf = ec2pss(Conductivity/1000, t=25)) %>%
  select(-Time) %>%
  # distinct(Station, Datetime, .keep_all = TRUE) %>%
  # left_join(DJFMP_stations, by = "Station") %>%
  # Add species names
  left_join(Species %>% select(OrganismCode = USFWS_Code, Taxa) %>% filter(!is.na(OrganismCode)), by="OrganismCode") %>%
  mutate(Survey = month(Date)) %>%
  # Remove unneeded variable
  select(-StartLat, -StartLong, -StopLat, -StopLong) %>%
  mutate(SampleID = 1:nrow(.)) %>%
  select(Source, Station, Latitude, Longitude, Date, Datetime, Survey, Depth, SampleID, Method, Tide,
         Sal_surf, Temp_surf, Secchi, Tow_volume, Tow_direction, OrganismCode, Taxa, Length, Count)

# which OrganismCodes are do not have a translation in the species file?
unique(EDSM.20mm %>% filter(is.na(EDSM.20mm$Taxa)) %>% select(OrganismCode, Taxa))

# Save compressed data to /data
usethis::use_data(EDSM.20mm, overwrite=TRUE)

EDSM.KDTR <- read_csv(file.path(tempdir(), "EDSM_KDTR.csv"),
                      col_types = cols_only(Station = "c", Date = "c", Time = "c", Tide = "c",
                                            StartLong = "d", StopLong = "d", StartLat = "d", StopLat = "d",
                                            EC = "d", Temp = "d", Scchi = "d", StartDepth = "d",
                                            Dur = "d", Volume = "d", Dir = "c", GearType = "c",
                                            OrganismCode = "c", ForkLength = "d", SumOfCatchCount = "d")) %>%
  # Station might need species attention because after July 1 of some year, also includes sample year & week
  rename(Temp_surf = Temp, Tow_duration = Dur, Tow_volume = Volume, Method = GearType, Secchi = Scchi, Depth = StartDepth,
         Tow_direction = Dir, Length = ForkLength, Conductivity = EC, Count = SumOfCatchCount) %>%
  # convert Secchi to cm
  mutate(Secchi = Secchi*100) %>%
  mutate(Source = "EDSM",
         Date = parse_date_time(Date, "%Y-%m-%d", tz = "America/Los_Angeles"),
         Time = parse_date_time(Time, "%H:%M:%S", tz = "America/Los_Angeles"),
         Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
         # Removing conductivity data from dates before it was standardized
         # Conductivity = if_else(Date<parse_date_time("2019-06-01", "%Y-%m-%d", tz="America/Los_Angeles"), NA_real_, Conductivity),
         # Method = if_else(Method == "MWTR", "Midwater trawl", "Kodiak trawl"),
         Latitude = (StartLat + StopLat)/2, Longitude = (StartLong + StopLong)/2,
         Tow_direction = if_else(Tow_direction == "U", "Upstream", if_else(Tow_direction == "D", "Downstream", "NA"))) %>%
  mutate(Sal_surf = ec2pss(Conductivity/1000, t = 25)) %>%
  select(-Time) %>%
  # distinct(Station, Datetime, .keep_all = TRUE) %>%
  # left_join(DJFMP_stations, by = "Station") %>%
  # Add species names
  left_join(Species %>% select(OrganismCode = USFWS_Code, Taxa) %>% filter(!is.na(OrganismCode)), by="OrganismCode") %>%
  mutate(Survey = month(Date)) %>%
  # Remove unneeded variable
  select(-StartLat, -StartLong, -StopLat, -StopLong) %>%
  mutate(SampleID = 1:nrow(.)) %>%
  select(Source, Station, Latitude, Longitude, Date, Datetime, Survey, Depth, SampleID, Method, Tide,
         Sal_surf, Temp_surf, Secchi, Tow_volume, Tow_direction, OrganismCode, Taxa, Length, Count)

# which OrganismCodes are do not have a translation in the species file?
unique(EDSM.KDTR %>% filter(is.na(EDSM.KDTR$Taxa)) %>% select(OrganismCode, Taxa))

# Save compressed data to /data
usethis::use_data(EDSM.KDTR, overwrite=TRUE)
