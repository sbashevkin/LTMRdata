###################################################################
## code to prepare `DJFMP` dataset as prepared by Sam Bashevkin  ##
###################################################################

require(wql)
require(LTMRdata)
require(readr)
require(dplyr)
require(lubridate)
require(hms)
require(stringr)

# downloading data because the dataset is too huge to keep on file

download.file("https://pasta.lternet.edu/package/data/eml/edi/244/5/71c16ead9b8ffa4da7a52da180f601f4", file.path(tempdir(), "DJFMP_1976-2001.csv"), mode="wb",method="libcurl")
download.file("https://pasta.lternet.edu/package/data/eml/edi/244/5/0edf413c39ac8b111a576d894306a60f", file.path(tempdir(), "DJFMP_2002-2020.csv"), mode="wb",method="libcurl")
download.file("https://pasta.lternet.edu/package/data/eml/edi/244/5/a3e94e8f0cf6f675d716151c1ca17b4f", file.path(tempdir(), "DJFMP_BeachSeine_1976-2020.csv"), mode="wb",method="libcurl")


download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.4&entityid=99a038d691f27cd306ff93fdcbc03b77", file.path(tempdir(), "DJFMP_stations.csv"), mode="wb")


# Methods in  metadata say they do not know if their data were corrected for temperature before May 3 or 17 2019
# so I will not use conductivity data before June 2019

DJFMP_stations <- read_csv(file.path(tempdir(), "DJFMP_stations.csv"),
                           col_types = cols_only(StationCode="c", Latitude_location="d", Longitude_location="d")) %>%    # pipeline
  rename(Station=StationCode, Latitude=Latitude_location, Longitude=Longitude_location)

DJFMP <-  bind_rows(
  read_csv(file.path(tempdir(), "DJFMP_1976-2001.csv"),
           col_types = cols_only(StationCode = "c", SampleDate = "c", SampleTime = "c",
                                 TowNumber = "c", MethodCode = "c",
                                 Conductivity = "d", WaterTemperature = "d", Secchi = "d",
                                 Volume = "d", TowDirectionCode = "c",
                                 OrganismCode = "c", ForkLength = "d", Count = "d")),
  read_csv(file.path(tempdir(), "DJFMP_2002-2020.csv"),
           col_types = cols_only(StationCode = "c", SampleDate = "c", SampleTime = "c",
                                 TowNumber = "c", MethodCode = "c",
                                 Conductivity = "d", WaterTemperature = "d", Secchi = "d",
                                 Volume = "d", TowDirectionCode = "c",
                                 OrganismCode = "c", ForkLength = "d", Count = "d")),
  read_csv(file.path(tempdir(), "DJFMP_BeachSeine_1976-2020.csv"),
           col_types = cols_only(StationCode = "c", SampleDate = "c", SampleTime = "c",
                                 TowNumber = "c", MethodCode = "c", SeineDepth = "d",
                                 Conductivity = "d", WaterTemperature = "d", Secchi = "d",
                                 Volume = "d", TowDirectionCode = "c",
                                 OrganismCode = "c", ForkLength = "d", Count = "d")))%>%
  rename(Station = StationCode, Date = SampleDate, Time = SampleTime, Temp_surf = WaterTemperature,
         Method = MethodCode, Tow_volume = Volume, Depth=SeineDepth,
         Tow_direction = TowDirectionCode, Length = ForkLength) %>%
  # convert Secchi to cm
  mutate(Secchi = Secchi*100) %>%
  mutate(Source = "DJFMP",
         Date = parse_date_time(Date, "%Y-%m-%d", tz = "America/Los_Angeles"),
         Time = parse_date_time(Time, "%H:%M:%S", tz = "America/Los_Angeles"),
         Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
         # Removing conductivity data from dates before it was standardized
         Conductivity = if_else(Date<parse_date_time("2019-06-01", "%Y-%m-%d", tz="America/Los_Angeles"), NA_real_, Conductivity),
         Sal_surf = ec2pss(Conductivity/1000, t=25),
         Method = recode(Method, MWTR="Midwater trawl", KDTR="Kodiak trawl", SEIN="Beach seine"),
         Tow_direction = recode(Tow_direction, U="Upstream", D="Downstream", X="Neither"),
         SampleID=paste(Datetime, Station, TowNumber, Method)) %>%
  select(-Time) %>%
  group_by(across(-Count))%>% # Some species are recorded with the same length multiple times
  summarise(Count=sum(Count), .groups="drop")%>%
  group_by(SampleID, OrganismCode)%>%
  mutate(TotalMeasured=sum(Count[which(Length!=0)]),
         Total=sum(Count),
         Count=(Count/TotalMeasured)*Total)%>%
  ungroup()%>%
  filter(Length!=0 | is.na(Length))%>%
  left_join(DJFMP_stations, by = "Station") %>%
  # Add species names
  left_join(Species %>%
              select(USFWS_Code, Taxa) %>%
              filter(!is.na(USFWS_Code)), by=c("OrganismCode"="USFWS_Code")) %>%
  # Remove unneeded variable
  select(-OrganismCode) %>%
  mutate(SampleID=paste(Source, SampleID), # Add variable for unique (across all studies) sampleID
         Length_NA_flag=if_else(is.na(Length), "No fish caught", NA_character_), # Add reasoning for an NA lengths (all "No Fish Caught" for FMWT)
         Taxa=str_remove(Taxa, " \\((.*)"))%>% # Remove life stage info from Taxa names
  select(Source, Station, Latitude, Longitude, Date, Datetime, Depth, SampleID, Method, Sal_surf,
         Temp_surf, Secchi, Tow_volume, Tow_direction, Taxa, Length, Count, Length_NA_flag)

# Save compressed data to /data
usethis::use_data(DJFMP, overwrite=TRUE, compress="xz")

#x = DJFMP[(DJFMP$Taxa == "Lucania goodei"),]

# this line of code was used to test if all fish that had an "organism code" in the DJFMP data
# had a Scientific Name of a fish associated with them. The only NA's in "Taxa" field are when
# there was "no catch" which has the organism code: "NOFISH"
# command below came up with ~35,000 entries; command below that with 0
# djfmp.sppnas = DJFMP[is.na(DJFMP$Taxa),]
# djfmp.sppnas = DJFMP[(is.na(DJFMP$Taxa) & DJFMP$OrganismCode != "NOFISH"),]
# There is no instance where Count exists but Length does not - where is the PLUS COUNT ?
# djfmp.test = DJFMP[(!is.na(DJFMP$Count) & is.na(DJFMP$Length)),]

# DJFMP BEACH SEINE DATA ###########


# # testing - ditch later
# DJFMP2 <- read_csv(file.path(tempdir(), "DJFMP_1976-2001.csv"),
#                   col_types = cols_only(StationCode = "c", SampleDate = "c", SampleTime = "c", flowDebris = "c",
#                                         TowNumber = "c", MethodCode = "c", StartMeter = "d", EndMeter = "d", TotalMeter = "d",
#                                         Conductivity = "d", WaterTemperature = "d", Secchi = "d",
#                                         TowDuration = "d", Volume = "d", TowDirectionCode = "c",
#                                         OrganismCode = "c", ForkLength = "d", Count = "d")) %>%
#   bind_rows(read_csv(file.path(tempdir(), "DJFMP_2002-2020.csv"),
#                      col_types = cols_only(StationCode = "c", SampleDate = "c", SampleTime = "c", flowDebris = "c",
#                                            TowNumber = "c", MethodCode = "c", StartMeter = "d", EndMeter = "d", TotalMeter = "d",
#                                            Conductivity = "d", WaterTemperature = "d", Secchi = "d",
#                                            TowDuration = "d", Volume = "d", TowDirectionCode = "c",
#                                            OrganismCode = "c", ForkLength = "d", Count = "d")))
#
# djfmp.noTowVol = DJFMP2[is.na(DJFMP2$Volume),]
# temps = djfmp.noTowVol[!is.na(djfmp.noTowVol$StartMeter),]
# temps = djfmp.noTowVol[(!is.na(djfmp.noTowVol$Volume) & is.na(djfmp.noTowVol$flowDebris)),]

# Clean up; remove temporary files
rm(DJFMP_stations)

