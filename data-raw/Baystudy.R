## code to prepare `Baystudy` dataset goes here

require(readr)
require(dplyr)
require(tidyr)
require(lubridate)
require(readxl)

stations_Baystudy <- read_excel(file.path("data-raw", "Baystudy", "Bay Study_Station Coordinates for Distribution_2018.xlsx"))%>%
  separate(Latitude, into=c("Lat_Deg", "Lat_Min"), sep = "°", convert=T)%>%
  separate(Longitude, into=c("Lon_Deg", "Lon_Min"), sep = "°", convert=T)%>%
  mutate(Latitude=Lat_Deg+Lat_Min/60,
         Longitude=Lon_Deg+Lon_Min/60)%>%
  select(Station, Latitude, Longitude)%>%
  filter(Station!="211E")%>%
  mutate(Station=parse_integer(recode(Station, `211W`="211")))

towcodes_Baystudy <- read_csv(file.path("data-raw", "Baystudy", "TowCodes_Lookup.csv"),
                              col_type=cols_only(TowCode="i", TowDescription="c"))%>%
  rename(Tow=TowCode, TowStatus=TowDescription)

tidecodes_Baystudy <- read_csv(file.path("data-raw", "Baystudy", "TideCodes_LookUp.csv"),
                               col_types=cols_only(Tide="i", Description="c"))

wavecodes_Baystudy <- read_csv(file.path("data-raw", "Baystudy", "WaveCodes_LookUp.csv"),
                               col_types=cols_only(Waves="i", Description="c"))

cloudcovercodes_Baystudy <- read_csv(file.path("data-raw", "Baystudy", "CloudCover_LookUp.csv"),
                               col_types=cols_only(CloudCover="i", Description="c"))

salintemp_Baystudy<-read_csv(file.path("data-raw", "Baystudy", "SalinTemp.csv"),
                             col_types = cols_only(Year="i", Survey="i", Station="i",
                                                   ECSurf="d", ECAvg="d", ECBott="d",
                                                   TempSurf="d", TempAvg="d", TempBott="d"))


boatstation_Baystudy <- read_csv(file.path("data-raw", "Baystudy", "BoatStation.csv"),
                                 col_types = cols_only(Year="i", Survey="i", Station="i",
                                                       Date="c", Depth="d", Secchi="d",
                                                       SubstrateCode="c", Waves="i", CloudCover="i",
                                                       Tide="i", StationComment="c"))%>%
  mutate(Date=parse_date_time(Date, orders="%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
  left_join(tidecodes_Baystudy, by="Tide")%>%
  select(-Tide, -SubstrateCode)%>%
  rename(Tidestation=Description)%>%
  left_join(wavecodes_Baystudy, by="Waves")%>%
  select(-Waves)%>%
  rename(Waves=Description)%>%
  left_join(cloudcovercodes_Baystudy, by="CloudCover")%>%
  select(-CloudCover)%>%
  rename(CloudCover=Description)%>%
  left_join(salintemp_Baystudy, by=c("Year", "Survey", "Station"))%>%
  left_join(stations_Baystudy, by="Station")

boattow_Baystudy<-read_csv(file.path("data-raw", "Baystudy", "BoatTow.csv"),
                           col_types = cols_only(Year="i", Survey="i", Station="i", Net="i",
                                                 Tow="i", Time="c", Bearing="d", Tide="i",
                                                 Direction="i", CatchCode="i", Duration="d",
                                                 StartMeter="d", EndMeter="d", TotalMeter="d",
                                                 StartLong="d", EndLong="d", StartLat="d",
                                                 EndLat="d", Distance="d", TowComment="c"))%>%
  select(-StartLong, -EndLong, -StartLat, -EndLat)%>% # Removing survey lats/longs for now
  mutate(Time=parse_date_time(Time, orders="%m/%d/%Y %H:%M:%S"))%>%
  left_join(tidecodes_Baystudy, by="Tide")%>%
  select(-Tide)%>%
  rename(Method=Net, Tidetow=Description)%>%
  mutate(Method=recode(Method, `1`="Midwater trawl", `2`="Otter Trawl", `3`="EL"))%>%
  filter(Method%in%c("Midwater trawl", "Otter Trawl"))%>%
  left_join(towcodes_Baystudy, by="Tow")

env_Baystudy <- left_join(boattow_Baystudy, boatstation_Baystudy, by=c("Year", "Survey", "Station"))%>%
  mutate(Tide=if_else(is.na(Tidetow), Tidestation, Tidetow),
         Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>%
  select(-Tidestation, -Tidetow, -Time)

rm(tidecodes_Baystudy, wavecodes_Baystudy, cloudcovercodes_Baystudy, towcodes_Baystudy, boattow_Baystudy, boatstation_Baystudy, salintemp_Baystudy, stations_Baystudy)

species_Baystudy <- read_csv(file.path("data-raw", "Baystudy", "SpeciesCodes_Lookup.csv"),
                             col_types=cols_only(AlphaCode="c", CommonName="c"))

catch_Baystudy <- read_csv(file.path("data-raw", "Baystudy", "Fish Catch Data.csv"),
                           col_types=cols_only(Year="i", Survey="i", Station="i",
                                               Net="i", AlphaCode="c",
                                               SizeGroup="i", QtsCaught="d", QtsSubsampled="d",
                                               PlusCount="d"))%>%
  rename(Method=Net)%>%
  mutate(Method=recode(Method, `1`="Midwater trawl", `2`="Otter Trawl", `3`="EL"))%>%
  filter(Method%in%c("Midwater trawl", "Otter Trawl"))%>%
  left_join(species_Baystudy, by="AlphaCode")%>%
  select(-AlphaCode)

length_Baystudy <- read_csv(file.path("data-raw", "Baystudy", "Fish Length Data.csv"),
                            col_types=cols_only(Year="i", Survey="i", Station="i",
                                                Net="i", AlphaCode="c",
                                                SizeGroup="i", Length="d", Frequency="d"))%>%
  rename(Method=Net)%>%
  mutate(Method=recode(Method, `1`="Midwater trawl", `2`="Otter Trawl", `3`="EL"),
         AlphaCode = toupper(AlphaCode))%>%
  filter(Method%in%c("Midwater trawl", "Otter Trawl"))%>%
  left_join(species_Baystudy, by="AlphaCode")%>%
  select(-AlphaCode)

rm(species_Baystudy)

Baystudy<-catch_Baystudy%>%
  full_join(length_Baystudy%>%
               group_by(Year, Survey, Station, Method, SizeGroup, CommonName)%>%
               summarize(TotalMeasured=sum(Frequency))%>%
               ungroup(),
             by=c("Year", "Survey", "Station", "Method", "SizeGroup", "CommonName"))%>%
  filter(!is.na(TotalMeasured) & !is.na(PlusCount))%>%
  mutate(TotalCatch=(TotalMeasured+PlusCount)*(QtsCaught/QtsSubsampled))%>%
  select(-PlusCount, -QtsCaught, -QtsSubsampled)%>%
  right_join(length_Baystudy, by=c("Year", "Survey", "Station", "Method", "SizeGroup", "CommonName"))%>%
  mutate(Frequency = (Frequency/TotalMeasured)*TotalCatch)%>%
  select(-TotalMeasured, -TotalCatch)


usethis::use_data(Baystudy)
