## code to prepare `Baystudy` dataset goes here

require(readr)
library(wql)
require(dplyr)
require(tidyr)
require(lubridate)
require(readxl)

stations_baystudy <- read_excel(file.path("data-raw", "Baystudy", "Bay Study_Station Coordinates for Distribution_2018.xlsx"))%>%
  separate(Latitude, into=c("Lat_Deg", "Lat_Min"), sep = "°", convert=T)%>%
  separate(Longitude, into=c("Lon_Deg", "Lon_Min"), sep = "°", convert=T)%>%
  mutate(Latitude=Lat_Deg+Lat_Min/60,
         Longitude=Lon_Deg+Lon_Min/60)%>%
  select(Station, Latitude, Longitude)%>%
  filter(Station!="211E")%>%
  mutate(Station=recode(Station, `211W`="211"))

tidecodes_baystudy <- read_csv(file.path("data-raw", "Baystudy", "TideCodes_LookUp.csv"),
                               col_types=cols_only(Tide="i", Description="c"))

wavecodes_baystudy <- read_csv(file.path("data-raw", "Baystudy", "WaveCodes_LookUp.csv"),
                               col_types=cols_only(Waves="i", Description="c"))

cloudcovercodes_baystudy <- read_csv(file.path("data-raw", "Baystudy", "CloudCover_LookUp.csv"),
                               col_types=cols_only(CloudCover="i", Description="c"))

salintemp_baystudy<-read_csv(file.path("data-raw", "Baystudy", "SalinTemp.csv"),
                             col_types = cols_only(Year="i", Survey="i", Station="c",
                                                   ECSurf="d", ECAvg="d", ECBott="d",
                                                   TempSurf="d", TempAvg="d", TempBott="d"))


boatstation_baystudy <- read_csv(file.path("data-raw", "Baystudy", "BoatStation.csv"),
                                 col_types = cols_only(Year="i", Survey="i", Station="c",
                                                       Date="c", Depth="d", Secchi="d",
                                                       SubstrateCode="c", Waves="i", CloudCover="i",
                                                       Tide="i", StationComment="c"))%>%
  mutate(Date=parse_date_time(Date, orders="%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
  left_join(tidecodes_baystudy, by="Tide")%>%
  select(-Tide, -SubstrateCode)%>%
  rename(Tidestation=Description)%>%
  left_join(wavecodes_baystudy, by="Waves")%>%
  select(-Waves)%>%
  rename(Waves=Description)%>%
  left_join(cloudcovercodes_baystudy, by="CloudCover")%>%
  select(-CloudCover)%>%
  rename(CloudCover=Description)%>%
  left_join(salintemp_baystudy, by=c("Year", "Survey", "Station"))%>%
  left_join(stations_baystudy, by="Station")

boattow_baystudy<-read_csv(file.path("data-raw", "Baystudy", "BoatTow.csv"),
                           col_types = cols_only(Year="i", Survey="i", Station="c", Net="i",
                                                 Tow="i", Time="c", Bearing="d", Tide="i",
                                                 Direction="i", CatchCode="i", Duration="d",
                                                 StartMeter="d", EndMeter="d", TotalMeter="d",
                                                 StartLong="d", EndLong="d", StartLat="d",
                                                 EndLat="d", Distance="d", TowComment="c"))%>%
  select(-StartLong, -EndLong, -StartLat, -EndLat, -StartMeter, -EndMeter)%>% # Removing survey lats/longs and start/end meters for now
  mutate(Time=parse_date_time(Time, orders="%m/%d/%Y %H:%M:%S"),
         Tow_direction=recode(Direction, `1`="With current", `2`="Against current", `3`="Slack or cross-current"),
         Tow_volume=TotalMeter * 0.02687 * 10.7,
         Tow_area=Distance*3.42)%>%
  left_join(tidecodes_baystudy, by="Tide")%>%
  select(-Tide, -Direction)%>%
  rename(Method=Net, Tidetow=Description)%>%
  mutate(Method=recode(Method, `1`="Midwater trawl", `2`="Otter trawl", `3`="EL"))%>%
  filter(Method%in%c("Midwater trawl", "Otter trawl"))%>%
  mutate(TowStatus=recode(Tow, `0`="Invalid", `1`="Valid", `2`="Valid", `51`="Valid", `52`="Invalid", `53`="Invalid", `54`="Valid", `55`="Valid", `56`="Valid", `57`="Valid", `58`="Invalid"))%>%
  select(-Tow, -TotalMeter, -Distance)

env_baystudy <- left_join(boattow_baystudy, boatstation_baystudy, by=c("Year", "Survey", "Station"))%>%
  mutate(Tide=if_else(is.na(Tidetow), Tidestation, Tidetow),
         Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"),
         SampleID=1:nrow(.))%>%
  select(-Tidestation, -Tidetow, -Time)

rm(tidecodes_baystudy, wavecodes_baystudy, cloudcovercodes_baystudy, boattow_baystudy, boatstation_baystudy, salintemp_baystudy, stations_baystudy)

catch_baystudy <- read_csv(file.path("data-raw", "Baystudy", "Fish Catch Data.csv"),
                           col_types=cols_only(Year="i", Survey="i", Station="c",
                                               Net="i", AlphaCode="c",
                                               SizeGroup="i", QtsCaught="d", QtsSubsampled="d",
                                               PlusCount="d"))%>%
  rename(Method=Net)%>%
  mutate(Method=recode(Method, `1`="Midwater trawl", `2`="Otter trawl", `3`="EL"))%>%
  filter(Method%in%c("Midwater trawl", "Otter trawl"))%>%
  left_join(Species%>%
              select(AlphaCode=Baystudy_Code, Taxa)%>%
              filter(!is.na(AlphaCode)),
            by="AlphaCode")%>%
  select(-AlphaCode)

length_baystudy <- read_csv(file.path("data-raw", "Baystudy", "Fish Length Data.csv"),
                            col_types=cols_only(Year="i", Survey="i", Station="c",
                                                Net="i", AlphaCode="c",
                                                SizeGroup="i", Length="d", Frequency="d"))%>%
  rename(Method=Net)%>%
  mutate(Method=recode(Method, `1`="Midwater trawl", `2`="Otter trawl", `3`="EL"),
         AlphaCode = toupper(AlphaCode))%>%
  filter(Method%in%c("Midwater trawl", "Otter trawl"))%>%
  left_join(Species%>%
              select(AlphaCode=Baystudy_Code, Taxa)%>%
              filter(!is.na(AlphaCode)),
            by="AlphaCode")%>%
  select(-AlphaCode)

lengthcatch_baystudy<-catch_baystudy%>%
  full_join(length_baystudy%>%
               group_by(Year, Survey, Station, Method, SizeGroup, Taxa)%>%
               summarize(TotalMeasured=sum(Frequency))%>%
               ungroup(),
             by=c("Year", "Survey", "Station", "Method", "SizeGroup", "Taxa"))%>%
  filter(!is.na(TotalMeasured) & !is.na(PlusCount))%>%
  mutate(TotalCatch=(TotalMeasured+PlusCount)*(QtsCaught/QtsSubsampled))%>%
  select(-PlusCount, -QtsCaught, -QtsSubsampled)%>%
  right_join(length_baystudy, by=c("Year", "Survey", "Station", "Method", "SizeGroup", "Taxa"))%>%
  mutate(Count = (Frequency/TotalMeasured)*TotalCatch)%>%
  select(-TotalMeasured, -TotalCatch, -Frequency)


Baystudy <- env_baystudy%>%
  left_join(lengthcatch_baystudy, by=c("Year", "Survey", "Station", "Method"))%>%
  mutate(Sal_surf=ec2pss(ECSurf/1000, t=25),
         Sal_avg=ec2pss(ECAvg/1000, t=25),
         Sal_bott=ec2pss(ECBott/1000, t=25),
         Source="Bay Study",
         SampleID=paste(Source, SampleID))%>%
  select(-ECSurf, -ECAvg, -ECBott, -CatchCode, -SizeGroup, -StationComment)%>% # Remove unneeded variables
  group_by_at(vars(-Count))%>%
  summarise(Count=sum(Count))%>%
  ungroup()%>%
  rename(Notes_tow=TowComment, Tow_status=TowStatus,
         Weather=CloudCover, Temp_surf=TempSurf, Temp_avg=TempAvg,
         Temp_bott=TempBott, Tow_duration=Duration)%>%
  select(-Bearing, -Waves, -Weather, -Temp_avg, -Temp_bott, -Sal_avg, -Sal_bott) # Remove extra environmental variables

Baystudy_measured_lengths<-length_baystudy%>%
  left_join(Baystudy%>%
              select(SampleID, Year, Survey, Station, Method)%>%
              distinct(),
            by=c("Year", "Survey", "Station", "Method"))%>%
  select(SampleID, Taxa, Size_group=SizeGroup, Length, Count=Frequency)

Baystudy <- Baystudy%>%
  select(-Year)

usethis::use_data(Baystudy, Baystudy_measured_lengths, overwrite = TRUE)
