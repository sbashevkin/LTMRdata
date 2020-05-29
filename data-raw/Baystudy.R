## code to prepare `Baystudy` dataset goes here

require(readr)
require(wql)
require(dplyr)
require(tidyr)
require(lubridate)
require(readxl)
require(LTMRdata)
require(stringr)

# Station locations -------------------------------------------------------


stations_baystudy <- read_excel(file.path("data-raw", "Baystudy", "Bay Study_Station Coordinates for Distribution_04May2020.xlsx"))%>%
  separate(Latitude, into=c("Lat_Deg", "Lat_Min"), sep = "°", convert=T)%>% # Convert to decimal degrees
  separate(Longitude, into=c("Lon_Deg", "Lon_Min"), sep = "°", convert=T)%>%
  mutate(Latitude=Lat_Deg+Lat_Min/60,
         Longitude=Lon_Deg-Lon_Min/60)%>%
  select(Station, Latitude, Longitude)%>%
  filter(Station!="211E")%>% # Kathy said W location of station 211 is more often used, so using those coordinates
  mutate(Station=recode(Station, `211W`="211"))


# Import lookup tables ----------------------------------------------------


tidecodes_baystudy <- read_csv(file.path("data-raw", "Baystudy", "TideCodes_LookUp.csv"),
                               col_types=cols_only(Tide="i", Description="c"))

wavecodes_baystudy <- read_csv(file.path("data-raw", "Baystudy", "WaveCodes_LookUp.csv"),
                               col_types=cols_only(Waves="i", Description="c"))

cloudcovercodes_baystudy <- read_csv(file.path("data-raw", "Baystudy", "CloudCover_LookUp.csv"),
                               col_types=cols_only(CloudCover="i", Description="c"))


# Sample-level data -------------------------------------------------------


salintemp_baystudy<-read_csv(file.path("data-raw", "Baystudy", "SalinTemp.csv"),
                             col_types = cols_only(Year="i", Survey="i", Station="c",
                                                   ECSurf="d", ECAvg="d", ECBott="d",
                                                   TempSurf="d", TempAvg="d", TempBott="d"))

# Station-visit-level data

boatstation_baystudy <- read_csv(file.path("data-raw", "Baystudy", "BoatStation.csv"),
                                 col_types = cols_only(Year="i", Survey="i", Station="c",
                                                       Date="c", Depth="d", Secchi="d",
                                                       SubstrateCode="c", Waves="i", CloudCover="i",
                                                       Tide="i", StationComment="c"))%>%
  mutate(Date=parse_date_time(Date, orders="%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
  left_join(tidecodes_baystudy, by="Tide")%>% # Convert tide codes to values
  select(-Tide, -SubstrateCode)%>%
  rename(Tidestation=Description)%>%
  left_join(wavecodes_baystudy, by="Waves")%>% # Convert wave codes to values
  select(-Waves)%>%
  rename(Waves=Description)%>%
  left_join(cloudcovercodes_baystudy, by="CloudCover")%>% # Convert cloud cover codes to values
  select(-CloudCover)%>%
  rename(CloudCover=Description)


# Tow-level data

boattow_baystudy<-read_csv(file.path("data-raw", "Baystudy", "BoatTow.csv"),
                           col_types = cols_only(Year="i", Survey="i", Station="c", Net="i",
                                                 Tow="i", Time="c", Bearing="d", Tide="i",
                                                 Direction="i", CatchCode="i", Duration="d",
                                                 StartMeter="d", EndMeter="d", TotalMeter="d",
                                                 StartLong="d", EndLong="d", StartLat="d",
                                                 EndLat="d", Distance="d", TowComment="c"))%>%
  select(-StartLong, -EndLong, -StartLat, -EndLat, -StartMeter, -EndMeter)%>% # Removing survey lats/longs and start/end meters for now
  mutate(Time=parse_date_time(Time, orders="%m/%d/%Y %H:%M:%S"),
         Tow_direction=recode(Direction, `1`="With current", `2`="Against current", `3`="Slack or cross-current"), # Convert tow direction codes to values
         Tow_volume=TotalMeter * 0.02687 * 10.7, # Calculate tow volume using formulas in database metadata
         Tow_area=Distance*1852*3.42)%>% # Calculate tow area using formulas in database metadata, after first converting nautical miles to meters
  left_join(tidecodes_baystudy, by="Tide")%>% # Convert tide codes to values
  select(-Tide, -Direction)%>%
  rename(Method=Net, Tidetow=Description)%>%
  mutate(Method=recode(Method, `1`="Midwater trawl", `2`="Otter trawl", `3`="EL"))%>% # Convert method codes to values
  filter(Method%in%c("Midwater trawl", "Otter trawl"))%>% # Only include midwater and otter trawls. This currently does not do anything since those are the only two methods in the data.
  mutate(TowStatus=recode(Tow, `0`="Invalid", `1`="Valid", `2`="Valid", `51`="Valid", `52`="Invalid",
                          `53`="Invalid", `54`="Valid", `55`="Valid", `56`="Valid", `57`="Valid", `58`="Invalid"))%>% # Classify TowStatus into Valid or Invalid tows
  select(-Tow, -TotalMeter, -Distance) # Remove unneeded variables

# All sample-level data

env_baystudy <- left_join(boattow_baystudy, boatstation_baystudy, by=c("Year", "Survey", "Station"))%>% # Join together station-visit and tow - level data
  mutate(Tide=if_else(is.na(Tidetow), Tidestation, Tidetow), # Tide was sometimes recorded at each station visit and sometimes at each tow
         Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"),
         SampleID=1:nrow(.))%>% # Create identifier for each sample (tow)
  left_join(stations_baystudy, by="Station")%>% # Add station locations
  left_join(salintemp_baystudy, by=c("Year", "Survey", "Station"))%>% #Add salinity and temperature data
  select(-Tidestation, -Tidetow, -Time) # Remove unneeded variables

rm(tidecodes_baystudy, wavecodes_baystudy, cloudcovercodes_baystudy, boattow_baystudy, boatstation_baystudy, salintemp_baystudy, stations_baystudy) # Clean up


# Catch data --------------------------------------------------------------


catch_baystudy <- read_csv(file.path("data-raw", "Baystudy", "Fish Catch Data.csv"),
                           col_types=cols_only(Year="i", Survey="i", Station="c",
                                               Net="i", AlphaCode="c",
                                               SizeGroup="i", QtsCaught="d", QtsSubsampled="d",
                                               PlusCount="d"))%>%
  rename(Method=Net)%>%
  mutate(Method=recode(Method, `1`="Midwater trawl", `2`="Otter trawl", `3`="EL"))%>% # Convert method codes to values
  filter(Method%in%c("Midwater trawl", "Otter trawl"))%>% #Only keep midwater and otter trawls
  left_join(Species%>% # Add species names
              select(AlphaCode=Baystudy_Code, Taxa)%>%
              filter(!is.na(AlphaCode)),
            by="AlphaCode")%>%
  select(-AlphaCode) # Remove unneeded variable


# Length data -------------------------------------------------------------


length_baystudy <- read_csv(file.path("data-raw", "Baystudy", "Fish Length Data.csv"),
                            col_types=cols_only(Year="i", Survey="i", Station="c",
                                                Net="i", AlphaCode="c",
                                                SizeGroup="i", Length="d", Frequency="d"))%>%
  rename(Method=Net)%>%
  mutate(Method=recode(Method, `1`="Midwater trawl", `2`="Otter trawl", `3`="EL"), # Convert method codes to values
         AlphaCode = toupper(AlphaCode))%>% # Make alphacodes consistent
  filter(Method%in%c("Midwater trawl", "Otter trawl"))%>% #Only keep midwater and otter trawls
  left_join(Species%>% # Add species names
              select(AlphaCode=Baystudy_Code, Taxa)%>%
              filter(!is.na(AlphaCode)),
            by="AlphaCode")%>%
  select(-AlphaCode) # Remove unneeded variable

# Join length and catch data

lengthcatch_baystudy<-catch_baystudy%>%
  full_join(length_baystudy%>% # This first join will just include total number measured
               group_by(Year, Survey, Station, Method, SizeGroup, Taxa)%>%
               summarize(TotalMeasured=sum(Frequency))%>% # Calculate total number of each species of fish measured in each sample
               ungroup(),
             by=c("Year", "Survey", "Station", "Method", "SizeGroup", "Taxa"))%>%
  filter(!is.na(TotalMeasured) & !is.na(PlusCount))%>% # Remove any rows corresponding to 0 fish measured and 0 fish counted
  mutate(TotalCatch=(TotalMeasured+PlusCount)*(QtsCaught/QtsSubsampled))%>% # Calculate total number of each species caught in each sample
  select(-PlusCount, -QtsCaught, -QtsSubsampled)%>% # Remove unneeded variables
  right_join(length_baystudy, by=c("Year", "Survey", "Station", "Method", "SizeGroup", "Taxa"))%>% # Now join all measurements
  mutate(Count = (Frequency/TotalMeasured)*TotalCatch)%>% # Calculate adjusted size frequency
  select(-TotalMeasured, -TotalCatch, -Frequency) # Remove unneeded variables


# Create final datasets ---------------------------------------------------


Baystudy <- env_baystudy%>% # Start with sample-level data to retain samples with no catch (nets empty of fish)
  left_join(lengthcatch_baystudy, by=c("Year", "Survey", "Station", "Method"))%>% # Add length and catch data
  mutate(Sal_surf=ec2pss(ECSurf/1000, t=25), # Calculate salinity from conductivity
         Sal_avg=ec2pss(ECAvg/1000, t=25),
         Sal_bott=ec2pss(ECBott/1000, t=25),
         Source="Bay Study",
         SampleID=paste(Source, SampleID), # Add unique identifier for each sample across all studies
         Length_NA_flag=if_else(is.na(Length), "No fish caught", NA_character_), # Add reasoning for an NA lengths (all "No Fish Caught" for Baystudy)
         Taxa=stringr::str_remove(Taxa, " \\((.*)"))%>% # Remove life stage info from Taxa
  select(-ECSurf, -ECAvg, -ECBott, -CatchCode, -SizeGroup, -StationComment)%>% # Remove unneeded variables
  group_by_at(vars(-Count))%>%
  summarise(Count=sum(Count))%>% # Add up counts when the same species and length were recorded multiple times for a sample
  ungroup()%>%
  rename(Notes_tow=TowComment, Tow_status=TowStatus,
         Weather=CloudCover, Temp_surf=TempSurf, Temp_avg=TempAvg,
         Temp_bott=TempBott, Tow_duration=Duration)%>%
  select(-Bearing, -Waves, -Weather, -Temp_avg, -Temp_bott, -Sal_avg, -Sal_bott)%>% # Remove extra environmental variables
  filter(Tow_status=="Valid")%>% # Remove invalid tows
  select(Source, Station, Latitude, Longitude, Date, Datetime, Year, Survey, # Reorder variables for consistency
         Depth, SampleID, Method, Tide, Sal_surf, Temp_surf, Secchi,
         Tow_duration, Tow_area, Tow_volume, Tow_direction, Taxa, Length, Count, Length_NA_flag, Notes_tow)

# Just measured lengths

Baystudy_measured_lengths<-length_baystudy%>%
  inner_join(Baystudy%>% # Inner join to remove invalid tows
              select(SampleID, Year, Survey, Station, Method)%>% # Add SampleID and Method to length data
              distinct(),
            by=c("Year", "Survey", "Station", "Method"))%>%
  mutate(Taxa=stringr::str_remove(Taxa, " \\((.*)"))%>% #Remove life stage from Taxa
  select(SampleID, Taxa, Size_group=SizeGroup, Length, Count=Frequency)# Reorder variables for consistency

Baystudy <- Baystudy%>%
  select(-Year)# Remove unneeded variables

usethis::use_data(Baystudy, Baystudy_measured_lengths, overwrite = TRUE) # Save compressed data to /data folder
