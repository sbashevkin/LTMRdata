## code to prepare `Baystudy` dataset goes here

require(readr)
require(wql)
require(dplyr)
require(tidyr)
require(lubridate)
require(readxl)
require(LTMRdata)
require(stringr)
library(rvest)

# Station locations -------------------------------------------------------

stations_baystudy <- read_excel(file.path("data-raw", "Baystudy", "Bay Study_Station Coordinates for Distribution_04May2020.xlsx"))%>%
  separate(Latitude, into=c("Lat_Deg", "Lat_Min"), sep = "°", convert=T)%>% # Convert to decimal degrees
  separate(Longitude, into=c("Lon_Deg", "Lon_Min"), sep = "°", convert=T)%>%
  mutate(Latitude=Lat_Deg+Lat_Min/60,
         Longitude=Lon_Deg-Lon_Min/60)%>%
  dplyr::select(Station, Latitude, Longitude)%>%
  dplyr::filter(Station!="211E")%>% # Kathy said W location of station 211 is more often used, so using those coordinates
  mutate(Station=recode(Station, `211W`="211"))%>%
  mutate(Station=as.integer(Station))

# Import lookup tables ----------------------------------------------------
options(timeout = 999999)
# Path<-file.path(tempdir(), "BayStudy_AccessDatabase_1980-2021.zip")
# Path_origin<-file.path(tempdir())
#Downloading MWT_data.zip----
# Grab file name of Access DB, which changes per year

session <- session("https://filelib.wildlife.ca.gov/Public/BayStudy/Access_Database/")
links <- html_elements(session, "a") %>%
  html_attr("href")
dbLink <- links[which(grepl("\\.zip", links))]
fileName <- regmatches(dbLink, regexpr("(BayStudy_).*", dbLink))

download.file(paste0("https://filelib.wildlife.ca.gov", dbLink),
              file.path(tempdir(), fileName), mode = "wb", method = "libcurl")

unzip(file.path(tempdir(), fileName), exdir = tempdir())

accessFile <- unzip(file.path(tempdir(), fileName), list = T) %>%
  pull(Name) %>%
  .[which(grepl("\\.accdb", .))]

# File path to Access database (Salvage)
source(file.path("data-raw", "bridgeAccess.R"))

db_path <- file.path(tempdir(), accessFile)

keepTables <- c("TideCodes_LookUp","WaveCodes_LookUp","CloudCover_LookUp",
                "SalinTemp","BoatStation","BoatTow",
                "Fish Catch Data","Fish Length Data")

BayStudyTables <- bridgeAccess(db_path,
                     tables = keepTables,
                     script = file.path("data-raw", "connectAccess.R"))

# Environ data ----
tidecodes_baystudy <- BayStudyTables$TideCodes_LookUp%>%select(Tide,Description)

wavecodes_baystudy <- BayStudyTables$WaveCodes_LookUp%>%select(Waves,Description)

cloudcovercodes_baystudy <- BayStudyTables$CloudCover_LookUp%>%select(CloudCover,Description)



# Sample-level data -------------------------------------------------------


salintemp_baystudy<-BayStudyTables$SalinTemp%>%select(Year,Survey,Station,
                                                      ECSurf,ECAvg,ECBott,
                                                      TempSurf,TempAvg,TempBott)

# Station-visit-level data -----

boatstation_baystudy <- BayStudyTables$BoatStation%>%select(Year, Survey, Station,
                                                            Date, Depth, Secchi,
                                                            SubstrateCode, Waves, CloudCover,
                                                            Tide, StationComment)%>%
  #mutate(Date=parse_date_time(Date, orders="%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
  left_join(tidecodes_baystudy, by="Tide")%>% # Convert tide codes to values
  dplyr::select(-Tide, -SubstrateCode)%>%
  rename(Tidestation=Description)%>%
  left_join(wavecodes_baystudy, by="Waves")%>% # Convert wave codes to values
  dplyr::select(-Waves)%>%
  rename(Waves=Description)%>%
  left_join(cloudcovercodes_baystudy, by="CloudCover")%>% # Convert cloud cover codes to values
  dplyr::select(-CloudCover)%>%
  rename(CloudCover=Description)


# Tow-level data -----

boattow_baystudy<-BayStudyTables$BoatTow%>%select(Year, Survey,Station, Net,
                                                  Tow, Time, Bearing, Tide,
                                                  Direction, CatchCode, Duration,
                                                  StartMeter, EndMeter, TotalMeter,
                                                  StartLong, EndLong, StartLat,
                                                  EndLat, Distance, TowComment)%>%
  dplyr::select(-StartLong, -EndLong, -StartLat, -EndLat, -StartMeter, -EndMeter)%>% # Removing survey lats/longs and start/end meters for now
  mutate(Tow_direction=recode(Direction, `1`="With current", `2`="Against current", `3`="Slack or cross-current"))%>% # Convert tow direction codes to values
  left_join(tidecodes_baystudy, by="Tide")%>% # Convert tide codes to values
  dplyr::select(-Tide, -Direction)%>%
  rename(Method=Net, Tidetow=Description)%>%
  mutate(Method=recode(Method, `1`="Midwater trawl", `2`="Otter trawl", `3`="EL"))%>% # Convert method codes to values
  mutate(Tow_volume=if_else(Method=="Midwater trawl", TotalMeter * 0.02687 * 10.7, NA_real_),# Calculate tow volume using formulas in database metadata
         Tow_area=if_else(Method=="Otter trawl", Distance*1852*3.42, NA_real_))%>% # Calculate tow area using formulas in database metadata, after first converting nautical miles to meters)
  dplyr::filter(Method%in%c("Midwater trawl", "Otter trawl"))%>% # Only include midwater and otter trawls. This currently does not do anything since those are the only two methods in the data.
  mutate(TowStatus=recode(Tow, `0`="Invalid", `1`="Valid", `2`="Valid", `51`="Valid", `52`="Invalid",
                          `53`="Invalid", `54`="Valid", `55`="Valid", `56`="Valid", `57`="Valid", `58`="Invalid"))%>% # Classify TowStatus into Valid or Invalid tows
  dplyr::select(-Tow, -TotalMeter, -Distance) %>% # Remove unneeded variables
  # fixing time zone to tbe pst
  mutate(Time = lubridate::force_tz(Time, tz = "America/Los_Angeles"))

# All sample-level data -----

env_baystudy <- left_join(boattow_baystudy, boatstation_baystudy, by=c("Year", "Survey", "Station"))%>% # Join together station-visit and tow - level data
  mutate(Tide=if_else(is.na(Tidetow), Tidestation, Tidetow), # Tide was sometimes recorded at each station visit and sometimes at each tow
         Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"),
         SampleID=1:nrow(.))%>% # Create identifier for each sample (tow)
  left_join(stations_baystudy, by="Station")%>% # Add station locations
  left_join(salintemp_baystudy, by=c("Year", "Survey", "Station"))%>% #Add salinity and temperature data
  dplyr::select(-Tidestation, -Tidetow, -Time) # Remove unneeded variables

rm(tidecodes_baystudy, wavecodes_baystudy, cloudcovercodes_baystudy, boattow_baystudy, boatstation_baystudy, salintemp_baystudy, stations_baystudy) # Clean up


# Catch data --------------------------------------------------------------


catch_baystudy <- BayStudyTables$`Fish Catch Data`%>%select(Year, Survey, Station,
                                                            Net, AlphaCode,
                                                            SizeGroup, QtsCaught, QtsSubsampled,
                                                            PlusCount)%>%
  rename(Method=Net)%>%
  mutate(Method=recode(Method, `1`="Midwater trawl", `2`="Otter trawl", `3`="EL"))%>% # Convert method codes to values
  dplyr::filter(Method%in%c("Midwater trawl", "Otter trawl"))%>% #Only keep midwater and otter trawls
  left_join(Species%>% # Add species names
              dplyr::select(AlphaCode=Baystudy_Code, Taxa)%>%
              dplyr::filter(!is.na(AlphaCode)),
            by="AlphaCode")%>%
  dplyr::select(-AlphaCode) # Remove unneeded variable


# Length data -------------------------------------------------------------


length_baystudy <- BayStudyTables$`Fish Length Data`%>%select(Year, Survey, Station,
                                                              Net, AlphaCode,
                                                              SizeGroup, Length, Frequency)%>%
  rename(Method=Net)%>%
  mutate(Method=recode(Method, `1`="Midwater trawl", `2`="Otter trawl", `3`="EL"), # Convert method codes to values
         AlphaCode = toupper(AlphaCode))%>% # Make alphacodes consistent
  dplyr::filter(Method%in%c("Midwater trawl", "Otter trawl"))%>% #Only keep midwater and otter trawls
  left_join(Species%>% # Add species names
              dplyr::select(AlphaCode=Baystudy_Code, Taxa)%>%
              dplyr::filter(!is.na(AlphaCode)),
            by="AlphaCode")%>%
  dplyr::select(-AlphaCode) # Remove unneeded variable

# Join length and catch data ----

lengthcatch_baystudy<-catch_baystudy%>%
  full_join(length_baystudy%>% # This first join will just include total number measured
               group_by(Year, Survey, Station, Method, SizeGroup, Taxa)%>%
               summarize(TotalMeasured=sum(Frequency))%>% # Calculate total number of each species of fish measured in each sample
               ungroup(),
             by=c("Year", "Survey", "Station", "Method", "SizeGroup", "Taxa"))%>%
  dplyr::filter(!is.na(TotalMeasured) & !is.na(PlusCount))%>% # Remove any rows corresponding to 0 fish measured and 0 fish counted
  mutate(TotalCatch=(TotalMeasured+PlusCount)*(QtsCaught/QtsSubsampled))%>% # Calculate total number of each species caught in each sample
  dplyr::select(-PlusCount, -QtsCaught, -QtsSubsampled)%>% # Remove unneeded variables
  right_join(length_baystudy, by=c("Year", "Survey", "Station", "Method", "SizeGroup", "Taxa"))%>% # Now join all measurements
  mutate(Count = (Frequency/TotalMeasured)*TotalCatch)%>% # Calculate adjusted size frequency
  dplyr::select(-TotalMeasured, -TotalCatch, -Frequency) # Remove unneeded variables


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
  dplyr::select(-ECSurf, -ECAvg, -ECBott, -CatchCode, -SizeGroup, -StationComment)%>% # Remove unneeded variables
  group_by_at(vars(-Count))%>%
  summarise(Count=sum(Count), .groups="drop")%>% # Add up counts when the same species and length were recorded multiple times for a sample
  rename(Notes_tow=TowComment, Tow_status=TowStatus,
         Weather=CloudCover, Temp_surf=TempSurf, Temp_avg=TempAvg,
         Temp_bott=TempBott, Tow_duration=Duration)%>%
  dplyr::select(-Bearing, -Waves, -Weather, -Temp_avg, -Temp_bott, -Sal_avg, -Sal_bott)%>% # Remove extra environmental variables
  dplyr::filter(Tow_status=="Valid")%>% # Remove invalid tows
  mutate(Count=if_else(Length_NA_flag=="No fish caught", 0, Count, missing=Count))%>% # Transform all counts for 'No fish caught' to 0.
  dplyr::select(Source, Station, Latitude, Longitude, Date, Datetime, Year, Survey, # Reorder variables for consistency
         Depth, SampleID, Method, Tide, Sal_surf, Temp_surf, Secchi,
         Tow_duration, Tow_area, Tow_volume, Tow_direction, Taxa, Length, Count, Length_NA_flag, Notes_tow)

# Just measured lengths

Baystudy_measured_lengths<-length_baystudy%>%
  inner_join(Baystudy%>% # Inner join to remove invalid tows
              dplyr::select(SampleID, Year, Survey, Station, Method)%>% # Add SampleID and Method to length data
              distinct(),
            by=c("Year", "Survey", "Station", "Method"))%>%
  mutate(Taxa=stringr::str_remove(Taxa, " \\((.*)"))%>% #Remove life stage from Taxa
  dplyr::select(SampleID, Taxa, Size_group=SizeGroup, Length, Count=Frequency)# Reorder variables for consistency

Baystudy <- Baystudy%>%
  dplyr::select(-Year)# Remove unneeded variables

usethis::use_data(Baystudy, Baystudy_measured_lengths, overwrite = TRUE, compress="xz") # Save compressed data to /data folder

