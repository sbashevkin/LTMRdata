library(wql)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(tidyverse)
library(stringr)
require(LTMRdata)

# Setting up path for SLS files----
Path<-file.path(tempdir(), "SLS.zip")
Path_origin<-file.path(tempdir())
#Downloading MWT_data.zip----
download.file("https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SLS.zip", Path, mode="wb",method="libcurl")
unzip(Path,files="SLS.mdb",exdir=Path_origin)

# MS access database set up----
# File path to Access database (Salvage)
db_path <- file.path(tempdir(),"SLS.mdb")

source(file.path("data-raw", "bridgeAccess.R"))

keepTables <- c("20mm Stations","AreaCode1","Catch","FishCodes","Lengths",
                "Tow Info","Water Info","Meter Corrections","Wt_factors")

SLSTables <- bridgeAccess(db_path,
                            tables = keepTables,
                            script = file.path("data-raw", "connectAccess.R"))

#MWT data setup ----

SLSTables$Catch <- SLSTables$Catch%>%
  dplyr::select(Date,Station,Tow,FishCode,Catch,CatchID)%>%
  mutate(Station=as.character(Station))

SLSTables$Lengths <- SLSTables$Lengths %>%
  dplyr::select(Date,Station,Tow,FishCode,Length,entryorder)%>%
  mutate(Station=as.character(Station))

SLSTables$`Meter Corrections` <- SLSTables$`Meter Corrections`%>%
  dplyr::select(StudyYear,MeterSerial,CalibrationDate,kfactor,Notes)
SLSTables$`20mm Stations` <- SLSTables$`20mm Stations`%>%
  dplyr::select(Station,LatD,LatM,LatS,LonD,LonM,LonS)%>%
  mutate(Latitude=LatD+LatM/60+LatS/3600,
         Longitude=LonD+LonM/60+LonS/3600,
         Station=as.character(Station))%>%
  dplyr::select(Station,Latitude,Longitude)%>%
  na.omit()

SLSTables$`Tow Info`<-SLSTables$`Tow Info`%>%
  dplyr::select(Date,Station,Tow,Time,Tide,BottomDepth,CableOut,Duration,NetMeterSerial,NetMeterStart,NetMeterCheck,CBMeterSerial,CBMeterStart,CBMeterEnd,CBMeterCheck,Comments)%>%
  rename(Notes_tow=Comments)%>%
  mutate(Station=as.character(Station),
         Tide=as.character(Tide),
         CableOut=CableOut*0.3048)

SLSTables$`Water Info`<-SLSTables$`Water Info`%>%dplyr::select(Survey,Date,Station,Temp,TopEC,BottomEC,Secchi,Turbidity,Comments)%>%
  rename(Notes_env=Comments)%>%
  mutate(Station=as.character(Station))

# Manipulating the data tables --------------------------------------------

waterInfo <- SLSTables$`Water Info` %>%
  mutate(# Converting secchi from cm to m
    Secchi = Secchi/100,
    # Converting EC to salinity
    # Per SOP normalized at temp = 25C; units are in millisiemens
    Sal_surf = wql::ec2pss(TopEC/1000, t = 25),
    Sal_bot = wql::ec2pss(BottomEC/1000, t = 25),
    # This is to take care of how the floats are read between Access and readr methods...
    Temp_surf = round(Temp, 2),
    # Changing Date to as.Date
    Date = as.Date(Date))

towInfo <- SLSTables$`Tow Info` %>%
  # 1 parsing error because time was not recorded for that row
  # Now, joining to the Meter Corrections table to calculate tow volume later
  # This is based on the "WEB_Raw_Catch_Volum_Info" query in the SLS Access query database
  # First need to create a study year column; from the query above, the "WEB_VolumeInfo" query calculates year from the
  # towInfo table.
  mutate(StudyYear = year(Date),
         # Changing Date from POSIXct format to simply Date for easy of use
         # Timezone data is still retained in the Datetime column
         Date = as.Date(Date)) %>%
  left_join(SLSTables$`Meter Corrections` %>%
              # There are duplicated values here in this table; will simply distinct() them
              # Confirmed via email with Adam, ES of Native Fish unit as of 10-27-2021
              distinct(),
            by = c("StudyYear", "NetMeterSerial" = "MeterSerial")) %>%
  # Moving on to various required calculations
  mutate(Datetime = ymd_hms(if_else(is.na(Time), NA_character_, paste(Date, paste(hour(Time), minute(Time), second(Time), sep=":"))),
                            tz = "America/Los_Angeles"),
         # Now to turn tide into factors as outlined in the metadata file
         Tide = case_when(Tide == 1 ~ "High Slack",
                          Tide == 2 ~ "Ebb",
                          Tide == 3 ~ "Low Slack",
                          Tide == 4 ~ "Flood",
                          TRUE ~ NA_character_),
         # Converting bottom depth to meters
         Depth = BottomDepth * 0.3048,
         # Calculating tow volume for the fish net, 0.37 as the area of the mout of the net (per Access + SLS SOP document)
         Tow_volume = NetMeterCheck * kfactor * 0.37)

catch <- SLSTables$Catch %>%
  mutate(Date = as.Date(Date))

lengths <- SLSTables$Lengths %>%
  mutate(Date = as.Date(Date)) %>%
  # Calculating total number of fish measured (across all lengths) and # of fish measured
  # per Date, Station, Tow, and FishCode
  # This is to calculate plus counts later in dfFin
  group_by(Date, Station, Tow, FishCode, Length)%>%
  summarise(LengthFrequency=n(), .groups="drop")%>%
  group_by(Date, Station, Tow, FishCode) %>%
  mutate(TotalLengthMeasured=sum(LengthFrequency)) %>%
  ungroup()

# Now to combine the datasets together, following the relationship table in Access
# The tables go waterInfo -> towInfo -> Catch -> lengths
SLS <- waterInfo %>%
  full_join(towInfo,
            by = c("Date", "Station")) %>%
  full_join(catch,
            by = c("Date", "Station", "Tow"),multiple="all") %>%
  full_join(lengths,
            by = c("Date", "Station", "Tow", "FishCode"),multiple="all") %>%
  # Adding in taxa name based on Species Code.csv file
  left_join(Species %>%
              select(TMM_Code,
                     Taxa) %>%
              dplyr::filter(!is.na(TMM_Code)),
            by = c("FishCode"="TMM_Code")) %>%
  left_join(SLSTables$"20mm Stations",
            by="Station")%>%
  # Merging the two comment columns together; they both have data in them
  mutate(Notes_tow=paste(Notes_tow, Notes_env, sep = "; ")) %>%
  arrange(Date, Datetime, Survey, Station, Tow, Taxa, Length) %>%
  mutate(Source = "SLS",
         SampleID=paste(Source, Date, Station, Tow), # Creating SampleID index
         Count = if_else(is.na(Length), as.numeric(Catch), (LengthFrequency/TotalLengthMeasured) * Catch),
         # Creating Length_NA_flag to parallel the other survey datasets in LTMR
         Length_NA_flag = case_when(is.na(Count) ~ "No fish caught",
                                    is.na(Length) & Count > 0 ~ "Unknown length",
                                    TRUE ~ NA_character_),
         # Creating Method column; Adam described this as an "Oblique tow", significantly diff from MWT
         Method = "Oblique tow",
         Station=as.character(Station),
         Count=if_else(Length_NA_flag=="No fish caught", 0, Count, missing=Count))%>% # Transform all counts for "No fish caught" to 0.
  # Removing CatchID and entryorder as they are not relevant to the dataset
  # Removing TopEC, BottomEC as they have been converted over the salinity already
  # Removing CBMeterSerial, CBMeterStart, CBMeterEnd, CBMeterCheck as CB not ran on the SLS
  select(Source, Station, Latitude, Longitude,
         Date, Datetime, Survey, Depth, SampleID, Method,
         Tide, Sal_surf, Sal_bot, Temp_surf, Secchi, Turbidity, Tow_volume,
         Cable_length=CableOut, Tow_duration=Duration,
         Taxa, Length, Count, Length_NA_flag,
         Notes_tow, Notes_flowmeter = Notes)

# # Just to make sure that no duplications occurred; lengths should be the same
# all.equal(lengths$Length %>% sum(na.rm = T),
#           dfFin$Length %>% sum(na.rm = T))

usethis::use_data(SLS, overwrite=TRUE, compress="xz")
