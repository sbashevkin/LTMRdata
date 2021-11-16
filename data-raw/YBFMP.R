## Code to format YBFMP fish data to LTMR package data
## Original data from: https://portal.edirepository.org/nis/mapbrowse?packageid=edi.233.2
## Last edited 11/15/2021 by C. Pien

# Notes:
# - Effort for beach seines to be calculated with Seine_volume
# - Effort for traps to be calculated with Trap_effort_hrs starting 2010
# - YBFMP_trap_effort provides monthly effort for data prior to 2010

# Load packages ----------------------------------------------------------

require(readr)
require(wql) #for ec2pss
require(dplyr)
require(tidyr)
require(lubridate)
require(LTMRdata)

# Station locations -------------------------------------------------------

stations_yolo <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.2&entityid=6a82451e84be1fe82c9821f30ffc2d7d",
                          col_types = cols_only(MethodCode = "f", StationCode="c", LatitudeLocation="d", LongitudeLocation="d"))%>%
  rename(Station=StationCode, Latitude=LatitudeLocation, Longitude=LongitudeLocation)%>%
  drop_na()


# Taxa ----------------------------------------------------------------------
# read in from crosswalk, only select yolo code + final taxon

taxa_yolo <- read_csv(file.path("data-raw", "Species Codes.csv")) %>%
  select(DWR_Code, ScientificName) %>%
  rename(CommonName = DWR_Code,
         Taxa = ScientificName)

# Effort data --------------------------------------------------------------------
trapeffort <- read_csv("data-raw/YBFMP/YBFMP_TrapHours_2010_2020.csv") %>%
  rename(Datetime = DateTime,
         Station = StationCode,
         Trap_effort_hrs = effort.hrs,
         Method = MethodCode) %>%
  mutate(Method = factor(Method, levels = c("FKTR", "RSTR")),
         Method = recode(Method, "RSTR" = "Rotary Screw Trap",
         "FKTR" = "Fyke Trap")) %>%
  select(SampleID,Trap_effort_hrs)

# First set of sample data to add sample ID--------------------------------------------------------
## Uncleaned datetimes to match edi data
sample_yolo_numbers <- read_csv("data-raw/YBFMP/Sample_20210722.csv", col_types = cols_only(SampleID = "i", SampleDate = "?", SampleTime = "t", StationCode = "f", MethodCode = "f")) %>%
  mutate(Date = lubridate::mdy(SampleDate),
         Datetime = paste0(Date, " ", hour(SampleTime), ":", minute(SampleTime)),
         Datetime = as.POSIXct(Datetime, format = "%Y-%m-%d %H:%M", tz="America/Los_Angeles")) %>%
  select(SampleID, StationCode, Datetime, MethodCode) %>%
  rename(Station = StationCode)

# Catch and WQ data --------------------------------------------------------------

yolodata <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.2&entityid=015e494911cf35c90089ced5a3127334",
            col_types = cols_only(SampleDate = "?", SampleTime = "t", StationCode = "f", MethodCode = "f", GearID = "f", CommonName = "c", GeneticallyConfirmed = "f", Field_ID_CommonName = "c", ForkLength = "d", Count = "i", FishSex = "f", Race = "f", MarkCode = "f", CWTSample = "l", StageCode = "f", Dead = "f", GearConditionCode = "f", WeatherCode = "f", WaterTemperature = "d", Secchi = "d", Conductivity = "d", SpCnd = "d", DO = "d", pH = "d", Turbidity = "d", SubstrateCode = "f", Tide = "f", VolumeSeined = "d", Latitude = "d", Longitude = "d"))%>%
  dplyr::select(-c(MarkCode, CWTSample, Dead, SubstrateCode, Field_ID_CommonName, GeneticallyConfirmed))%>%
  rename(Date = SampleDate,
         Time = SampleTime,
         Station = StationCode)%>%
  mutate(Date=mdy(Date),
         Datetime = paste0(Date, " ", hour(Time), ":", minute(Time)),
         Datetime = as.POSIXct(Datetime, format = "%Y-%m-%d %H:%M", tz="America/Los_Angeles"))%>% # Create Datetime
  filter(!(GearID %in% c("PSEIN100", "RSTR5", "SEINENCL", "SEINCOVE", "SEIN100", "SEIN30", "FKNT")),#remove old gear types
         GearConditionCode !=4) %>%# remove condition code of 4
  left_join(sample_yolo_numbers) # add in SampleID


# Catch data ---------------------------------------------------------

catch_yolo <- yolodata %>%
  select(-c(Latitude:Longitude, GearID, MethodCode, GearConditionCode:VolumeSeined)) %>%
  filter(!is.na(CommonName)) %>%# Remove any records with an NA organism code
  mutate(CommonName = recode(CommonName, "Pacific  Lamprey" = "Pacific Lamprey")) %>%
  left_join(taxa_yolo)%>%
  select(-CommonName)

## For length frequencies, need total catch/species
catch_yolo_total <- catch_yolo %>%
  group_by(Datetime, Station, Taxa) %>%
  summarize(Catch = sum(Count))


# Sample-level data -------------------------------------------------------
sample_yolo <- yolodata %>%
  rename(Method = MethodCode,
         Seine_volume = VolumeSeined) %>%
  select(c(SampleID, Date:Station, Datetime, Method, GearID, GearConditionCode:Seine_volume)) %>%
  distinct() %>% # Get just the sample rows
  mutate(SampleID2 = 1:nrow(.), # Assign new ID since some seem to be missing
        Tide = factor(Tide, levels = c("High", "Ebb", "Low", "Flood", "Overtopping")),
         Method = factor(Method, levels = c("FKTR", "RSTR", "BSEIN")),
         Method=recode(Method, "FKTR" = "Fyke Trap",
                       "RSTR" = "Rotary Screw Trap",
                       "BSEIN" = "Beach Seine"),
         Notes_tow = ifelse(GearConditionCode == 2, "Condition Code 2",
                            ifelse(GearConditionCode == 3, "Condition Code 3", ""))) %>%
  left_join(stations_yolo) %>% # Get Lat and Lon (original data missing some)
  select(-GearID, -MethodCode, -WeatherCode, -GearConditionCode)

## Import in sample numbers from our raw database because some dates and times were edited in effort table and will not join by datetime
sample_yolo_clean <- read_csv("data-raw/YBFMP/sample_cleaned_20211115.csv", col_types = cols_only(SampleID = "i", SampleDate = "?", SampleTime = "t", StationCode = "f", MethodCode = "f")) %>%
  mutate(Date = lubridate::mdy(SampleDate),
         SampleTime = lubridate::hms(SampleTime),
         Datetime = paste0(Date, " ", hour(SampleTime), ":", minute(SampleTime)),
         Datetime2 = as.POSIXct(Datetime, format = "%Y-%m-%d %H:%M", tz="America/Los_Angeles")) %>%
  select(SampleID, Datetime2)

# Join trap effort with sample info
sample_effort <- left_join(sample_yolo, sample_yolo_clean, by = c("SampleID")) %>%
  select(-Datetime) %>%
  rename(Datetime = Datetime2) %>% # Will join by cleaned up datetimes
  left_join(trapeffort, by = "SampleID") %>%
  select(-SampleID) %>% # This sampleID came from Yolo Database, and there were some missing
  filter(!(is.na(Datetime))) %>%
  rename(SampleID = SampleID2) # New sample ID created 1:nrows

# Length frequency -------------------------------------------------------
length_yolo <- catch_yolo %>%
  left_join(sample_yolo_clean, by = c("SampleID")) %>%
  select(-Datetime) %>%
  rename(Datetime = Datetime2) %>% # Will join by cleaned up datetimes
  select(Datetime, Station, Taxa, Count, ForkLength) %>%
  filter(!(is.na(ForkLength)),
         !(is.na(Datetime))) %>% # only want measured for this part
  group_by(Datetime, Station, Taxa, ForkLength) %>%
  summarise(LengthFrequency = sum(Count))

catchlength_yolo <- length_yolo %>%
  group_by(Datetime, Station, Taxa) %>%
  mutate(TotalMeasured = sum(LengthFrequency, na.rm = T))%>% # Calculate total number of fish measured for each species in each sample
  ungroup() %>%
  left_join(catch_yolo_total) %>% # Add catch numbers and species names
  mutate(Count = (LengthFrequency/TotalMeasured) * Catch)# Calculate adjusted count

# Create final datasets ---------------------------------------------------

## Overall datatset
YBFMP <- left_join(sample_effort, catchlength_yolo, by = c("Datetime", "Station")) %>%
  mutate(Sal_surf=round(ec2pss(Conductivity/1000, t=WaterTemperature),2),
         DO_surf = DO,
         Turb_surf = Turbidity,
         pH_surf = pH,
         Source = "YBFMP",
         SampleID = paste(Source, SampleID)) %>%
  rename(Length = ForkLength,
         Temp_surf = WaterTemperature) %>%
select(Source, Station, Latitude, Longitude, Date, Datetime, SampleID, Method, Tide, Sal_surf, Temp_surf, DO_surf, pH_surf, Turb_surf, Secchi, Taxa, Length, Count, Trap_effort_hrs, Seine_volume, Notes_tow)

## Just measured lengths
YBFMP_measured_lengths<-length_yolo%>%
  left_join(YBFMP %>% # Join species names and sampleID
              select(Datetime, Station, SampleID, Taxa)%>%
              distinct(),
            by=c("Datetime", "Station", "Taxa"))%>%
  group_by(across(-LengthFrequency))%>% # Add up any new multiples after removing lifestages
  summarise(Count=sum(LengthFrequency), .groups="drop")%>%
  select(SampleID, Taxa, Length=ForkLength, Count) # Reorder variables for consistency

## Effort
YBFMP_trap_effort <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.2&entityid=ace1ef25f940866865d24109b7250955")

# Clean up files -----------------------------------------------------------------
rm(taxa_yolo, stations_yolo, sample_yolo, length_yolo, catch_yolo, catch_yolo_total, catchlength_yolo, yolodata, trapeffort, sample_effort, sample_yolo_numbers, sample_yolo_clean)

# Compress data -------------------------------------------------------------------
usethis::use_data(YBFMP, YBFMP_measured_lengths, YBFMP_trap_effort, overwrite=TRUE, compress="xz") # Save compressed data to /data
