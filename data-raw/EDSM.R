
###################################################################
## code to prepare `EDSM` dataset as prepared by Sam Bashevkin   ##
###################################################################

require(wql)
require(LTMRdata)
require(readr)
require(dplyr)
require(lubridate)
require(tidyr)
require(stringr)
require(utils)
require(rvest)
require(XML)
library(deltadata)

# downloading data because the dataset is too huge to keep on file
# start pipeline to edi
# relational tables downloaded using getEDI due to new api restricitons on downloading EDI dadta
#Version 13 for SampleYear 2025 data change yearly

tableNames<- getEDI("edi.415.13", files=c("EDSM_20mm.csv","EDSM_KDTR.csv"))
# Find the newest revision

#get edi gives a list, parse out the correct tables from the list.
TMMtable<- tableNames$EDSM_20mm.csv
KDTRtable<-tableNames$EDSM_KDTR.csv

# Make sure URLs exist for each table
if(any(nrow(TMMtable)==0,
       nrow(KDTRtable)==0)){
  stop("regex for URLs isn't working right")
}

# Want only the 20mm and kdtr data
#moved time formatting earlier in script
#added Turb FNU
#########removed Sal qc measures.



EDSM <- bind_rows(
  TMMtable %>%
    select(StationCode, SampleDate, SampleTime, Tide,
           LongitudeStart, LatitudeStart, TowNumber,
           GearConditionCode, FlowDebris,
           SpecificConductanceTop, WaterTempTop,
           TurbidityTopFNU, TurbidityTopNTU, TurbidityBottom, Secchi,
           BottomDepth, Volume, SamplingDirection, MethodCode,
           OrganismCode, ForkLength, Count,
           MarkCode, RaceByLength) %>%
    mutate(SampleDate = parse_date_time(SampleDate, "%Y-%m-%d", tz = "America/Los_Angeles")) %>%
    rename(TurbidityNTU = TurbidityTopNTU,
           TurbidityFNU = TurbidityTopFNU),

  KDTRtable %>%
    select(StationCode, SampleDate, SampleTime, Tide,
           LongitudeStart, LatitudeStart, TowNumber,
           SpecificConductance, WaterTemp,
           TurbidityFNU, TurbidityNTU, Secchi, BottomDepth,
           GearConditionCode, FlowDebris,
           Volume, SamplingDirection, MethodCode,
           OrganismCode, ForkLength, Count,
           MarkCode, RaceByLength) %>%
    mutate(SampleDate = parse_date_time(SampleDate, "%Y-%m-%d", tz = "America/Los_Angeles")) %>%
    rename(SpecificConductanceTop = SpecificConductance, WaterTempTop = WaterTemp)
) %>%
  rename(Temp_surf = WaterTempTop, Tow_volume = Volume, Method = MethodCode,
         Tow_direction = SamplingDirection, Length = ForkLength,
         Conductivity = SpecificConductanceTop,
         Latitude = LatitudeStart, Longitude = LongitudeStart,
         Date = SampleDate, Time = SampleTime, Depth = BottomDepth, Station = StationCode, Tow = TowNumber) %>%
  dplyr::filter(is.na(GearConditionCode) | !GearConditionCode %in% c(3, 4, 9)) %>%
  mutate(Tow_volume = if_else(FlowDebris %in% c("Y", "Yes"), NA_real_, Tow_volume, missing = Tow_volume),
         Tow_volume = if_else(Tow_volume == 0, NA_real_, Tow_volume),
         Source = "EDSM",
         # Date = parse_date_time(Date, "%Y-%m-%d", tz = "America/Los_Angeles"),
         Time = parse_date_time(Time, "%H:%M:%S", tz = "America/Los_Angeles"),
         Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz = "America/Los_Angeles"),
         # Removing conductivity data from dates before it was standardized
         Conductivity = if_else(Date < parse_date_time("2019-06-01", "%Y-%m-%d", tz = "America/Los_Angeles"), NA_real_, Conductivity),
         Sal_surf = ec2pss(Conductivity / 1000, t = 25),
         # Updating recode (superseded as of dplyr 1.1.2) to case_when()
         # Using regex to make it more generic
         Method = case_when(grepl("^K(D)?T(R|S\\d+)", Method) ~ "Kodiak trawl",
                            Method %in% "20mm" ~ "20mm net"),
         # recode(Method, KDTR="Kodiak trawl", `20mm`="20mm net"),
         Tow_direction = recode(Tow_direction, U = "Upstream", D = "Downstream"),
         Depth = if_else(Method == "20mm net", Depth * 0.3048, Depth), # Convert feet to meters for 20mm (KDTR already in meters)
         Secchi = Secchi * 100, # convert Secchi to cm
         Tide = recode(Tide, HS = "High Slack", LS = "Low Slack"), # Standardize tide codes
         SampleID = paste(Datetime, Station, Tow, Method, Latitude, Longitude),
         MarkCode = replace_na(MarkCode, "None"),
         Group = case_when(MarkCode == "None" & OrganismCode == "CHN" ~ RaceByLength,
                           MarkCode != "None" ~ paste("Tag", 1:nrow(.)),
                           TRUE ~ NA_character_),
         Count = if_else(OrganismCode == "NOFISH", NA_real_, Count)) %>%
  select(-Time, -MarkCode, -RaceByLength, -GearConditionCode, -FlowDebris) %>%
  group_by(across(-Count)) %>% # Some species are recorded with the same length multiple times
  summarise(Count = sum(Count), .groups = "drop") %>%
  group_by(SampleID, OrganismCode, Group) %>%
  mutate(TotalMeasured = sum(Count[which(Length != 0)]), # Calculate total number of fish of each species measured
         Total = sum(Count), # Calculate total number of fish of each species caught
         Count = (Count / TotalMeasured) * Total) %>% # Calculate the adjusted length frequency
  ungroup() %>%
  mutate(Length = if_else((is.infinite(Count) & Length == 0) | OrganismCode == "NOFISH", NA_real_, Length), # Some Chinook were not measured, so these lines fix some after-effects of that
         Length_NA_flag = case_when(
           is.infinite(Count) ~ "Unknown length",
           is.na(Length) ~ "No fish caught",
           TRUE ~ NA_character_), # Add reasoning for NA lengths
         Count = if_else(is.infinite(Count), Total, Count)) %>% # These cases all represent the only row of that SamppleID, OrganismCode, and Group, so this doesn't result in over-counting, it just returns the value to the prior count
  dplyr::filter(Length != 0 | is.na(Length)) %>%
  dplyr::filter(Count != 0 | is.na(Count)) %>% # Remove 1 case of a 0 count of a striped bass, *****NEED TO CHECK IN UPDATES*****
  select(-Total, -TotalMeasured, -Group) %>%
  group_by(across(-Count)) %>% # Add up any new multiples after removing Group
  summarise(Count = sum(Count), .groups = "drop") %>%
  group_by(SampleID) %>% # Now we need to remove any NOFISH records when there are actually fish counts in that sample (including next 3 lines)
  mutate(Valid = sum(Count, na.rm = T)) %>%
  ungroup() %>%
  dplyr::filter(!(Valid > 0 & OrganismCode == "NOFISH")) %>%
  left_join(Species %>%
              select(USFWS_Code, Taxa) %>%
              dplyr::filter(!is.na(USFWS_Code)),
            by = c("OrganismCode" = "USFWS_Code")) %>%
  mutate(SampleID = paste(Source, SampleID), # Add variable for unique (across all studies) sampleID
         Taxa = str_remove(Taxa, " \\((.*)"), # Remove life stage info from Taxa names
         Count = if_else(Length_NA_flag == "No fish caught", 0, Count, missing = Count)) %>% # Transform all counts for 'No fish caught' to 0.
  select(Source, Station, Latitude, Longitude, Date, Datetime, Depth, SampleID, Method, Tide, Sal_surf,
         Temp_surf, TurbidityNTU, TurbidityFNU,
         # TurbidityBottomNTU, # Can include this back in if in the future more than just EDSM collects this data
         Secchi, Tow_volume, Tow_direction, Taxa, Length, Count, Length_NA_flag) %>%
  # Remove NA gear types
  # As of 07/19/2024, there are two instances in the KDTR dataset. Claudia MacFarlane
  # could not confirm which gear type was used for these instances. Removing for now
  filter(!is.na(Method))



# Check for new species to add to Species.CSV
# 1. Clean the species lookup table
USFWS_Species <- Species %>%
  select(USFWS_Code, Taxa) %>%
  dplyr::filter(!is.na(USFWS_Code))

# 2. Find new species in the EDSM dataset
New_Species <- EDSM %>%
  group_by(Taxa) %>%
  slice(1) %>%
  transmute(Year = year(Datetime),
            Taxa) %>%
  full_join(USFWS_Species,
            by = "Taxa") %>%
  filter(is.na(USFWS_Code), Taxa != "NOFISH")



# --- Check previous data ---

source(file.path("data-raw", "comparison.R"))



# Run before updating the data object to the new data
####Mutate added to deal with new TurbidityColumn added to df in new EDI publication
compareEDSM <- create_comparison_report(
  EDSM, LTMRdata::EDSM %>%
    mutate(TurbidityFNU=NA),
  id_cols = c("SampleID", "Taxa", "Length", "Count")
)

###105525 new datapoints to add
#189248 changed records all verified as valid QC efforts, or small edits that will be fixed and are minimal


#adding Tide Tow_area, Sal_bot as NA columns for testing
EDSM<- EDSM %>%
  mutate(Tow_area=NA,
         Sal_bot=NA)

#testing
# --- Run check on singular database ---
# devtools::load_all(".") should load this helper function for use
tests <- test_dataset(EDSM, return_failures = T)

if (length(tests) == 0) {
  message("Tests passed, saving the data.\n")
  usethis::use_data(EDSM,overwrite=TRUE, compress="xz")
}

#
# # Save compressed data to /data


#
# # Save compressed data to /data
usethis::use_data(EDSM, overwrite=TRUE, compress = "xz")
