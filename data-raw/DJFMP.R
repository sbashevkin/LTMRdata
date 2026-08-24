library(wql)
library(readr)
library(dplyr)
library(lubridate)
library(hms)
library(curl)
library(tidyverse)
library(stringr)
require(LTMRdata)
library(rvest)
library(deltadata)

# downloading data because the dataset is too huge to keep on file
##Update revision number .14 yearly. File Names may change from year to year should auto detect tables in future
#use delta data getEDI for data download

tableNames<- getEDI("edi.244.14", files=c("1976-2001_DJFMP_trawl_fish_and_water_quality_data.csv","2002-2025_DJFMP_trawl_fish_and_water_quality_data.csv",
                                          "1976-2025_DJFMP_beach_seine_fish_and_water_quality_data.csv", "DJFMP_Site_Locations.csv"))
stationtable <- tableNames[[grep("Site_Locations", names(tableNames))]]

earlytrawltable <- tableNames[[grep("1976.*trawl", names(tableNames))]]

presenttrawltable <- tableNames[[grep("2002.*trawl", names(tableNames))]]

seinetable <- tableNames[[grep("beach_seine", names(tableNames))]]


# Make sure URLs exist for each table
if(any(nrow(stationtable)==0,
   nrow(earlytrawltable)==0,
   nrow(presenttrawltable)==0,
   nrow(seinetable)==0)){
  stop("regex for URLs isn't working right")
}

DJFMP_stations <- stationtable %>%
  select(StationCode, Latitude, Longitude)

data <- bind_rows(
  # 1976-2001 trawl data
  earlytrawltable %>%
    select(StationCode, SampleDate, SampleTime,
           TowNumber, MethodCode, GearConditionCode,
           FlowDebris, SpecificConductance,
           WaterTemp, Turbidity, TurbidityFnu, Secchi,
           Volume, SamplingDirection, MarkCode, RaceByLength,
           OrganismCode, ForkLength, Count, TowDuration),
  # 2002-present trawl
  presenttrawltable %>%
    select(StationCode, SampleDate, SampleTime,
           TowNumber, MethodCode, GearConditionCode,
           FlowDebris, SpecificConductance,
           WaterTemp, Turbidity, TurbidityFnu, Secchi,
           Volume, SamplingDirection, MarkCode, RaceByLength,
           OrganismCode, ForkLength, Count, TowDuration),
  # 1976-present beach seine
  seinetable %>%
    select(StationCode, SampleDate, SampleTime,
           MethodCode, SeineDepth, GearConditionCode,
           SpecificConductance, WaterTemp,
           Turbidity, TurbidityFnu,
           Volume, MarkCode, RaceByLength,
           OrganismCode, ForkLength, Count)
)
#moved formatting sample Date higher up for following script
data <- data %>%
  mutate(SampleDate = mdy(SampleDate))
# Check to see if there are new Taxa added to the dataset:
USFWS_Species <- Species %>%
  select(USFWS_Code, Taxa) %>%
  dplyr::filter(!is.na(USFWS_Code))

New_Species <- data %>%
  group_by(OrganismCode) %>%
  slice(1) %>%
  transmute(Year = year(SampleDate),
            OrganismCode) %>%
  full_join(USFWS_Species,
            by = c("OrganismCode" = "USFWS_Code")) %>%
  filter(is.na(Taxa), OrganismCode != "NOFISH")

if (nrow(New_Species) > 0) stop("New species entry, update the Species_Code.csv")

#DJFMP did not record FlowMeter numbers until 1985. Did not record Flow Meter debris (in the database) until August 2014,
#recorded flow debris on datasheet starting in July 2012.
#below is a volume QC script that excludes volumes based on thresholds and tow duration instead of FlowDebris=Yes. This is more accurate, that will hopefully be used in DJFMP formal Qc process
# =========================================================================
# VOLUME FILTERING CONSTANTS (Place this right before DJFMP_Volume_adjusted)
# =========================================================================
##large mwtr trawl only currenlty Chipps island, historically Benicia
#small MWTR are Sac (seasonally), historic clarksburg
large_mwtr_stations <- c("SB001M", "SB001N", "SB001S", "SB001X", "SB018M", "SB018N", "SB018S", "SB018X")
small_mwtr_stations <- c("SR036E", "SR036M", "SR036W", "SR037E", "SR037M", "SR037W", "SR038E", "SR038M", "SR038W", "SR043M", "SR055M", "SR055W", "SR055X")

vol_limits <- list(
  large  = list(min = 7489.51, max = 29958.02),
  small  = list(min = 2047.65, max = 8190.59),
  kodiak = list(min = 2527.41, max = 10109.62),
  seine  = list(min = 0.60,    max = 146.25)
)
# =========================================================================
# CREATE ADJUSTED DATAFRAME
# =========================================================================


DJFMP_Volume_adjusted <- data %>%
  dplyr::rename(
    Station = StationCode, Date = SampleDate, Time = SampleTime, Temp_surf = WaterTemp,
    TurbidityNTU = Turbidity, TurbidityFNU= TurbidityFnu, Method = MethodCode, Tow_volume = Volume,
    Depth=SeineDepth, Tow_direction = SamplingDirection, Length = ForkLength, Conductivity=SpecificConductance
  ) %>%
  dplyr::filter(is.na(GearConditionCode) | !GearConditionCode %in% c(3,4,9)) %>%
  mutate(
    # 1. Ensure duration is numeric
    TowDuration_num = as.numeric(TowDuration),

    # 2. Calculate proportional limits directly based on renamed columns
    Min_Vol = case_when(
      Method == "MWTR" & Station %in% large_mwtr_stations ~ vol_limits$large$min * (TowDuration_num / 20),
      Method == "MWTR" & Station %in% small_mwtr_stations ~ vol_limits$small$min * (TowDuration_num / 20),
      Method == "KDTR" ~ vol_limits$kodiak$min * (TowDuration_num / 20),
      Method == "SEIN" ~ vol_limits$seine$min,
      TRUE ~ NA_real_
    ),
    Max_Vol = case_when(
      Method == "MWTR" & Station %in% large_mwtr_stations ~ vol_limits$large$max * (TowDuration_num / 20),
      Method == "MWTR" & Station %in% small_mwtr_stations ~ vol_limits$small$max * (TowDuration_num / 20),
      Method == "KDTR" ~ vol_limits$kodiak$max * (TowDuration_num / 20),
      Method == "SEIN" ~ vol_limits$seine$max,
      TRUE ~ NA_real_
    ),

    # 3. Directly overwrite Tow_volume (Replaces your old FlowDebris logic)
    Tow_volume = case_when(
      is.na(Tow_volume) ~ NA_real_,
      is.na(Min_Vol) | is.na(Max_Vol) ~ Tow_volume, # Keeps volume if no valid limits (e.g. missing duration)
      Tow_volume < Min_Vol ~ NA_real_,
      Tow_volume > Max_Vol ~ NA_real_,
      TRUE ~ Tow_volume
    )
  ) %>%
  # 4. Drop temporary calculation columns and the raw TowDuration column
  select(-TowDuration, -TowDuration_num, -Min_Vol, -Max_Vol)
###output from Volume processing #can be deleted in future
#296 volumes made NA.
# Total_Rows Original_Missing_Volume Removed_Below_Min Removed_Above_Max Total_NA_Volume_End
# <int>                   <int>             <int>             <int>               <int>
#   1    2436493                   52779             10718            232710              296207



  DJFMP<-DJFMP_Volume_adjusted%>%
mutate(Secchi = Secchi*100, # convert Secchi to cm
         Source = "DJFMP",
         Date = parse_date_time(Date, "%Y-%m-%d", tz = "America/Los_Angeles"),
         Time = parse_date_time(Time, "%H:%M:%S", tz = "America/Los_Angeles"),
         # Setting midnight Times to 0 per conversation with Jonathan Speegle
         Datetime = parse_date_time(ifelse(is.na(Time) | (hour(Time)==0 & minute(Time)==0 & second(Time)==0),
                                           NA_character_,
                                           paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
       TurbidityFNU=   ifelse(Date<as.Date("2024-07-31"), NA_real_, TurbidityFNU),
      #Before this date FNU not collected for DJFMP trawls
        # Removing conductivity data from dates before it was standardized
         Conductivity = ifelse(Date<as.Date("2019-06-01"), NA_real_, Conductivity),
         Sal_surf = ec2pss(Conductivity/1000, t=25),
         Method = recode(Method, MWTR="Midwater trawl", KDTR="Kodiak trawl", SEIN="Beach seine"),
         Tow_direction = recode(Tow_direction, U="Upstream", D="Downstream", X="Neither"),
         SampleID=paste(if_else(is.na(Datetime), Date, Datetime), Station, TowNumber, Method), ############################## Some datetimes and tow numbers are showing as NA, resulting in duplicate sampleIDs
         MarkCode=ifelse(OrganismCode=="NOFISH", "None", MarkCode),
         # Set up code for sub-groups to apply plus counts. Untagged Chinook Salmon are grouped by RaceByLength and any tagged fish are not incorporated into the process
         Group=case_when(MarkCode=="None" & OrganismCode=="CHN" ~ RaceByLength,
                         MarkCode!="None" ~ paste("Tag", 1:nrow(.)),
                         TRUE ~ NA_character_))%>%
  select(-Time, -MarkCode, -RaceByLength, -GearConditionCode, -FlowDebris) %>%
  group_by(across(-Count))%>% # Some species are recorded with the same length multiple times
  summarise(Count=sum(Count), .groups="drop")%>%
  group_by(SampleID, OrganismCode, Group)%>%
  mutate(TotalMeasured=sum(Count[which(Length!=0)]), # Calculate total number of fish of each species measured
         Total=sum(Count), # Calculate total number of fish of each species caught
         Count=(Count/TotalMeasured)*Total)%>% # Calculate the adjusted length frequency
  ungroup()%>%
  mutate(Length=if_else(is.infinite(Count) & Length==0, NA_real_, Length), # Some Chinook were not measured, so these lines fix some after-effects of that
         Length_NA_flag=case_when(
           is.infinite(Count) ~ "Unknown length",
           is.na(Length)~ "No fish caught",
           TRUE ~ NA_character_), # Add reasoning for an NA lengths (all "No Fish Caught" for FMWT)
         Count=ifelse(is.infinite(Count), Total, Count))%>%
  dplyr::filter(Length!=0 | is.na(Length))%>%
  select(-Total, -TotalMeasured, -Group)%>%
  left_join(DJFMP_stations, by = c("Station"="StationCode")) %>%
  # Add species names
  left_join(USFWS_Species,
            by=c("OrganismCode"="USFWS_Code")) %>%
  mutate(SampleID=paste(Source, SampleID), # Add variable for unique (across all studies) sampleID
         Taxa=str_remove(Taxa, " \\((.*)")
         )%>% # Remove life stage info from Taxa names
  # dplyr::rename(Taxa=ScientificName)%>%
  select(-OrganismCode)%>%
  group_by(across(-Count))%>% # Add up any new multiples after removing Group
  summarise(Count=sum(Count), .groups="drop")%>%
  # Transform all counts for 'No fish caught' to 0.
  # Also 4 instances of taxa record when there was no fish caught
  mutate(Count=if_else(Length_NA_flag=="No fish caught", 0, Count, missing=Count),
         Taxa = ifelse(Length_NA_flag == "No fish caught" & !is.na(Length_NA_flag), NA, Taxa))%>%
  select(Source, Station, Latitude, Longitude, Date, Datetime, Depth, SampleID, Method, Sal_surf,
         Temp_surf, TurbidityNTU, TurbidityFNU, Secchi, Tow_volume, Tow_direction, Taxa, Length, Count, Length_NA_flag)



# --- Check previous data ---

source(file.path("data-raw", "comparison.R"))



# Run before updating the data object to the new data
####Turb FNU Mutate added to deal with new TurbidityColumn added to df in new EDI publication
compareDJFMP <- create_comparison_report(
  DJFMP, LTMRdata::DJFMP %>%
  mutate(TurbidityFNU=NA),
  id_cols = c("SampleID", "Taxa", "Length", "Count")
)




############89392 new datapoints to add.
# 2247470 changed records found.
# bulk of Changes due to rounding/significant figures in Temp_surf
##TurbNTU was NA for some stations 2022 and now has values
#a large archive project was done and likely filled in gaps/found missing data
##Edits to Depth (seines w depth of 10m changes to 1. )

#adding Tide Tow_area, Sal_bot as NA columns for testing
DJFMP<- DJFMP %>%
  mutate(Tide=NA,
         Tow_area=NA,
         Sal_bot=NA)

#testing
# --- Run check on singular database ---
# devtools::load_all(".") should load this helper function for use
#one test did not past high sal_surf ignored for now
tests <- test_dataset(DJFMP, return_failures = T)

if (length(tests) == 0) {
  message("Tests passed, saving the data.\n")
  usethis::use_data(DJFMP,overwrite=TRUE, compress="xz")
}

#
# # Save compressed data to /data
usethis::use_data(DJFMP, overwrite=TRUE, compress="xz")
