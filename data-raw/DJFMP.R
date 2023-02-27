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

# downloading data because the dataset is too huge to keep on file
options(timeout = 99999)

# Find the newest revision
tableLinks <- read.delim("https://pasta.lternet.edu/package/eml/edi/244/newest", header = F) %>%
  .[[1]] %>%
  .[which(grepl("/data/", .))]

tableNames <- lapply(tableLinks, function(x) {
  entityName <- read_html(gsub("data", "name", x)) %>%
    html_text()

  data.frame(id = gsub(".*\\/", "", x),
             name = entityName,
             url = x)
}) %>%
  bind_rows()

# Want the trawl/seine data and the site locations
data <- tableNames %>%
  filter(grepl("fish_and_water_quality_data", name) | grepl("Site_Locations", name)) %>%
  pull(url) %>%
  lapply(read.csv) %>%
  setNames(tableNames %>%
             filter(grepl("fish_and_water_quality_data", name) | grepl("Site_Locations", name)) %>%
             pull(name))

DJFMP_station<-data$DJFMP_Site_Locations.csv%>%
  select(StationCode,Latitude,Longitude)%>%
  rename(Station=StationCode)

DJFMP<-bind_rows(data$`1976-2001_DJFMP_trawl_fish_and_water_quality_data.csv`%>%
                   select(StationCode,SampleDate,SampleTime,
                              TowNumber,MethodCode,GearConditionCode,FlowDebris,
                              SpecificConductance,WaterTemp, Secchi, SeineDepth,
                              Volume, SamplingDirection, MarkCode, RaceByLength,
                              OrganismCode,ForkLength,Count),
                 data$`2002-2022_DJFMP_trawl_fish_and_water_quality_data.csv`%>%
                   select(StationCode,SampleDate,SampleTime,
                              TowNumber,MethodCode,GearConditionCode,FlowDebris,
                              SpecificConductance,WaterTemp, Secchi, SeineDepth,
                              Volume, SamplingDirection, MarkCode, RaceByLength,
                              OrganismCode,ForkLength,Count),
                 data$`1976-2022_DJFMP_beach_seine_fish_and_water_quality_data.csv`%>%
                   select(StationCode,SampleDate,SampleTime,
                              TowNumber,MethodCode,GearConditionCode,FlowDebris,
                              SpecificConductance,WaterTemp, Secchi, SeineDepth,
                              Volume, SamplingDirection, MarkCode, RaceByLength,
                              OrganismCode,ForkLength,Count))

DJFMP<-DJFMP%>%
  rename(Station = StationCode, Date = SampleDate, Time = SampleTime, Temp_surf = WaterTemp,
         Method = MethodCode, Tow_volume = Volume, Depth=SeineDepth,
         Tow_direction = SamplingDirection, Length = ForkLength,Conductivity=SpecificConductance) %>%
  dplyr::filter(is.na(GearConditionCode) | !GearConditionCode%in%c(3,4,9))%>%
  mutate(Tow_volume = if_else(FlowDebris=="Y", NA_real_, Tow_volume, missing=Tow_volume),
         Secchi = Secchi*100, # convert Secchi to cm
         Source = "DJFMP",
         #Date = parse_date_time(Date, "%Y-%m-%d", tz = "America/Los_Angeles"),
         Time = parse_date_time(Time, "%H:%M:%S", tz = "America/Los_Angeles"),
         Datetime = parse_date_time(ifelse(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
         # Removing conductivity data from dates before it was standardized
         Conductivity = ifelse(Date<as.Date("2019-06-01"), NA_real_, Conductivity),
         Sal_surf = ec2pss(Conductivity/1000, t=25),
         Method = recode(Method, MWTR="Midwater trawl", KDTR="Kodiak trawl", SEIN="Beach seine"),
         Tow_direction = recode(Tow_direction, U="Upstream", D="Downstream", X="Neither"),
         SampleID=paste(Datetime, Station, TowNumber, Method),
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
  left_join(DJFMP_station, by = "Station") %>%
  # Add species names
  left_join(Species %>%
              select(USFWS_Code, ScientificName) %>%
              dplyr::filter(!is.na(USFWS_Code)),
            by=c("OrganismCode"="USFWS_Code")) %>%
  mutate(SampleID=paste(Source, SampleID), # Add variable for unique (across all studies) sampleID
         #Taxa=str_remove(Taxa, " \\((.*)")
         )%>% # Remove life stage info from Taxa names
  rename(Taxa=ScientificName)%>%
  select(-OrganismCode)%>%
  group_by(across(-Count))%>% # Add up any new multiples after removing Group
  summarise(Count=sum(Count), .groups="drop")%>%
  mutate(Count=if_else(Length_NA_flag=="No fish caught", 0, Count, missing=Count))%>% # Transform all counts for 'No fish caught' to 0.
  select(Source, Station, Latitude, Longitude, Date, Datetime, Depth, SampleID, Method, Sal_surf,
         Temp_surf, Secchi, Tow_volume, Tow_direction, Taxa, Length, Count, Length_NA_flag)



# Save compressed data to /data
usethis::use_data(DJFMP, overwrite=TRUE)
