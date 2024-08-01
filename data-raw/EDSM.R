
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

# downloading data because the dataset is too huge to keep on file
# start pipeline to edi
# relational tables

# Find the newest revision
# IF you want to pull a specific version of a package, which is a number
version <- NA

link <- ifelse(is.na(version), "https://pasta.lternet.edu/package/eml/edi/415/newest",
               paste0("https://pasta.lternet.edu/package/eml/edi/415/", version))

tableLinks <- read.delim(link, header = F) %>%
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

TMMtable<-tableNames %>%
  filter(grepl("20mm", name))

KDTRtable<-tableNames %>%
  filter(grepl("KDTR", name))

# Make sure URLs exist for each table
if(any(nrow(TMMtable)==0,
       nrow(KDTRtable)==0)){
  stop("regex for URLs isn't working right")
}

# Want only the 20mm and kdtr data
EDSM <- bind_rows(
  read_csv(TMMtable%>%
             pull(url),
           col_types = cols_only(StationCode = "c", SampleDate = "c", SampleTime = "c", Tide = "c",
                                 LongitudeStart = "d", LatitudeStart = "d", TowNumber="d",
                                 GearConditionCode = "i", FlowDebris = "c",
                                 SpecificConductanceTop = "d", WaterTempTop = "d",
                                 TurbidityTop = "d", TurbidityBottom = "d", Secchi = "d",
                                 BottomDepth = "d", Volume = "d", SamplingDirection = "c", MethodCode = "c",
                                 OrganismCode = "c", ForkLength = "d", Count = "d",
                                 MarkCode="c", RaceByLength="c")) %>%
    mutate(SampleDate = parse_date_time(SampleDate, "%m-%d-%Y", tz = "America/Los_Angeles")) %>%
    rename(Turbidity = TurbidityTop),
  read_csv(KDTRtable%>%
             pull(url),
           col_types = cols_only(StationCode = "c", SampleDate = "c", SampleTime = "c", Tide = "c",
                                 LongitudeStart = "d", LatitudeStart = "d", TowNumber="d",
                                 SpecificConductance = "d", WaterTemp = "d",
                                 Turbidity = "d", Secchi = "d", BottomDepth = "d",
                                 GearConditionCode = "i", FlowDebris = "c",
                                 Volume = "d", SamplingDirection = "c", MethodCode = "c",
                                 OrganismCode = "c", ForkLength = "d", Count = "d",
                                 MarkCode="c", RaceByLength="c"))%>%
    mutate(SampleDate = parse_date_time(SampleDate, "%Y-%m-%d", tz = "America/Los_Angeles")) %>%
    rename(SpecificConductanceTop=SpecificConductance, WaterTempTop=WaterTemp))%>%
  rename(Temp_surf = WaterTempTop, Tow_volume = Volume, Method = MethodCode,
         Tow_direction = SamplingDirection, Length = ForkLength,
         TurbidityNTU = Turbidity, TurbidityBottomNTU = TurbidityBottom,
         Conductivity = SpecificConductanceTop,
         Latitude=LatitudeStart, Longitude=LongitudeStart,
         Date = SampleDate, Time = SampleTime, Depth = BottomDepth, Station = StationCode, Tow = TowNumber) %>%
  dplyr::filter(is.na(GearConditionCode) | !GearConditionCode%in%c(3,4,9))%>%
  mutate(Tow_volume = if_else(FlowDebris%in%c("Y", "Yes"), NA_real_, Tow_volume, missing=Tow_volume),
         Tow_volume = if_else(Tow_volume==0, NA_real_, Tow_volume),
         Source = "EDSM",
         # Date = parse_date_time(Date, "%Y-%m-%d", tz = "America/Los_Angeles"),
         Time = parse_date_time(Time, "%H:%M:%S", tz = "America/Los_Angeles"),
         Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
         # Removing conductivity data from dates before it was standardized
         Conductivity = if_else(Date<parse_date_time("2019-06-01", "%Y-%m-%d", tz="America/Los_Angeles"), NA_real_, Conductivity),
         Sal_surf = ec2pss(Conductivity/1000, t=25),
         Sal_surf = case_when(Sal_surf<0 & Sal_surf>(-0.1)~ 0,
                              Sal_surf>40 ~ NA_real_,
                              TRUE ~ Sal_surf),
         # Updating recode (superseded as of dplyr 1.1.2) to case_when()
         # Using regex to make it more generic
         Method = case_when(grepl("^K(D)?T(R|S\\d+)", Method) ~ "Kodiak trawl",
                            Method %in% "20mm" ~ "20mm net"),
         # recode(Method, KDTR="Kodiak trawl", `20mm`="20mm net"),
         Tow_direction = recode(Tow_direction, U="Upstream", D="Downstream"),
         Depth = if_else(Method=="20mm net", Depth*0.3048, Depth), # Convert feet to meters for 20mm (KDTR already in meters)
         Secchi = Secchi*100, # convert Secchi to cm
         Tide=recode(Tide, HS="High Slack", LS = "Low Slack"), #Standardize tide codes
         SampleID=paste(Datetime, Station, Tow, Method, Latitude, Longitude),
         MarkCode=replace_na(MarkCode, "None"),
         Group=case_when(MarkCode=="None" & OrganismCode=="CHN" ~ RaceByLength,
                         MarkCode!="None" ~ paste("Tag", 1:nrow(.)),
                         TRUE ~ NA_character_),
         Count=if_else(OrganismCode=="NOFISH", NA_real_, Count))%>%
  select(-Time, -MarkCode, -RaceByLength, -GearConditionCode, -FlowDebris) %>%
  group_by(across(-Count))%>% # Some species are recorded with the same length multiple times
  summarise(Count=sum(Count), .groups="drop")%>%
  group_by(SampleID, OrganismCode, Group)%>%
  mutate(TotalMeasured=sum(Count[which(Length!=0)]), # Calculate total number of fish of each species measured
         Total=sum(Count), # Calculate total number of fish of each species caught
         Count=(Count/TotalMeasured)*Total)%>% # Calculate the adjusted length frequency
  ungroup()%>%
  mutate(Length=if_else((is.infinite(Count) & Length==0) | OrganismCode=="NOFISH", NA_real_, Length), # Some Chinook were not measured, so these lines fix some after-effects of that
         Length_NA_flag=case_when(
           is.infinite(Count) ~ "Unknown length",
           is.na(Length)~ "No fish caught",
           TRUE ~ NA_character_), # Add reasoning for NA lengths
         Count=if_else(is.infinite(Count), Total, Count))%>% # These cases all represent the only row of that SamppleID, OrganismCode, and Group, so this doesn't result in over-counting, it just returns the value to the prior count
  dplyr::filter(Length!=0 | is.na(Length))%>%
  dplyr::filter(Count!=0 | is.na(Count))%>% # Remove 1 case of a 0 count of a striped bass, *****NEED TO CHECK IN UPDATES*****
  select(-Total, -TotalMeasured, -Group)%>%
  group_by(across(-Count))%>% # Add up any new multiples after removing Group
  summarise(Count=sum(Count), .groups="drop")%>%
  group_by(SampleID)%>% # Now we need to remove any NOFISH records when there are actually fish counts in that sample (including next 3 lines)
  mutate(Valid=sum(Count, na.rm=T))%>%
  ungroup()%>%
  dplyr::filter(!(Valid>0 & OrganismCode=="NOFISH"))%>%
  left_join(Species %>%
              select(USFWS_Code, Taxa) %>%
              dplyr::filter(!is.na(USFWS_Code)),
            by=c("OrganismCode"="USFWS_Code")) %>%
  mutate(SampleID=paste(Source, SampleID), # Add variable for unique (across all studies) sampleID
         Taxa=str_remove(Taxa, " \\((.*)"), # Remove life stage info from Taxa names
         Count=if_else(Length_NA_flag=="No fish caught", 0, Count, missing=Count))%>% # Transform all counts for 'No fish caught' to 0.
  select(Source, Station, Latitude, Longitude, Date, Datetime, Depth, SampleID, Method, Tide, Sal_surf,
         Temp_surf, TurbidityNTU,
         # TurbidityBottomNTU, # Can include this back in if in the future more than just EDSM collects this data
         Secchi, Tow_volume, Tow_direction, Taxa, Length, Count, Length_NA_flag)%>%
  # Remove NA gear types
  # As of 07/19/2024, there are two instances in the KDTR dataset. Claudia MacFarlane
  # could not confirm which gear type was used for these instances. Removing for now
  filter(!is.na(Method))

# Save compressed data to /data
usethis::use_data(EDSM, overwrite=TRUE, compress = "xz")
