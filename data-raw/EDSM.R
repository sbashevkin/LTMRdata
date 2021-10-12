
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

# downloading data because the dataset is too huge to keep on file

download.file("https://pasta.lternet.edu/package/data/eml/edi/415/5/d468c513fa69c4fc6ddc02e443785f28", file.path(tempdir(), "EDSM_20mm.csv"), mode="wb",method="libcurl")
download.file("https://pasta.lternet.edu/package/data/eml/edi/415/5/4d7de6f0a38eff744a009a92083d37ae", file.path(tempdir(), "EDSM_KDTR.csv"), mode="wb",method="libcurl")


EDSM <- bind_rows(
  read_csv(file.path(tempdir(), "EDSM_20mm.csv"),
           col_types = cols_only(Station = "c", Date = "c", Time = "c", Tide = "c",
                                 StartLong = "d", StartLat = "d", Tow="d",
                                 TopEC = "d", TopTemp = "d", Scchi = "d", Depth = "d",
                                 Volume = "d", Dir = "c", GearType = "c",
                                 OrganismCode = "c", ForkLength = "d", SumOfCatchCount = "d",
                                 MarkCode="c", RaceByLength="c")),
  read_csv(file.path(tempdir(), "EDSM_KDTR.csv"),
           col_types = cols_only(Station = "c", Date = "c", Time = "c", Tide = "c",
                                 StartLong = "d", StartLat = "d", Tow="d",
                                 EC = "d", Temp = "d", Scchi = "d", StartDepth = "d",
                                 Volume = "d", Dir = "c", GearType = "c",
                                 OrganismCode = "c", ForkLength = "d", SumOfCatchCount = "d",
                                 MarkCode="c", RaceByLength="c"))%>%
    rename(TopEC=EC, TopTemp=Temp, Depth=StartDepth))%>%
  rename(Temp_surf = TopTemp, Tow_volume = Volume, Method = GearType, Secchi = Scchi,
         Tow_direction = Dir, Length = ForkLength, Conductivity = TopEC, Count = SumOfCatchCount,
         Latitude=StartLat, Longitude=StartLong) %>%
  mutate(Source = "EDSM",
         Date = parse_date_time(Date, "%Y-%m-%d", tz = "America/Los_Angeles"),
         Time = parse_date_time(Time, "%H:%M:%S", tz = "America/Los_Angeles"),
         Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
         # Removing conductivity data from dates before it was standardized
         Conductivity = if_else(Date<parse_date_time("2019-06-01", "%Y-%m-%d", tz="America/Los_Angeles"), NA_real_, Conductivity),
         Sal_surf = ec2pss(Conductivity/1000, t=25),
         Method = recode(Method, KDTR="Kodiak trawl"),
         Tow_direction = recode(Tow_direction, U="Upstream", D="Downstream"),
         Depth = if_else(Method=="20mm", Depth*0.3048, Depth), # Convert feet to meters for 20mm (KDTR already in meters)
         Secchi = Secchi*100, # convert Secchi to cm
         Tide=recode(Tide, HS="High Slack", LS = "Low Slack"), #Standardize tide codes
         SampleID=paste(Datetime, Station, Tow, Method, Latitude, Longitude),
         MarkCode=replace_na(MarkCode, "None"),
         Group=case_when(MarkCode=="None" & OrganismCode=="CHN" ~ RaceByLength,
                         MarkCode!="None" ~ paste("Tag", 1:nrow(.)),
                         TRUE ~ NA_character_))%>%
  select(-Time, -MarkCode, -RaceByLength) %>%
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
         Count=if_else(is.infinite(Count), Total, Count))%>%
  filter(Length!=0 | is.na(Length))%>%
  select(-Total, -TotalMeasured, -Group)%>%
  group_by(across(-Count))%>% # Add up any new multiples after removing Group
  summarise(Count=sum(Count), .groups="drop")%>%
  left_join(Species %>%
              select(USFWS_Code, Taxa) %>%
              filter(!is.na(USFWS_Code)),
            by=c("OrganismCode"="USFWS_Code")) %>%
  mutate(SampleID=paste(Source, SampleID), # Add variable for unique (across all studies) sampleID
         Taxa=str_remove(Taxa, " \\((.*)"))%>% # Remove life stage info from Taxa names
  select(Source, Station, Latitude, Longitude, Date, Datetime, Depth, SampleID, Method, Tide, Sal_surf,
         Temp_surf, Secchi, Tow_volume, Tow_direction, Taxa, Length, Count, Length_NA_flag)

# Save compressed data to /data
usethis::use_data(EDSM, overwrite=TRUE, compress = "xz")
