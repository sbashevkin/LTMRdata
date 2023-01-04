
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
utils::download.file("https://pasta.lternet.edu/package/data/eml/edi/415/5/d468c513fa69c4fc6ddc02e443785f28", file.path(tempdir(), "EDSM_20mm.csv"), mode="wb",method="libcurl")
utils::download.file("https://pasta.lternet.edu/package/data/eml/edi/415/5/4d7de6f0a38eff744a009a92083d37ae", file.path(tempdir(), "EDSM_KDTR.csv"), mode="wb",method="libcurl")

testlink<-rvest::read_html("https://www.fws.gov/media/edsm-daily-report-0")
test<-testlink%>%html_elements("span")%>%html_text()
test2<-paste("https://www.fws.gov/sites/default/files/documents/",test[77],sep="")
utils::download.file(test2, file.path(tempdir(), "EDSM_recent.xlsx"), mode="wb",method="libcurl")
test3<-read_excel(file.path(tempdir(),"EDSM_recent.xlsx"))

EDSM <- bind_rows(
  read_csv(file.path(tempdir(), "EDSM_20mm.csv"),
           col_types = cols_only(Station = "c", Date = "c", Time = "c", Tide = "c",
                                 StartLong = "d", StartLat = "d", Tow="d",
                                 GearConditionCode = "i", Debris = "c",
                                 TopEC = "d", TopTemp = "d", Scchi = "d", Depth = "d",
                                 Volume = "d", Dir = "c", GearType = "c",
                                 OrganismCode = "c", ForkLength = "d", SumOfCatchCount = "d",
                                 MarkCode="c", RaceByLength="c")),
  read_csv(file.path(tempdir(), "EDSM_KDTR.csv"),
           col_types = cols_only(Station = "c", Date = "c", Time = "c", Tide = "c",
                                 StartLong = "d", StartLat = "d", Tow="d",
                                 EC = "d", Temp = "d", Scchi = "d", StartDepth = "d",
                                 GearConditionCode = "i", Debris = "c",
                                 Volume = "d", Dir = "c", GearType = "c",
                                 OrganismCode = "c", ForkLength = "d", SumOfCatchCount = "d",
                                 MarkCode="c", RaceByLength="c"))%>%
    rename(TopEC=EC, TopTemp=Temp, Depth=StartDepth))%>%
  rename(Temp_surf = TopTemp, Tow_volume = Volume, Method = GearType, Secchi = Scchi,
         Tow_direction = Dir, Length = ForkLength, Conductivity = TopEC, Count = SumOfCatchCount,
         Latitude=StartLat, Longitude=StartLong) %>%
  dplyr::filter(is.na(GearConditionCode) | !GearConditionCode%in%c(3,4,9))%>%
  mutate(Tow_volume = if_else(Debris%in%c("Y", "Yes"), NA_real_, Tow_volume, missing=Tow_volume),
         Source = "EDSM",
         Date = parse_date_time(Date, "%Y-%m-%d", tz = "America/Los_Angeles"),
         Time = parse_date_time(Time, "%H:%M:%S", tz = "America/Los_Angeles"),
         Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
         # Removing conductivity data from dates before it was standardized
         Conductivity = if_else(Date<parse_date_time("2019-06-01", "%Y-%m-%d", tz="America/Los_Angeles"), NA_real_, Conductivity),
         Sal_surf = ec2pss(Conductivity/1000, t=25),
         Method = recode(Method, KDTR="Kodiak trawl", `20mm`="20mm net"),
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
  select(-Time, -MarkCode, -RaceByLength, -GearConditionCode, -Debris) %>%
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
         Temp_surf, Secchi, Tow_volume, Tow_direction, Taxa, Length, Count, Length_NA_flag)

# Save compressed data to /data
usethis::use_data(EDSM, overwrite=TRUE, compress = "xz")
