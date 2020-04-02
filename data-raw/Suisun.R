## code to prepare `Suisun` dataset goes here

library(readr)
library(dplyr)
library(lubridate)

depth_suisun <- read_csv(file.path("data-raw", "Suisun", "Depth.csv"),
                         col_types=cols_only(SampleRowID="c", Depth="d"))%>%
  group_by(SampleRowID)%>%
  summarise(Depth=mean(Depth, na.rm=T))%>%
  ungroup()


stations_suisun <- read_csv(file.path("data-raw", "Suisun", "StationsLookUp.csv"),
                 col_types=cols_only(StationCode="c", x_WGS84="d", y_WGS84="d"))%>%
  rename(Longitude=x_WGS84, Latitude=y_WGS84, Station=StationCode)

effort_suisun <- read_csv(file.path("data-raw", "Suisun", "TrawlEffort.csv"),
                        col_types = cols_only(SampleRowID="c", TowDuration="d", TrawlComments="c",
                                              StartMeter="d", EndMeter="d"))

#Removing salinity because data do not correspond well with conductivity
sample_suisun<-read_csv(file.path("data-raw", "Suisun", "Sample.csv"),
                    col_types = cols_only(SampleRowID="c", MethodCode="c", StationCode="c", SampleDate="c", SampleTime="c",
                                          QADone="l", WaterTemperature="d", DO="d", PctSaturation="d",
                                          Secchi="d", SpecificConductance="d", TideCode="c"))%>%
  rename(Station=StationCode, Date=SampleDate, Time=SampleTime,
         Temperature=WaterTemperature, Conductivity=SpecificConductance,
         Tide=TideCode, Method=MethodCode)%>%
  filter(Method%in%c("MWTR", "OTR"))%>% #Only included midwater trawl and otter trawl data
  mutate(Date=parse_date_time(Date, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"),
         Time=parse_date_time(Time, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"),
         Method=recode(Method, MWTR="Midwater trawl", OTR="Otter trawl"))%>%
  mutate(Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>%
  select(-Time)%>%
  mutate(Tide=recode(Tide, flood="Flood", ebb="Ebb", low="Low Slack", high="High Slack", outgoing="Ebb", incoming="Flood"),
         Source="Suisun")%>%
  left_join(stations_suisun,
            by="Station")%>%
  left_join(depth_suisun,
            by="SampleRowID")%>%
  left_join(effort_suisun,
            by="SampleRowID")

species_suisun <- read_csv(file.path("data-raw", "Suisun", "OrganismsLookUp.csv"),
                           col_types = cols_only(OrganismCode="c", CommonName="c"))

Suisun <- read_csv(file.path("data-raw", "Suisun", "Catch.csv"), na=c("NA", "n/p"),
                         col_types = cols_only(SampleRowID="c", OrganismCode="c", StandardLength="d",
                                               Dead="c", Count="d", CatchComments="c"))%>%
  inner_join(sample_suisun,
             by="SampleRowID")%>% # Inner join to remove other methods
  left_join(species_suisun,
            by="OrganismCode")%>%
  mutate(Count = if_else(SampleRowID=="{8327B645-BC36-4405-ADB3-C6561718A17B}" & StandardLength==87, Count+1, Count))%>% # Correcting for misstyped data point per email from Teejay that
  filter(!(!QADone & CommonName=="Splittail" & StandardLength==8))%>% # all QADone==FALSE data from January 2007 are correct EXCEPT for that lone splittail measuring 8 mm (was actually 87 mm).
  select(-SampleRowID)

usethis::use_data(Suisun, overwrite=TRUE)
