## code to prepare `Suisun` dataset goes here

library(readr)
library(dplyr)
library(wql)
library(lubridate)
library(LTMRdata)

depth_suisun <- read_csv(file.path("data-raw", "Suisun", "Depth.csv"),
                         col_types=cols_only(SampleRowID="c", Depth="d"))%>%
  group_by(SampleRowID)%>%
  summarise(Depth=mean(Depth, na.rm=T))%>%
  ungroup()


stations_suisun <- read_csv(file.path("data-raw", "Suisun", "StationsLookUp.csv"),
                            col_types=cols_only(StationCode="c", x_WGS84="d", y_WGS84="d"))%>%
  rename(Longitude=x_WGS84, Latitude=y_WGS84, Station=StationCode)

effort_suisun <- read_csv(file.path("data-raw", "Suisun", "TrawlEffort.csv"),
                          col_types = cols_only(SampleRowID="c", TowDuration="d", TrawlComments="c"))%>%
  mutate(Tow_area = (TowDuration/60)*4*1000*4.3) # ((TowDuration minutes) / (60 minutes/hour)) * 4km/hour towing speed * 1000 m/km * 4.3 m net width

#Removing salinity because data do not correspond well with conductivity
sample_suisun <- read_csv(file.path("data-raw", "Suisun", "Sample.csv"),
                        col_types = cols_only(SampleRowID="c", MethodCode="c", StationCode="c", SampleDate="c", SampleTime="c",
                                              QADone="l", WaterTemperature="d", DO="d", PctSaturation="d",
                                              Secchi="d", SpecificConductance="d", TideCode="c"))%>%
  rename(Station=StationCode, Date=SampleDate, Time=SampleTime,
         Temperature=WaterTemperature, Conductivity=SpecificConductance,
         Tide=TideCode, Method=MethodCode)%>%
  mutate(Method=recode(Method, MWTR="Midwater trawl", OTR="Otter trawl"))%>%
  filter(Method=="Otter trawl")%>% #Only included midwater trawl and otter trawl data # I think we're excluding midwater?
  mutate(Date=parse_date_time(Date, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"),
         Time=parse_date_time(Time, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
  mutate(Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>%
  select(-Time)%>%
  mutate(Tide=recode(Tide, flood="Flood", ebb="Ebb", low="Low Slack", high="High Slack", outgoing="Ebb", incoming="Flood"),
         Source="Suisun",
         SampleID=paste(Source, 1:nrow(.)))%>%
  left_join(stations_suisun,
            by="Station")%>%
  left_join(depth_suisun,
            by="SampleRowID")%>%
  left_join(effort_suisun,
            by="SampleRowID")

catch_suisun <- read_csv(file.path("data-raw", "Suisun", "Catch.csv"), na=c("NA", "n/p"),
                         col_types = cols_only(SampleRowID="c", OrganismCode="c", StandardLength="d",
                                               Dead="c", Count="d", CatchComments="c"))%>%
  mutate(CatchComments=na_if(CatchComments, ""))%>%
  right_join(sample_suisun,
             by="SampleRowID")%>%
  filter(Method=="Otter trawl" & OrganismCode!="NOTRAWL")%>% # I think we're excluding midwater? Also excluding samples with no trawl.
  left_join(Species%>%
              select(OrganismCode=SMF_Code, Taxa)%>%
              filter(!is.na(OrganismCode)),
            by="OrganismCode")%>%
  select(-OrganismCode)%>%
  mutate(Count = if_else(SampleRowID=="{8327B645-BC36-4405-ADB3-C6561718A17B}" & StandardLength==87, Count+1, Count))%>% # Correcting for misstyped data point per email from Teejay that
  filter(!(!QADone & Taxa=="Pogonichthys macrolepidotus" & StandardLength==8))%>% # all QADone==FALSE data from January 2007 are correct EXCEPT for that lone splittail measuring 8 mm (was actually 87 mm).
  mutate(StandardLength=if_else(is.na(Taxa), NA_real_, StandardLength)) #Trying to retain samples in which no fish were caught


Suisun <- catch_suisun%>%
  filter(StandardLength!=0 | is.na(StandardLength))%>% #Remove unmeasured fish, but not all of these 0s seem to be unmeasured according to the Catch Comments so these should be inspected. The is.na part is Trying to retain samples in which no fish were caught
  group_by(SampleRowID, Taxa)%>%
  mutate(TotalMeasured=sum(Count, na.rm=T))%>%
  ungroup()%>%
  left_join(catch_suisun%>%
              select(SampleRowID, Taxa, Count)%>%
              group_by(SampleRowID, Taxa)%>%
              summarise(TotalCatch=sum(Count, na.rm=T))%>%
              ungroup(),
            by=c("SampleRowID", "Taxa"))%>%
  mutate(Count = (Count/TotalMeasured)*TotalCatch)%>%
  select(-SampleRowID)%>%
  mutate(Sal_surf=ec2pss(Conductivity/1000, t=25))%>%
  select(-Conductivity, -QADone, -TotalMeasured, -TotalCatch, -Dead)%>%
  rename(Length=StandardLength, Notes_catch=CatchComments, Tow_duration=TowDuration, Notes_tow=TrawlComments,
         DO_concentration=DO, DO_saturation=PctSaturation, Temp_surf=Temperature)%>%
  select(-DO_concentration, -DO_saturation) # Remove extra environmental variables

Suisun_measured_lengths <- catch_suisun%>%
  filter(StandardLength!=0)%>%
  select(SampleID, Taxa, Dead, Length=StandardLength, Count)

usethis::use_data(Suisun, Suisun_measured_lengths, overwrite=TRUE)
