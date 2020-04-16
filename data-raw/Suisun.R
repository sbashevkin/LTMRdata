## code to prepare `Suisun` dataset goes here

require(readr)
require(dplyr)
require(wql)
require(lubridate)
require(LTMRdata)

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
  mutate(StandardLength=if_else(is.na(Taxa), NA_real_, StandardLength))%>% #Trying to retain samples in which no fish were caught
  mutate(CatchComments=if_else(SampleID=="Suisun 1536" & Taxa=="Gobiidae" & StandardLength==0, "larval", CatchComments))

# This approach assumes that if a record of an unmeasured fish has a comment, there are no other unmeasured fish in that sample of the same species without a comment.
# Currently, as of 4/15/20, this is true.
#First have to manually translate the catch comments from the below exported csv into the format of the Suisun comments.xlsx
#write_csv(filter(catch_suisun, StandardLength==0 & !is.na(CatchComments)), "~/Suisun comments.csv")

catch_fix<-catch_suisun%>%
  select(Taxa, StandardLength, Count, SampleID)%>%
  filter(StandardLength>0)

catch_comments_suisun <- read_excel(file.path("data-raw", "Suisun", "Suisun comments.xlsx"))%>%
  filter(is.na(Ignore))%>%
  select(SampleID, Taxa, Count, CatchComments, Min_length, Length, Max_length, Lifestage, Notes)%>%
  mutate(NA_length = if_else(is.na(Min_length) & is.na(Max_length) & is.na(Length), TRUE, FALSE))

sizegroups_suisun<-catch_comments_suisun%>%
  rename(Unmeasured=Count)%>%
  filter(!is.na(Min_length) | !is.na(Max_length))%>%
  mutate(Min_length=if_else(is.na(Min_length), -Inf, Min_length),
         Max_length=if_else(is.na(Max_length), Inf, Max_length),
         RowNum=2:(n()+1))%>%
  nest_join(catch_fix,
            by=c("Taxa", "SampleID"))%>%
  rowwise()%>%
  mutate(catch_fix = list(mutate(catch_fix, SizeGroup=if_else(StandardLength<Max_length & StandardLength>Min_length, RowNum, as.integer(1)))))%>%
  mutate(NA_length=if_else(nrow(catch_fix)==0, TRUE, FALSE))%>%
  ungroup()%>%
  unnest(cols="catch_fix", keep_empty = TRUE)

catch_comments_suisun2<-sizegroups_suisun%>%
  select(-SizeGroup, -Count, -StandardLength, -Notes, -Lifestage, -Max_length, -Min_length)%>%
  rename(Count=Unmeasured)%>%
  distinct()%>%
  bind_rows(catch_comments_suisun%>%
              filter(is.na(Min_length) & is.na(Max_length))%>%
              select(-Notes, -Lifestage, -Max_length, -Min_length))%>%
  mutate(Length=case_when(
    !is.na(Length) ~ Length,
    NA_length ~ NA_real_,
    TRUE ~ 0),
    RowNum=replace_na(RowNum, 1))%>%
  rename(SizeGroup=RowNum, StandardLength=Length)%>%
  left_join(catch_suisun%>%
              select(-StandardLength, -Count, -Dead, -CatchComments)%>%
              distinct(),
            by=c("SampleID", "Taxa"))%>%
  mutate(ID=paste(SampleID, Taxa))%>%
  select(-NA_length)

catch_suisun2<-catch_suisun%>%
  left_join(sizegroups_suisun%>%
              filter(RowNum==SizeGroup)%>%
              select(SampleID, Taxa, StandardLength, Count, SizeGroup),
            by=c("SampleID", "Taxa", "StandardLength", "Count"))%>%
  mutate(SizeGroup=replace_na(SizeGroup, 1))%>%
  mutate(ID=paste(SampleID, Taxa))%>%
  filter(!(ID%in%unique(catch_comments_suisun2$ID) & StandardLength==0))%>%
  bind_rows(catch_comments_suisun2)

#Need to remove fixed_length_suisun records from catch_suisun but add back in the measured fish that were removed from catch_comments_suisun$catch_fix because they didn't fit into the min and max. Also need to preserve catch_suisun for suisun_measured_lengths

Suisun <- catch_suisun2%>%
  filter(StandardLength!=0 | is.na(StandardLength))%>% #Remove unmeasured fish, but not all of these 0s seem to be unmeasured according to the Catch Comments so these should be inspected. The is.na part is Trying to retain samples in which no fish were caught
  group_by(SampleID, Taxa, SizeGroup)%>%
  mutate(TotalMeasured=sum(Count, na.rm=T))%>%
  ungroup()%>%
  left_join(catch_suisun2%>%
              select(SampleID, Taxa, Count, SizeGroup)%>%
              group_by(SampleID, Taxa, SizeGroup)%>%
              summarise(TotalCatch=sum(Count, na.rm=T))%>%
              ungroup(),
            by=c("SampleID", "Taxa", "SizeGroup"))%>%
  mutate(Count = (Count/TotalMeasured)*TotalCatch,
         Sal_surf=ec2pss(Conductivity/1000, t=25))%>%
  select(-Conductivity, -QADone, -TotalMeasured, -TotalCatch, -Dead, -SampleRowID, -SizeGroup, -ID)%>%
  rename(Length=StandardLength, Notes_catch=CatchComments, Tow_duration=TowDuration, Notes_tow=TrawlComments,
         DO_concentration=DO, DO_saturation=PctSaturation, Temp_surf=Temperature)%>%
  select(-DO_concentration, -DO_saturation) # Remove extra environmental variables

Suisun_measured_lengths <- catch_suisun2%>%
  filter(StandardLength!=0)%>%
  select(SampleID, Taxa, Dead, Length=StandardLength, Count)

usethis::use_data(Suisun, Suisun_measured_lengths, overwrite=TRUE)
