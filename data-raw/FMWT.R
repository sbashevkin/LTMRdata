## code to prepare `FMWT` dataset goes here
require(readr)
require(wql)
require(dplyr)
require(tidyr)
require(lubridate)
require(LTMRdata)
require(stringr)


# Station locations -------------------------------------------------------

stations_fmwt <- read_csv(file.path("data-raw", "FMWT", "StationsLookUp.csv"),
                          col_types = cols_only(StationCode="c", DD_Latitude="d", DD_Longitude="d"))%>%
  rename(Station=StationCode, Latitude=DD_Latitude, Longitude=DD_Longitude)%>%
  drop_na()

# Sample-level data -------------------------------------------------------


sample_fmwt <- read_csv(file.path("data-raw", "FMWT", "Sample.csv"),
                        col_types = cols_only(SampleRowID="i", StationCode="c", MethodCode="c", SampleDate="c",
                                              SampleTimeStart="c", SurveyNumber="i", WaterTemperature="d",
                                              Turbidity="d", Secchi="d", SecchiEstimated="l", ConductivityTop="d",
                                              ConductivityBottom="d", TowDirectionCode="i", MeterStart="d",
                                              MeterEnd="d", CableOut="d", TideCode="i", DepthBottom="d",
                                              WeatherCode="i", Microcystis="i", WaveCode="i",
                                              WindDirection="c", BottomTemperature="d"))%>%
  rename(Station=StationCode, Method=MethodCode, Tide=TideCode, Time=SampleTimeStart, Depth=DepthBottom, Date=SampleDate)%>%
  mutate(Date=parse_date_time(Date, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"),
         Time = parse_date_time(Time, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"),
         Tide=recode(Tide, `1` = "High Slack", `2` = "Ebb", `3` = "Low Slack", `4` = "Flood"), # Convert tide codes to values
         Weather=recode(WeatherCode, `1` = "Cloud (0-33%)", `2` = "Cloud (33-66%)", `3` = "Cloud (66-100%)", `4` = "Rain"), # Convert weather codes to values
         Waves=recode(WaveCode, `1` = "Calm", `2` = "Waves w/o whitecaps", `3` = "Waves w/ whitecaps"), # Convert wave codes to values
         WindDirection=toupper(WindDirection), # Make Wind direction codes consistent
         Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"),
         Meter_total=MeterEnd-MeterStart)%>% # Calculate flowmeter total difference
  mutate(Tow_volume = Meter_total*0.02687*10.7, # Calculate tow volume using formula provided by Steve Slater / James White
         WindDirection=recode(WindDirection, "NA"=NA_character_, "N/A"=NA_character_))%>% # Convert NA codes to NA
  filter(Method=="MWTR")%>% # Select only Midwater Trawl. All rows are MWTR but just in case the data change
  mutate(Method=recode(Method, MWTR="Midwater trawl"), # Recode method for consistency
         Microcystis=recode(Microcystis, `1`="Absent", `2`="Low", `6`="Low", `3`="Medium", `4`="High", `5`="Very high"), #Convert Microcystis codes to values
         Tow_direction = recode(TowDirectionCode, `1`="With current", `2`="Against current", `3`="Unknown"))%>% # Convert tow direction codes to values
  select(-TowDirectionCode, -WeatherCode, -WaveCode, -MeterEnd, -MeterStart, -Meter_total)%>% # Remove unneeded variables
  left_join(stations_fmwt, by="Station")%>% # Add station coordinates
  mutate(SampleID=1:nrow(.)) # Add unique identifier for each sample (net tow)


# Catch data --------------------------------------------------------------


catch_fmwt <- read_csv(file.path("data-raw", "FMWT", "Catch.csv"),
                       col_types = cols_only(CatchRowID="i", SampleRowID="i", OrganismCode="i", Catch="d"))%>%
  filter(!is.na(OrganismCode))%>% # Remove any records with an NA organism code
  left_join(Species%>% # Add species names
              select(OrganismCode=FMWT_Code, Taxa)%>%
              filter(!is.na(OrganismCode)),
            by="OrganismCode")%>%
  select(-OrganismCode) # Remove unneeded variable


# Length data -------------------------------------------------------------


length_fmwt<- read_csv(file.path("data-raw", "FMWT", "Length.csv"), na=c("NA", "n/p", ""),
                       col_types = cols_only(CatchRowID="i", ForkLength="d", LengthFrequency="d"))%>%
  filter(ForkLength!=0)%>% # 0 fork length means not measured, so removing those from length table so those fish can be redistributed among measured lengths
  group_by(CatchRowID, ForkLength)%>%
  summarise(LengthFrequency=sum(LengthFrequency), .groups="drop")

catchlength_fmwt <- catch_fmwt%>%
  left_join(length_fmwt%>%
              group_by(CatchRowID)%>%
              mutate(TotalMeasured=sum(LengthFrequency, na.rm=T))%>% # Calculate total number of fish measured for each species in each sample
              ungroup(),
            by="CatchRowID")%>% # Add catch numbers and species names
  mutate(Count = if_else(is.na(ForkLength), Catch, (LengthFrequency/TotalMeasured)*Catch)) # Calculate adjusted count


# Create final datasets ---------------------------------------------------


FMWT<-sample_fmwt%>% # Start with sample to ensure samples without any catch (empty nets) are included
  left_join(catchlength_fmwt, by="SampleRowID")%>% # Join to catch/length data
  mutate(Sal_surf=ec2pss(ConductivityTop/1000, t=25), # Convert conductivity to salinity
         Sal_bott=ec2pss(ConductivityBottom/1000, t=25),
         Secchi=Secchi*100, # Convert Secchi to cm from m
         Depth = Depth*0.3048, #Convert depth to m from feet
         Source="FMWT",
         SampleID=paste(Source, SampleID), # Add variable for unique (across all studies) sampleID
         Length_NA_flag=case_when(
           is.na(ForkLength) & is.na(Count)~ "No fish caught",
           is.na(ForkLength) ~ "Unknown length",
           TRUE ~ NA_character_), # Add reasoning for an NA lengths (all "No Fish Caught" for FMWT)
         Taxa=stringr::str_remove(Taxa, " \\((.*)"))%>% # Remove life stage info from Taxa names
  rename(Length=ForkLength, Temp_surf=WaterTemperature, Temp_bott=BottomTemperature,
         Secchi_estimated=SecchiEstimated, Survey=SurveyNumber, Cable_length=CableOut,
         Wind_direction=WindDirection)%>%
  select(-ConductivityTop, -ConductivityBottom, -LengthFrequency, -TotalMeasured,
         -SampleRowID, -Time, -Catch, -Turbidity, -Microcystis,
         -Wind_direction, -Temp_bott, -Weather, -Waves, -Sal_bott)%>% # Remove extra variables
  select(Source, Station, Latitude, Longitude, Date, Datetime, Survey, # Reorder variables for consistency
         Depth, SampleID, CatchRowID, Method, Tide, Sal_surf, Temp_surf, Secchi, Secchi_estimated,
         Tow_volume, Tow_direction, Cable_length, Taxa, Length, Count, Length_NA_flag)

# Just measured lengths

FMWT_measured_lengths<-length_fmwt%>%
  left_join(FMWT%>% # Join species names and sampleID
              select(CatchRowID, SampleID, Taxa)%>%
              distinct(),
            by="CatchRowID")%>%
  select(-CatchRowID)%>%
  group_by(across(-LengthFrequency))%>% # Add up any new multiples after removing lifestages
  summarise(Count=sum(LengthFrequency), .groups="drop")%>%
  select(SampleID, Taxa, Length=ForkLength, Count) # Reorder variables for consistency

FMWT<-FMWT%>%
  select(-CatchRowID)%>% # Remove unneeded variable
  group_by(across(-Count))%>% # Add up any new multiples after removing lifestages
  summarise(Count=sum(Count), .groups="drop")

rm(catchlength_fmwt, catch_fmwt, length_fmwt, sample_fmwt, stations_fmwt) # Clean up

usethis::use_data(FMWT, FMWT_measured_lengths, overwrite=TRUE, compress="xz") # Save compressed data to /data
