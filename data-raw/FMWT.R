## code to prepare `FMWT` dataset goes here
require(readr)
require(wql)
require(dplyr)
require(tidyr)
require(lubridate)
require(LTMRdata)


# Station locations -------------------------------------------------------


stations_fmwt <- read_csv(file.path("data-raw", "FMWT", "StationsLookUp.csv"),
                          col_types = cols_only(StationCode="c", Active="i",
                                                Lat="c", Long="c", Comments="c",
                                                `Channel/Shoal`="i", `WGS84 Lat`="d", `WGS84 Long`="d"))%>%
  select(Station=StationCode, Active, Lat, Long, Station_notes=Comments, Lat2=`WGS84 Lat`, Long2=`WGS84 Long`)%>%
  separate(Lat, into=c("Lat_d", "Lat_m", "Lat_s"), sep="[ ]{1,}", convert=T)%>% # parse latitude and longitude into something usable
  separate(Long, into=c("Long_d", "Long_m", "Long_s"), sep="[ ]{1,}", convert=T)%>%
  mutate(Latitude=Lat_d+Lat_m/60+Lat_s/3600, # Convert Latitude and Longitude to decimal degrees
         Longitude=Long_d-Long_m/60-Long_s/3600)%>%
  mutate(Latitude=if_else(is.na(Latitude), Lat2, Latitude), #Use WGS84 Lats and Longs if values from Lat or Long columns are missing
         Longitude=if_else(is.na(Longitude), -1*Long2, Longitude))%>% # WGS84 Longitude is positive
  select(Station, Latitude, Longitude, Active)%>%
  drop_na(Station, Latitude, Longitude) # Drop any rows with NAs in these variables


# Sample dates ------------------------------------------------------------


date_fmwt <- read_csv(file.path("data-raw", "FMWT", "Date.csv"), col_types=cols_only(DateID="i", SampleDate="c"))%>%
  mutate(SampleDate=parse_date_time(SampleDate, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
  rename(Date=SampleDate)


# Sample-level data -------------------------------------------------------


sample_fmwt <- read_csv(file.path("data-raw", "FMWT", "Sample.csv"),
                        col_types = cols_only(SampleRowID="i", StationCode="c", MethodCode="c",
                                              SampleTimeStart="c", SurveyNumber="i", WaterTemperature="d",
                                              Turbidity="d", Secchi="d", SecchiEstimated="l", ConductivityTop="d",
                                              ConductivityBottom="d", TowDirectionCode="i", MeterStart="d",
                                              MeterEnd="d", CableOut="d", TideCode="i", DepthBottom="d",
                                              WeatherCode="i", Microcystis="i", WaveCode="i",
                                              WindDirection="c", BottomTemperature="d", DateID="i"))%>%
  left_join(date_fmwt, by="DateID")%>% # Add dates
  rename(Station=StationCode, Method=MethodCode, Tide=TideCode, Time=SampleTimeStart, Depth=DepthBottom)%>%
  mutate(Time = parse_date_time(Time, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"),
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
  select(-TowDirectionCode, -WeatherCode, -WaveCode, -MeterEnd, -MeterStart, -Meter_total, -DateID)%>% # Remove unneeded variables
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


length_fmwt<- read_csv(file.path("data-raw", "FMWT", "Length.csv"), na=c("NA", "n/p"),
                       col_types = cols_only(CatchRowID="i", ForkLength="d", Dead="c", LengthFrequency="d"))%>%
  filter(ForkLength!=0) # 0 fork length means not measured, so removing those from length table so those fish can be redistributed among measured lengths

catchlength_fmwt <- length_fmwt%>%
  group_by(CatchRowID)%>%
  mutate(TotalMeasured=sum(LengthFrequency, na.rm=T))%>% # Calculate total number of fish measured for each species in each sample
  ungroup()%>%
  left_join(catch_fmwt, by="CatchRowID")%>% # Add catch numbers and species names
  mutate(Count = (LengthFrequency/TotalMeasured)*Catch) # Calculate adjusted count


# Create final datasets ---------------------------------------------------


FMWT<-sample_fmwt%>% # Start with sample to ensure samples without any catch (empty nets) are included
  left_join(catchlength_fmwt, by="SampleRowID")%>% # Join to catch/length data
  mutate(Sal_surf=ec2pss(ConductivityTop/1000, t=25), # Convert conductivity to salinity
         Sal_bott=ec2pss(ConductivityBottom/1000, t=25),
         Secchi=Secchi*100, # Convert Secchi to cm from m
         Depth = Depth*0.3048, #Convert depth to m from feet
         Source="FMWT",
         SampleID=paste(Source, SampleID), # Add variable for unique (across all studies) sampleID
         Length_NA_flag=if_else(is.na(ForkLength), "No fish caught", NA_character_))%>% # Add reasoning for an NA lengths (all "No Fish Caught" for FMWT)
  rename(Length=ForkLength, Temp_surf=WaterTemperature, Temp_bott=BottomTemperature,
         Secchi_estimated=SecchiEstimated, Survey=SurveyNumber, Cable_length=CableOut,
         Wind_direction=WindDirection)%>%
  select(-ConductivityTop, -ConductivityBottom, -LengthFrequency, -TotalMeasured,
         -SampleRowID, -Time, -Catch, -Active, -Dead)%>% # Remove extra variables
  select(-Turbidity, -Microcystis, -Wind_direction, -Temp_bott, -Weather, -Waves, -Sal_bott)%>% # Remove extra environmental variables
  select(Source, Station, Latitude, Longitude, Date, Datetime, # Reorder variables for consistency
         Depth, SampleID, CatchRowID, Method, Tide, Sal_surf, Temp_surf, Secchi, Secchi_estimated,
         Tow_volume, Tow_direction, Cable_length, Taxa, Length, Count, Length_NA_flag)

# Just measured lengths

FMWT_measured_lengths<-length_fmwt%>%
  left_join(FMWT%>% # Join species names and sampleID
              select(CatchRowID, SampleID, Taxa)%>%
              distinct(),
            by="CatchRowID")%>%
  select(SampleID, Taxa, Dead, Length=ForkLength, Count=LengthFrequency) # Reorder variables for consistency

FMWT<-FMWT%>%
  select(-CatchRowID) # Remove unneeded variable

rm(catchlength_fmwt, catch_fmwt, length_fmwt, sample_fmwt, date_fmwt, stations_fmwt) # Clean up

usethis::use_data(FMWT, FMWT_measured_lengths, overwrite=TRUE) # Save compressed data to /data
