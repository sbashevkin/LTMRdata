## code to prepare `FMWT` dataset goes here
# require(readr)
require(wql)
require(dplyr)
require(tidyr)
require(lubridate)
require(LTMRdata)
require(stringr)
require(readr)

Path<-file.path(tempdir(), "MWT_data.zip")
Path_origin<-file.path(tempdir())
#Downloading MWT_data.zip----
download.file("https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/FMWT%20Data/MWT_data.zip", Path, mode="wb",method="libcurl")
unzip(Path,files="MWT_data.accdb",exdir=Path_origin)

# MS access database set up----
# File path to Access database (Salvage)
db_path <- file.path(tempdir(),"MWT_data.accdb")

source(file.path("data-raw", "bridgeAccess.R"))

keepTables <- c("StationsLookUp", "Sample", "Catch", "Length")

FMWT_Tables <- bridgeAccess(db_path,
                            tables = keepTables,
                            script = file.path("data-raw", "connectAccess.R"))

# # If you've chosen to read csv --------------------------------------------
# FMWT_Tables <- list()
#
# FMWT_Tables$StationsLookUp <- read_csv(file.path("data-raw", "FMWT", "StationsLookUp.csv"),
#                                        col_types = cols_only(StationCode="c", DD_Latitude="d", DD_Longitude="d"))
#
# FMWT_Tables$Sample <- read_csv(file.path("data-raw", "FMWT", "Sample.csv"),
#                                col_types = cols_only(SampleRowID="i", StationCode="c", MethodCode="c", SampleDate="c",
#                                                      SampleTimeStart="c", SurveyNumber="i", WaterTemperature="d",
#                                                      Turbidity="d", Secchi="d", SecchiEstimated="l", ConductivityTop="d",
#                                                      ConductivityBottom="d", TowDirectionCode="i", MeterStart="d",
#                                                      MeterEnd="d", CableOut="d", TideCode="i", DepthBottom="d",
#                                                      WeatherCode="i", Microcystis="i", WaveCode="i",
#                                                      WindDirection="c", BottomTemperature="d")) %>%
#   # Check format here for time as it can change based on how you export your relational tables
#   mutate(SampleDate = as.Date(SampleDate),
#          SampleTimeStart = as.POSIXct(SampleTimeStart, format = "%Y-%m-%d %H:%M:%S"))
#
# FMWT_Tables$Catch <- read_csv(file.path("data-raw", "FMWT", "Catch.csv"),
#                               col_types = cols_only(CatchRowID="i", SampleRowID="i", OrganismCode="i", Catch="d"))
#
# FMWT_Tables$Length <- read_csv(file.path("data-raw", "FMWT", "Length.csv"), na=c("NA", "n/p", ""),
#                                col_types = cols_only(CatchRowID="i", ForkLength="d", LengthFrequency="d"))

# Station locations -------------------------------------------------------
FMWT_Tables$Station <- FMWT_Tables$StationsLookUp%>%
  # select("StationCode","DD_Latitude","DD_Longitude")%>%
  # rename(Station=StationCode, Latitude=DD_Latitude, Longitude=DD_Longitude)%>%
  transmute(Station = as.character(StationCode),
            Latitude = as.double(DD_Latitude), Longitude = as.double(DD_Longitude)) %>%
  drop_na()

# Sample-level data -------------------------------------------------------
FMWT_Tables$Sample <- FMWT_Tables$Sample %>%
    transmute(SampleRowID = as.integer(SampleRowID),
              across(c(StationCode, MethodCode, SampleTimeStart), as.character),
              SampleDate = as.Date(SampleDate),
              SurveyNumber = as.integer(SurveyNumber),
              across(c(WaterTemperature, Turbidity, Secchi), as.double),
              SecchiEstimated = as.logical(SecchiEstimated),
              across(c(ConductivityTop, ConductivityBottom), as.double),
              TowDirectionCode = as.integer(TowDirectionCode),
              across(c(MeterStart, MeterEnd, CableOut), as.double),
              TideCode = as.integer(TideCode),
              DepthBottom = as.double(DepthBottom),
              across(c(WeatherCode, Microcystis, WaveCode), as.double),
              WindDirection = as.character(WindDirection),
              BottomTemperature = as.double(BottomTemperature)) %>%
  rename(Station=StationCode, Method=MethodCode, Tide=TideCode, Time=SampleTimeStart, Depth=DepthBottom, Date=SampleDate)%>%
  mutate(Station=ifelse(Station<100,as.character(paste("0",as.character(Station),sep="")),as.character(Station)), # Setting Station
         Tide=recode(Tide, `1` = "High Slack", `2` = "Ebb", `3` = "Low Slack", `4` = "Flood"), # Convert tide codes to values
         Weather=recode(WeatherCode, `1` = "Cloud (0-33%)", `2` = "Cloud (33-66%)", `3` = "Cloud (66-100%)", `4` = "Rain"), # Convert weather codes to values
         Waves=recode(WaveCode, `1` = "Calm", `2` = "Waves w/o whitecaps", `3` = "Waves w/ whitecaps"), # Convert wave codes to values
         WindDirection=toupper(WindDirection), # Make Wind direction codes consistent
         Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"),
         Meter_total=MeterEnd-MeterStart)%>% # Calculate flowmeter total difference
  mutate(Tow_volume = Meter_total*0.02687*10.7, # Calculate tow volume using formula provided by Steve Slater / James White
         WindDirection=recode(WindDirection, "NA"=NA_character_, "N/A"=NA_character_))%>% # Convert NA codes to NA
  dplyr::filter(Method=="MWTR")%>% # Select only Midwater Trawl. All rows are MWTR but just in case the data change
  mutate(Method=recode(Method, MWTR="Midwater trawl"), # Recode method for consistency
         Microcystis=recode(Microcystis, `1`="Absent", `2`="Low", `6`="Low", `3`="Medium", `4`="High", `5`="Very high"), #Convert Microcystis codes to values
         Tow_direction = recode(TowDirectionCode, `1`="With current", `2`="Against current", `3`="Unknown"))%>% # Convert tow direction codes to values
  select(-TowDirectionCode, -WeatherCode, -WaveCode, -MeterEnd, -MeterStart, -Meter_total)%>% # Remove unneeded variables
  left_join(FMWT_Tables$Station, by="Station")%>% # Add station coordinates
  mutate(SampleID=1:nrow(.)) # Add unique identifier for each sample (net tow)

# Catch data --------------------------------------------------------------
FMWT_Tables$Catch<- FMWT_Tables$Catch%>%
  transmute(across(c(CatchRowID, SampleRowID, OrganismCode), as.integer),
            Catch = as.double(Catch)) %>%
  dplyr::filter(!is.na(OrganismCode))%>% # Remove any records with an NA organism code
  left_join(Species%>% # Add species names
              select(OrganismCode=FMWT_Code, Taxa)%>%
              dplyr::filter(!is.na(OrganismCode)),
            by="OrganismCode")%>%
  select(-OrganismCode) # Remove unneeded variable

# Length data -------------------------------------------------------------
FMWT_Tables$Length<- FMWT_Tables$Length%>%
  mutate(across(everything(), ~replace(.x, .x %in% c("n/p", "NA", ""), NA))) %>%
  transmute(CatchRowID = as.integer(CatchRowID),
            across(c(ForkLength, LengthFrequency), as.double)) %>%
  select("CatchRowID","ForkLength","LengthFrequency")%>%
  dplyr::filter(ForkLength!=0)%>% # 0 fork length means not measured, so removing those from length table so those fish can be redistributed among measured lengths
  group_by(CatchRowID, ForkLength)%>%
  summarise(LengthFrequency=sum(LengthFrequency), .groups="drop")%>%
  as.data.frame()

FMWT_Tables$CatchLength<- FMWT_Tables$Catch%>%
  left_join(FMWT_Tables$Length%>%
              group_by(CatchRowID)%>%
              mutate(TotalMeasured=sum(LengthFrequency, na.rm=T))%>% # Calculate total number of fish measured for each species in each sample
              ungroup(),
            by="CatchRowID")%>% # Add catch numbers and species names
  mutate(Count = ifelse(is.na(ForkLength), Catch, (LengthFrequency/TotalMeasured)*Catch))%>% # Calculate adjusted count
  dplyr::filter(!is.na(Count))

# Create final datasets ---------------------------------------------------


FMWT<-FMWT_Tables$Sample%>% # Start with sample to ensure samples without any catch (empty nets) are included
  left_join(FMWT_Tables$CatchLength, by="SampleRowID")%>% # Join to catch/length data
  mutate(Sal_surf=ec2pss(ConductivityTop/1000, t=25), # Convert conductivity to salinity
         Sal_bott=ec2pss(ConductivityBottom/1000, t=25),
         Secchi=Secchi*100, # Convert Secchi to cm from m
         Depth = Depth*0.3048, #Convert depth to m from feet
         CableOut = CableOut*0.3048, # Convert to m from feet
         Source="FMWT",
         SampleID=paste(Source, SampleID), # Add variable for unique (across all studies) sampleID
         Length_NA_flag=case_when(
           is.na(ForkLength) & is.na(Count)~ "No fish caught",
           is.na(ForkLength) ~ "Unknown length",
           TRUE ~ NA_character_), # Add reasoning for an NA lengths (all "No Fish Caught" for FMWT)
         Taxa=stringr::str_remove(Taxa, " \\((.*)"), # Remove life stage info from Taxa names
         Count=if_else(Length_NA_flag=="No fish caught", 0, Count, missing=Count))%>% # Transform all counts for 'No fish caught' to 0.
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

FMWT_measured_lengths<-FMWT_Tables$Length%>%
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

usethis::use_data(FMWT, FMWT_measured_lengths, overwrite=TRUE, compress="xz") # Save compressed data to /data
