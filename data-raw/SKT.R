
###################################################################
##               code to prepare `SKT` dataset                   ##
###################################################################

require(wql)
require(LTMRdata)
require(readr)
require(dplyr)
require(lubridate)
require(tidyr)
require(stringr)

Path<-file.path(tempdir(), "SKT.zip")
Path_origin<-file.path(tempdir())
#Downloading MWT_data.zip----
download.file("https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SKT.zip", Path, mode="wb",method="libcurl")
unzip(Path,files="SKT.accdb",exdir=Path_origin)

# MS access database set up----
# File path to Access database (Salvage)
db_path <- file.path(tempdir(),"Skt.accdb")

source(file.path("data-raw", "bridgeAccess.R"))

keepTables <- c("StationsSKT", "tblSample", "tblCatch", "tblFishInfo")

SKT_Data <- bridgeAccess(db_path,
                         tables = keepTables,
                         script = file.path("data-raw", "connectAccess.R"))


# # If you've chosen to read csv --------------------------------------------
# SKT_Data <- list()
#
# SKT_Data$lktblStationsSKT <- read_csv(file.path("data-raw", "SKT", "lktblStationsSKT.csv"),
#                                  col_types=cols_only(Station="c", LatDeg="d", LatMin="d", LatSec="d",
#                                                      LongDec="d", LongMin="d", LongSec="d"))
#
# SKT_Data$tblSample <- read_csv(file.path("data-raw", "SKT", "tblSample.csv"),
#                             col_types = cols_only(SampleRowID = "i", SampleDate = "c", Station = "c",
#                                                   SampleTimeStart = "c", SurveyNumber = "i",
#                                                   WaterTemperature = "d", TideCode = "i", DepthBottom = "d",
#                                                   Secchi = "d", ConductivityTop = "d",
#                                                   TowDirectionCode = "i", MeterStart = "d", MeterEnd = "d")) %>%
#   mutate(SampleDate = as.Date(SampleDate),
#          SampleTimeStart = as.POSIXct(SampleTimeStart, format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles"))
#
# SKT_Data$tblCatch <- read_csv(file.path("data-raw", "SKT", "tblCatch.csv"),
#                            col_types = cols_only(CatchRowID = "i", SampleRowID = "i", OrganismCode = "c", Catch = "d"))
#
# SKT_Data$tblFishInfo <- read_csv(file.path("data-raw", "SKT", "tblFishInfo.csv"), na = c("NA", "n/p", ""),
#                               col_types = cols_only(CatchRowID = "i", ForkLength = "d", LengthRowID = "i"))

#MWT data setup ----

# Station locations -------------------------------------------------------
# read table with station latitude and longitude (one row per station)
StationsSKT <- SKT_Data$StationsSKT%>%
  select(Station,LatDeg,LatMin,LatSec,LongDec,LongMin,LongSec)%>%
  mutate(Latitude=LatDeg+LatMin/60+LatSec/3600,
         Longitude=(LongDec+LongMin/60+LongSec/3600)*-1)%>%
  select(Station, Latitude, Longitude)%>%
  drop_na()

# Sample-level data -------------------------------------------------------
# read sample data (one row per tow)
SampleSKT <- SKT_Data$tblSample%>%
  select(SampleRowID, Date = SampleDate, Station, Time = SampleTimeStart, Survey = SurveyNumber,
         Temp_surf = WaterTemperature, TideCode, Depth = DepthBottom, Secchi, ConductivityTop,
         TowDirectionCode, MeterStart, MeterEnd,
         TurbidityNTU = NTU, TurbidityFNU = FNU)%>%
    mutate(Date = parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
           Time = force_tz(as.POSIXct(Time, format = "%m/%d/%Y %H:%M", tz="UTC"), tz = "America/Los_Angeles"),
           # Create a new field which is a Date-Time composite.
           # SKT staff confirmed there is one sample collected at 00:50, but setting any other midnightish samples to NA
           Datetime = parse_date_time(if_else(is.na(Time) | (hour(Time)==0 & minute(Time)==0), NA_character_, paste(Date, paste(hour(Time), minute(Time), sep=":"))),
                                      "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
           # Convert tide codes to values
           Tide = recode(TideCode, `1` = "High Slack", `2` = "Ebb", `3` = "Low Slack", `4` = "Flood", .default = NA_character_),
           # Calculate flowmeter total difference
           Meter_total = MeterEnd - MeterStart,
           Meter_total = ifelse(Meter_total<0, Meter_total + 1000000, Meter_total), # Correct negative metertotals from meter resetting during trawl
           Depth = Depth*0.3048)%>% # Convert feet to meters
    # Calculate tow volume using formula provided by Trishelle Temple
    # Volume = A*K*D (A = 13.95 area of trawl mouth; K = 0.026873027 K factor of flow meter; D = difference in flow readings)
    mutate(Tow_volume = Meter_total*0.026873027*13.95,
           # Convert tow direction codes to values
           Tow_direction = recode(TowDirectionCode, `1` = "With current", `2` = "Against current",
                                  `3` = "Unknown", .default = NA_character_)) %>%
    # Remove unneeded variables
    select(-Meter_total, -TideCode, -TowDirectionCode, -MeterStart, -MeterEnd, -Time) %>%
    # Add station coordinates
    left_join(StationsSKT, by = "Station", relationship="many-to-one")

# Catch data --------------------------------------------------------------
# Read Catch data (one row per species per tow)
# Fields: CatchRowID	SampleRowID	OrganismCode	Catch
SKTCatch <- SKT_Data$tblCatch%>%
  select(CatchRowID,SampleRowID,OrganismCode,Catch)%>%
  mutate(OrganismCode=as.character(OrganismCode))%>%
  # Add species names
  left_join(Species %>%
              select(SKT_Code, Taxa) %>%
              dplyr::filter(!is.na(SKT_Code)),
            by = c("OrganismCode"="SKT_Code"),
            relationship = "many-to-one")

# Length data -------------------------------------------------------------
# Read Length data (one row per measured fish per tow)
# Fields: CatchRowID, LengthRowID, ForkLength, ReleasedAlive (flag)
SKTFishInfo <- SKT_Data$tblFishInfo%>%
    mutate(LengthFrequency = 1) %>%
    # 0 fork length means not measured, so removing those from
    # length table so those fish can be redistributed among measured lengths
    dplyr::filter(ForkLength != 0)%>%
    group_by(CatchRowID, ForkLength)%>%
    summarise(LengthFrequency=sum(LengthFrequency), .groups="drop")


SKTCatchLength<-SKTCatch%>%
  left_join(SKTFishInfo%>%
              group_by(CatchRowID)%>%
              # Calculate total number of fish measured for each species in each sample
              mutate(TotalMeasured = sum(LengthFrequency))%>%
              ungroup(),
            # Add catch numbers and species names
            by = "CatchRowID",
            multiple="all",
            relationship="one-to-many")%>%
  # Calculate adjusted count
  mutate(Count = ifelse(is.na(TotalMeasured), Catch, (LengthFrequency/TotalMeasured)*Catch))

# Create final datasets ---------------------------------------------------

# Start with sample to ensure samples without any catch (empty nets) are included
SKT <- SampleSKT %>%
  # Join to catch/length data
  left_join(SKTCatchLength%>%
              dplyr::filter(!(is.na(Count) & OrganismCode!=0)), # Remove any cases other than nocatch where Count is NA
            by="SampleRowID",
            multiple="all",
            relationship="one-to-many") %>%
  # Convert conductivity to salinity
  mutate(Sal_surf = ec2pss(ConductivityTop/1000, t=25),
         # add identifier for survey
         Source = "SKT", Method = "Kodiak trawl",
         # Add variable for unique (across all studies) sampleID
         SampleID = paste(Source, SampleRowID),
         # Add reasoning for an NA lengths (all "No Fish Caught" for FMWT)
         Length_NA_flag = case_when(OrganismCode == 0 ~ "No fish caught",
                                    is.na(ForkLength) & Count > 0 ~ "Unknown length",
                                    TRUE ~ NA_character_),
         Count=if_else(Length_NA_flag=="No fish caught", 0, Count, missing=Count), # Setting Count to 0 for no fish caught, just like the other surveys
         # Remove life stage info from Taxa names
         Taxa = stringr::str_remove(Taxa, " \\((.*)")) %>%
  # Reorder variables for consistency
  dplyr::select(Source, Station, Latitude, Longitude, Date, Datetime, Survey,
         Depth, SampleID, CatchRowID, Method, Tide, Sal_surf, Temp_surf,
         TurbidityNTU, TurbidityFNU, Secchi, Tow_volume, Tow_direction, Taxa,
         Length = ForkLength, Count, Length_NA_flag, CatchRowID)


# Just measured lengths

SKT_measured_lengths<-SKTFishInfo %>%
  # Join species names and sampleID
  left_join(SKT %>%
              select(CatchRowID, SampleID, Taxa) %>%
              distinct(),
            by = "CatchRowID",
            relationship="many-to-one") %>%
  # Reorder variables for consistency
  select(SampleID, Taxa, Length = ForkLength, Count = LengthFrequency)

# Remove unneeded variable
SKT<-SKT %>%
  select(-CatchRowID)%>%
  distinct()

source(file.path("data-raw", "comparison.R"))
compareSKT <- create_comparison_report(SKT, LTMRdata::SKT,
                                        id_cols = c("SampleID", "Taxa", "Length", "Count"))

compareSKTLengths <- create_comparison_report(SKT_measured_lengths, LTMRdata::SKT_measured_lengths,
                                               id_cols = c("SampleID", "Taxa", "Length", "Count"))

devtools::load_all()
tests <- test_dataset(SKT, return_failures = T)

# Save compressed data to /data
usethis::use_data(SKT, SKT_measured_lengths, overwrite=TRUE, compress="xz")
