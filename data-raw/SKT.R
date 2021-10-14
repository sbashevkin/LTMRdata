
###################################################################
##  code to prepare `SKT` dataset as prepared by Sam Bashevkin   ##
###################################################################

require(readr)
require(wql)
require(dplyr)
require(tidyr)
require(lubridate)
require(LTMRdata)
require(stringr)

# shows the relationshiph between tables and which field is the connecting field
# tblSample <= (SampleRowID) => tblCatch <= (CatchRowID) => tblFishInfo
# tblCatch <= (OrganismCode) => tblOrganismCodes


# Station locations -------------------------------------------------------
# read table with station latitude and longitude (one row per station)

stations_skt <- read_csv(file.path("data-raw", "SKT", "lktblStationsSKT.csv"),
                         col_types = cols_only(Station = "c", Latitude = "d", Longitude = "d")) %>%
  drop_na()

# Sample-level data -------------------------------------------------------
# read sample data (one row per tow)

sample_skt <- read_csv(file.path("data-raw", "SKT", "tblSample.csv"),
                       col_types = cols_only(SampleRowID = "i", SampleDate = "c", StationCode = "c",
                                             SampleTimeStart = "c", SurveyNumber = "i",
                                             WaterTemperature = "d", TideCode = "i", DepthBottom = "d",
                                             Secchi = "d", ConductivityTop = "d",
                                             TowDirectionCode = "i", MeterStart = "d", MeterEnd = "d")) %>%
  rename(Station = StationCode, Depth = DepthBottom, Temp_surf = WaterTemperature, Survey = SurveyNumber, Date=SampleDate) %>%
  mutate(Date = parse_date_time(Date, "%m/%d/%Y", tz="America/Los_Angeles"),          # read formatted date
         # Create a new field which is a Date-Time composite
         Datetime = parse_date_time(if_else(is.na(SampleTimeStart), NA_character_, paste(Date, SampleTimeStart)),
                                    "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
         # Convert tide codes to values
         Tide = recode(TideCode, `1` = "High Slack", `2` = "Ebb", `3` = "Low Slack", `4` = "Flood", .default = NA_character_),
         # Calculate flowmeter total difference
         Meter_total = MeterEnd - MeterStart) %>%
  # Calculate tow volume using formula provided by Trishelle Temple
  # Volume = A*K*D (A = 13.95 area of trawl mouth; K = 0.026873027 K factor of flow meter; D = difference in flow readings)
  mutate(Tow_volume = Meter_total*0.026873027*13.95,
         # Convert tow direction codes to values
         Tow_direction = recode(TowDirectionCode, `1` = "With current", `2` = "Against current",
                                `3` = "Unknown", .default = NA_character_)) %>%
  # Remove unneeded variables
  select(-Meter_total, -TideCode, -TowDirectionCode, -MeterStart, -MeterEnd, -SampleTimeStart) %>%
  # Add station coordinates
  left_join(stations_skt, by = "Station")


# Catch data --------------------------------------------------------------
# Read Catch data (one row per species per tow)
# Fields: CatchRowID	SampleRowID	OrganismCode	Catch

catch_skt <- read_csv(file.path("data-raw", "SKT", "tblCatch.csv"),
                      col_types = cols_only(CatchRowID = "i", SampleRowID = "i", OrganismCode = "c", Catch = "d")) %>%
  # Add species names
  left_join(Species %>%
              select(SKT_Code, Taxa) %>%
              filter(!is.na(SKT_Code)),
            by = c("OrganismCode"="SKT_Code"))

# Length data -------------------------------------------------------------
# Read Length data (one row per measured fish per tow)
# Fields: CatchRowID, LengthRowID, ForkLength, ReleasedAlive (flag)

length_skt <- read_csv(file.path("data-raw", "SKT", "tblFishInfo.csv"), na = c("NA", "n/p", ""),
                       col_types = cols_only(CatchRowID = "i", ForkLength = "d", LengthRowID = "i")) %>%
  mutate(LengthFrequency = 1) %>%
  # 0 fork length means not measured, so removing those from
  # length table so those fish can be redistributed among measured lengths
  filter(ForkLength != 0)%>%
  group_by(CatchRowID, ForkLength)%>%
  summarise(LengthFrequency=sum(LengthFrequency), .groups="drop")

catchlength_skt <- catch_skt%>%
  left_join(length_skt %>%
              group_by(CatchRowID) %>%
              # Calculate total number of fish measured for each species in each sample
              mutate(TotalMeasured = sum(LengthFrequency)) %>%
              ungroup(),
            # Add catch numbers and species names
            by = "CatchRowID") %>%
  # Calculate adjusted count
  mutate(Count = if_else(is.na(TotalMeasured), Catch, (LengthFrequency/TotalMeasured)*Catch))


# Create final datasets ---------------------------------------------------

# Start with sample to ensure samples without any catch (empty nets) are included
SKT <- sample_skt %>%
  # Join to catch/length data
  left_join(catchlength_skt, by="SampleRowID") %>%
  # there should not be any row with Catch = NA hence filter those
  filter(!is.na(Catch)) %>%
  # Convert conductivity to salinity
  mutate(Sal_surf = ec2pss(ConductivityTop/1000, t=25),
         # add identifier for survey
         Source = "SKT", Method = "Kodiak trawl",
         # Add variable for unique (across all studies) sampleID
         SampleID = paste(Source, SampleRowID),
         # Add reasoning for an NA lengths (all "No Fish Caught" for FMWT)
         Length_NA_flag = if_else(Catch == 0, "No fish caught", NA_character_),
         # Remove life stage info from Taxa names
         Taxa = stringr::str_remove(Taxa, " \\((.*)")) %>%
  rename(Length = ForkLength) %>%
  # Reorder variables for consistency
  select(Source, Station, Latitude, Longitude, Date, Datetime, Survey,
         Depth, SampleID, CatchRowID, Method, Tide, Sal_surf, Temp_surf, Secchi,
         Tow_volume, Tow_direction, Taxa, Length, Count, Length_NA_flag)

# Just measured lengths

SKT_measured_lengths<-length_skt %>%
  # Join species names and sampleID
  left_join(SKT %>%
              select(CatchRowID, SampleID, Taxa) %>%
              distinct(),
            by = "CatchRowID") %>%
  # Reorder variables for consistency
  select(SampleID, Taxa, Length = ForkLength, Count = LengthFrequency)

# Remove unneeded variable
SKT<-SKT %>%
  select(-CatchRowID)

# Clean up; remove temporary files
rm(catchlength_skt, catch_skt, length_skt, sample_skt, stations_skt)

# Save compressed data to /data
usethis::use_data(SKT, SKT_measured_lengths, overwrite=TRUE)



