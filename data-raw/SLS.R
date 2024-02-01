library(wql)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
require(LTMRdata)

# Setting up path for SLS files----
Path<-file.path(tempdir(), "SLS.zip")
Path_origin<-file.path(tempdir())
#Downloading MWT_data.zip----
download.file("https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SLS.zip", Path, mode="wb",method="libcurl")
unzip(Path,files="SLS.mdb",exdir=Path_origin)

# MS access database set up----
# File path to Access database (Salvage)
db_path <- file.path(tempdir(),"SLS.mdb")

source(file.path("data-raw", "bridgeAccess.R"))

keepTables <- c("Catch","FishCodes","Lengths",
                "Tow Info","Water Info","Meter Corrections", "SLS Stations")

SLSTables <- bridgeAccess(db_path,
                            tables = keepTables,
                            script = file.path("data-raw", "connectAccess.R"))

# # If you've chosen to read csv --------------------------------------------
# SLSTables <- list()
#
# SLSTables$Catch  <- read_csv(file.path("data-raw", "SLS", "Catch.csv"),
#                              col_types = cols_only(Date = "c", Station = "c", Tow = "i",
#                                                    FishCode = "i", Catch = "i", CatchID = "i")) %>%
#   mutate(Date=parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"))
#
# SLSTables$Lengths <- read_csv(file.path("data-raw", "SLS", "Lengths.csv"),
#                                 col_types = cols_only(Date = "c", Station = "c", Tow = "i",
#                                     FishCode = "i", Length = "i", entryorder = "i")) %>%
#   mutate(Date=parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"))
#
# SLSTables$`Meter Corrections` <- read_csv(file.path("data-raw", "SLS", "Meter Corrections.csv"),
#                                           col_types = cols_only(StudyYear = "d", MeterSerial = "i",
#                                                                 CalibrationDate = "c", kfactor = "d", Notes = "c")) %>%
#   mutate(CalibrationDate=as.Date(CalibrationDate))
#
# SLSTables$`Tow Info` <- read_csv(file.path("data-raw", "SLS", "Tow Info.csv"),
#                                    col_types =
#                                      cols_only(Date = "c", Station = "c", Tow = "i",
#                                        Time = "c", Tide = "c", BottomDepth = "i", CableOut = "i",
#                                        Duration = "d", NetMeterSerial = "i", NetMeterStart = "i",
#                                        NetMeterEnd = "i", NetMeterCheck = "i", CBMeterSerial = "i",
#                                        CBMeterStart = "i", CBMeterEnd = "i", CBMeterCheck = "i", Comments = "c")) %>%
#   # Just going to change this to UTC here b/c that's how the database from Access is read as
#   # This gets changed later to America/Los_Angeles in creating the dataframes for the final table
#   mutate(Date = as.Date(Date),
#          Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"))
#
# SLSTables$`Water Info` <- read_csv(file.path("data-raw", "SLS", "Water Info.csv"),
#                                    col_types =
#                                      cols_only(Survey = "i", Date = "c", Station = "c", TopTemp = "d",
#                                                TopEC = "i", BottomEC = "i", Secchi = "i", Turbidity = "i",
#                                                Comments = "c")) %>%
#   mutate(Date = parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"))
#
# SLSTables$`20mm Stations` <- read_csv(file.path("data-raw", "SLS", "20mm Stations.csv"),
#                                       col_types =
#                                         cols_only(Station = "c", LatD = "d", LatM = "d",
#                                                   LatS = "d", LonD = "d", LonM = "d", LonS = "d")) %>%
#   mutate(Latitude=(LatD + LatM/60 + LatS/3600),
#          Longitude= -(LonD + LonM/60 + LonS/3600))

#MWT data setup ----

SLSTables$Catch <- SLSTables$Catch %>%
  transmute(Date = parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
            Station = as.character(Station),
            across(c(Tow, FishCode, Catch, CatchID), as.integer))

SLSTables$Lengths <- SLSTables$Lengths %>%
  transmute(Date = parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
            Station = as.character(Station),
            across(c(Tow, FishCode, Length, entryorder), as.integer))

SLSTables$`Meter Corrections` <- SLSTables$`Meter Corrections` %>%
  transmute(StudyYear = as.double(StudyYear),
            MeterSerial = as.integer(MeterSerial),
            CalibrationDate = as.Date(CalibrationDate),
            kfactor = as.double(kfactor),
            Notes = as.character(Notes))

SLSTables$`Tow Info` <- SLSTables$`Tow Info`%>%
  transmute(Date = parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
            Station = as.character(Station),
            Tow = as.integer(Tow),
            Time = as.POSIXct(Time),
            Tide = as.character(Tide),
            BottomDepth = as.integer(BottomDepth),
            CableOut = CableOut * 0.3048,
            Duration = as.double(Duration),
            across(c(NetMeterSerial, NetMeterStart, NetMeterEnd, NetMeterCheck),
                   as.integer),
            Notes_tow = as.character(Comments))

SLSTables$`Water Info` <- SLSTables$`Water Info`%>%
  transmute(Survey = as.integer(Survey),
            Date = parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
            Station = as.character(Station),
            TopTemp = as.double(TopTemp),
            across(c(TopEC, BottomEC, Secchi, NTU, FNU), as.integer),
            Notes_env = as.character(Comments))

# SLS used to have a 20 mm station table that has changed to be "SLS Stations"
SLSTables$`SLS Stations` <- SLSTables$`SLS Stations` %>%
  transmute(Station = as.character(Station),
            Latitude = (LatD + LatM/60 + LatS/3600),
            Longitude = -(LonD + LonM/60 + LonS/3600))

# Manipulating the data tables --------------------------------------------

waterInfo <- SLSTables$`Water Info` %>%
  mutate(# Converting secchi from cm to m
    Secchi = Secchi/100,
    # Converting EC to salinity
    # Per SOP normalized at TopTemp = 25C; units are in millisiemens
    Sal_surf = wql::ec2pss(TopEC/1000, t = 25),
    Sal_bot = wql::ec2pss(BottomEC/1000, t = 25),
    # This is to take care of how the floats are read between Access and readr methods...
    Temp_surf = round(TopTemp, 2),
    # Changing Date to as.Date
    Date = parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"))

towInfo <- SLSTables$`Tow Info` %>%
  # 1 parsing error because time was not recorded for that row
  # Now, joining to the Meter Corrections table to calculate tow volume later
  # This is based on the "WEB_Raw_Catch_Volum_Info" query in the SLS Access query database
  # First need to create a study year column; from the query above, the "WEB_VolumeInfo" query calculates year from the
  # towInfo table.
  mutate(StudyYear = year(Date),
         # Changing Date from POSIXct format to simply Date for easy of use
         # Timezone data is still retained in the Datetime column
         Date = parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles")) %>%
  left_join(SLSTables$`Meter Corrections` %>%
              # There are duplicated values here in this table; will simply distinct() them
              # Confirmed via email with Adam, ES of Native Fish unit as of 10-27-2021
              distinct(),
            by = c("StudyYear", "NetMeterSerial" = "MeterSerial")) %>%
  # Moving on to various required calculations
  mutate(Datetime = ymd_hms(if_else(is.na(Time), NA_character_, paste(Date, paste(hour(Time), minute(Time), second(Time), sep=":"))),
                            tz = "America/Los_Angeles"),
         # Now to turn tide into factors as outlined in the metadata file
         Tide = case_when(Tide == 1 ~ "High Slack",
                          Tide == 2 ~ "Ebb",
                          Tide == 3 ~ "Low Slack",
                          Tide == 4 ~ "Flood",
                          TRUE ~ NA_character_),
         # Converting bottom depth to meters
         Depth = BottomDepth * 0.3048,
         # Calculating tow volume for the fish net, 0.37 as the area of the mout of the net (per Access + SLS SOP document)
         Tow_volume = NetMeterCheck * kfactor * 0.37)

catch <- SLSTables$Catch %>%
  mutate(Date = parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"))

lengths <- SLSTables$Lengths %>%
  mutate(Date = parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles")) %>%
  # Calculating total number of fish measured (across all lengths) and # of fish measured
  # per Date, Station, Tow, and FishCode
  # This is to calculate plus counts later in dfFin
  group_by(Date, Station, Tow, FishCode, Length)%>%
  summarise(LengthFrequency=n(), .groups="drop")%>%
  group_by(Date, Station, Tow, FishCode) %>%
  mutate(TotalLengthMeasured=sum(LengthFrequency)) %>%
  ungroup()

# Now to combine the datasets together, following the relationship table in Access
# The tables go waterInfo -> towInfo -> Catch -> lengths
SLS <- waterInfo %>%
  full_join(towInfo,
            by = c("Date", "Station")) %>%
  full_join(catch,
            by = c("Date", "Station", "Tow"),multiple="all") %>%
  full_join(lengths,
            by = c("Date", "Station", "Tow", "FishCode"),multiple="all") %>%
  # Adding in taxa name based on Species Code.csv file
  left_join(Species %>%
              select(TMM_Code,
                     Taxa) %>%
              dplyr::filter(!is.na(TMM_Code)),
            by = c("FishCode"="TMM_Code")) %>%
  left_join(SLSTables$`SLS Stations`,
            by="Station")%>%
  # Merging the two comment columns together; they both have data in them
  mutate(Notes_tow=paste(Notes_tow, Notes_env, sep = "; ")) %>%
  arrange(Date, Datetime, Survey, Station, Tow, Taxa, Length) %>%
  mutate(Source = "SLS",
         SampleID=paste(Source, Date, Station, Tow), # Creating SampleID index
         Count = if_else(is.na(Length), as.numeric(Catch), (LengthFrequency/TotalLengthMeasured) * Catch),
         # Creating Length_NA_flag to parallel the other survey datasets in LTMR
         Length_NA_flag = case_when(is.na(Count) ~ "No fish caught",
                                    is.na(Length) & Count > 0 ~ "Unknown length",
                                    TRUE ~ NA_character_),
         # Creating Method column; Adam described this as an "Oblique tow", significantly diff from MWT
         Method = "Oblique tow",
         Station=as.character(Station),
         Count=if_else(Length_NA_flag=="No fish caught", 0, Count, missing=Count))%>% # Transform all counts for "No fish caught" to 0.
  # Removing CatchID and entryorder as they are not relevant to the dataset
  # Removing TopEC, BottomEC as they have been converted over the salinity already
  # Removing CBMeterSerial, CBMeterStart, CBMeterEnd, CBMeterCheck as CB not ran on the SLS
  select(Source, Station, Latitude, Longitude,
         Date, Datetime, Survey, Depth, SampleID, Method,
         Tide, Sal_surf, Sal_bot, Temp_surf, Secchi, NTU, FNU, Tow_volume,
         Cable_length=CableOut, Tow_duration=Duration,
         Taxa, Length, Count, Length_NA_flag,
         Notes_tow, Notes_flowmeter = Notes)

# # Just to make sure that no duplications occurred; lengths should be the same
# all.equal(lengths$Length %>% sum(na.rm = T),
#           dfFin$Length %>% sum(na.rm = T))

# Some differences in Notes_tow if you use bridgeAccess vs reading from the csv files; seems to be
# a unicode issue with the apostrophe symbol. Comments read the same, i.e., disregard error for this difference.

usethis::use_data(SLS, overwrite=TRUE, compress="xz")
