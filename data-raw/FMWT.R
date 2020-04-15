## code to prepare `FMWT` dataset goes here
require(readr)
require(wql)
require(dplyr)
require(tidyr)
require(lubridate)
require(LTMRdata)

stations_fmwt <- read_csv(file.path("data-raw", "FMWT", "StationsLookUp.csv"),
  col_types = cols_only(StationCode = "c", Active = "i",
    Lat = "c", Long = "c", Comments = "c",
    `Channel/Shoal` = "i", `WGS84 Lat` = "d", `WGS84 Long` = "d")) %>%
  select(Station = StationCode, Active, Lat, Long, Station_notes = Comments, Lat2 = `WGS84 Lat`, Long2 = `WGS84 Long`) %>%
  separate(Lat, into = c("Lat_d", "Lat_m", "Lat_s"), sep = "[ ]{1,}", convert = T) %>% # parse latitude and longitude into something usable
  separate(Long, into = c("Long_d", "Long_m", "Long_s"), sep = "[ ]{1,}", convert = T) %>%
  mutate(Latitude = Lat_d + Lat_m / 60 + Lat_s / 3600,
    Longitude = Long_d - Long_m / 60 - Long_s / 3600) %>%
  mutate(Latitude = if_else(is.na(Latitude), Lat2, Latitude),
    Longitude = if_else(is.na(Longitude), Long2, Longitude)) %>%
  select(Station, Latitude, Longitude, Active) %>%
  drop_na(Station, Latitude, Longitude) # Drop any rows with NAs in these variables

date_fmwt <- read_csv(file.path("data-raw", "FMWT", "Date.csv"), col_types = cols_only(DateID = "i", SampleDate = "c")) %>%
  mutate(SampleDate = parse_date_time(SampleDate, "%m/%d/%Y %H:%M:%S", tz = "America/Los_Angeles")) %>%
  rename(Date = SampleDate)

sample_fmwt <- read_csv(file.path("data-raw", "FMWT", "Sample.csv"),
  col_types = cols_only(SampleRowID = "i", StationCode = "c", MethodCode = "c",
    SampleTimeStart = "c", SurveyNumber = "i", WaterTemperature = "d",
    Turbidity = "d", Secchi = "d", SecchiEstimated = "l", ConductivityTop = "d",
    ConductivityBottom = "d", TowDirectionCode = "i", MeterStart = "d",
    MeterEnd = "d", CableOut = "d", TideCode = "i", DepthBottom = "d",
    WeatherCode = "i", Microcystis = "i", WaveCode = "i",
    WindDirection = "c", BottomTemperature = "d", DateID = "i")) %>%
  left_join(date_fmwt, by = "DateID") %>% # Add dates
  rename(Station = StationCode, Method = MethodCode, Tide = TideCode, Time = SampleTimeStart, Depth = DepthBottom) %>%
  mutate(Time = parse_date_time(Time, "%m/%d/%Y %H:%M:%S", tz = "America/Los_Angeles"),
    Tide = recode(Tide, `1` = "High Slack", `2` = "Ebb", `3` = "Low Slack", `4` = "Flood"),
    Weather = recode(WeatherCode, `1` = "Cloud (0-33%)", `2` = "Cloud (33-66%)", `3` = "Cloud (66-100%)", `4` = "Rain"),
    Waves = recode(WaveCode, `1` = "Calm", `2` = "Waves w/o whitecaps", `3` = "Waves w/ whitecaps"),
    WindDirection = toupper(WindDirection),
    Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %%H:%M", tz = "America/Los_Angeles"),
    Meter_total = MeterEnd - MeterStart) %>%
  mutate(Tow_volume = Meter_total * 0.02687 * 10.7,
    WindDirection = recode(WindDirection, "NA" = NA_character_, "N/A" = NA_character_)) %>%
  filter(Method == "MWTR") %>% # All rows are MWTR but just in case the data change
  mutate(Method = recode(Method, MWTR = "Midwater trawl"),
    Microcystis = recode(Microcystis, `1` = "Absent", `2` = "Low", `6` = "Low", `3` = "Medium", `4` = "High", `5` = "Very high"),
    Tow_direction = recode(TowDirectionCode, `1` = "With current", `2` = "Against current", `3` = "Unknown")) %>%
  select(-TowDirectionCode, -WeatherCode, -WaveCode, -MeterEnd, -MeterStart, -Meter_total, -DateID) %>%
  left_join(stations_fmwt, by = "Station") %>%
  mutate(SampleID = 1:nrow(.))

catch_fmwt <- read_csv(file.path("data-raw", "FMWT", "Catch.csv"),
  col_types = cols_only(CatchRowID = "i", SampleRowID = "i", OrganismCode = "i", Catch = "d")) %>%
  filter(!is.na(OrganismCode)) %>%
  left_join(Species %>%
    select(OrganismCode = FMWT_Code, Taxa) %>%
    filter(!is.na(OrganismCode)),
  by = "OrganismCode") %>%
  select(-OrganismCode)

length_fmwt <- read_csv(file.path("data-raw", "FMWT", "Length.csv"), na = c("NA", "n/p"),
  col_types = cols_only(CatchRowID = "i", ForkLength = "d", Dead = "c", LengthFrequency = "d")) %>%
  filter(ForkLength != 0) # 0 fork length means not measured, so removing those from length table so those fish can be redistributed among measured lengths

catchlength_fmwt <- length_fmwt %>%
  group_by(CatchRowID) %>%
  mutate(TotalMeasured = sum(LengthFrequency, na.rm = T)) %>%
  ungroup() %>%
  left_join(catch_fmwt, by = "CatchRowID") %>%
  mutate(Count = (LengthFrequency / TotalMeasured) * Catch)

FMWT <- sample_fmwt %>%
  left_join(catchlength_fmwt, by = "SampleRowID") %>%
  mutate(Sal_surf = ec2pss(ConductivityTop / 1000, t = 25),
    Sal_bott = ec2pss(ConductivityBottom / 1000, t = 25),
    Secchi = Secchi * 100, # Convert Secchi to cm from m
    Depth = Depth * 0.3048, # Convert depth to m from feet
    Source = "FMWT",
    SampleID = paste(Source, SampleID)) %>%
  rename(Length = ForkLength, Temp_surf = WaterTemperature, Temp_bott = BottomTemperature,
    Secchi_estimated = SecchiEstimated, Survey = SurveyNumber, Cable_length = CableOut,
    Wind_direction = WindDirection) %>%
  select(-ConductivityTop, -ConductivityBottom, -LengthFrequency, -TotalMeasured,
    -SampleRowID, -Time, -Catch, -Active, -Dead) %>%
  select(-Turbidity, -Microcystis, -Wind_direction, -Temp_bott, -Weather, -Waves, -Sal_bott) # Remove extra environmental variables

FMWT_measured_lengths <- length_fmwt %>%
  left_join(FMWT %>%
    select(CatchRowID, SampleID, Taxa) %>%
    distinct(),
  by = "CatchRowID") %>%
  select(SampleID, Taxa, Dead, Length = ForkLength, Count = LengthFrequency)

FMWT <- FMWT %>%
  select(-CatchRowID)

rm(catchlength_fmwt, catch_fmwt, length_fmwt, sample_fmwt, date_fmwt, stations_fmwt)

usethis::use_data(FMWT, FMWT_measured_lengths, overwrite = TRUE)
