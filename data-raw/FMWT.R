## code to prepare `FMWT` dataset goes here

require(readr)
require(dplyr)
require(tidyr)
require(lubridate)

stations_fmwt <- read_csv(file.path("data-raw", "FMWT", "StationsLookUp.csv"),
                          col_types = cols_only(StationCode="c", Active="i",
                                                Lat="c", Long="c", Comments="c",
                                                `Channel/Shoal`="i", `WGS84 Lat`="d", `WGS84 Long`="d"))%>%
  select(Station=StationCode, Active, Lat, Long, Station_notes=Comments, Lat2=`WGS84 Lat`, Long2=`WGS84 Long`)%>%
  separate(Lat, into=c("Lat_d", "Lat_m", "Lat_s"), sep="[ ]{1,}", convert=T)%>%
  separate(Long, into=c("Long_d", "Long_m", "Long_s"), sep="[ ]{1,}", convert=T)%>%
  mutate(Latitude=Lat_d+Lat_m/60+Lat_s/3600,
         Longitude=Long_d-Long_m/60-Long_s/3600)%>%
  mutate(Latitude=if_else(is.na(Latitude), Lat2, Latitude),
         Longitude=if_else(is.na(Longitude), Long2, Longitude))%>%
  select(Station, Latitude, Longitude, Active)%>%
  drop_na(Station, Latitude, Longitude)

date_fmwt <- read_csv(file.path("data-raw", "FMWT", "Date.csv"), col_types=cols_only(DateID="i", SampleDate="c"))%>%
  mutate(SampleDate=parse_date_time(SampleDate, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
  rename(Date=SampleDate)

sample_fmwt <- read_csv(file.path("data-raw", "FMWT", "Sample.csv"),
                     col_types = cols_only(SampleRowID="i", StationCode="c", MethodCode="c", SampleTimeStart="c",
                                           SampleTimeEnd="c", SurveyNumber="i", WaterTemperature="d",
                                           Turbidity="d", Secchi="d", SecchiEstimated="l", ConductivityTop="d",
                                           ConductivityBottom="d", TowDirectionCode="i", MeterStart="d",
                                           MeterEnd="d", CableOut="d", TideCode="i", DepthBottom="d",
                                           MeterNumber="d", WeatherCode="i", Microcystis="i", WaveCode="i",
                                           WindDirection="c", BottomTemperature="d",
                                           MeterEstimate="d", DateID="i"))%>%
  left_join(date_fmwt, by="DateID")%>%
  select(-DateID)%>%
  rename(Station=StationCode, Method=MethodCode)%>%
  mutate(SampleTimeStart = parse_date_time(SampleTimeStart, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"),
         SampleTimeEnd = parse_date_time(SampleTimeEnd, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
  filter(Method=="MWTR")%>% # All rows are MWTR but just in case the data change
  mutate(Method=recode(Method, MWTR="Midwater trawl"))%>%
  left_join(stations_fmwt, by="Station")

species_fmwt <- read_csv(file.path("data-raw", "FMWT", "OrganismsLookUp.csv"), na=c("NA", "n/a"),
                         col_types=cols_only(OrganismCode="i", CommonName="c"))

catch_fmwt <- read_csv(file.path("data-raw", "FMWT", "Catch.csv"),
                       col_types = cols_only(CatchRowID="i", SampleRowID="i", OrganismCode="i", Catch="d"))%>%
  filter(!is.na(OrganismCode))%>%
  left_join(species_fmwt, by="OrganismCode")%>%
  select(-OrganismCode)

FMWT <- read_csv(file.path("data-raw", "FMWT", "Length.csv"), na=c("NA", "n/p"),
                        col_types = cols_only(CatchRowID="i", ForkLength="d", Dead="c", LengthFrequency="d"))%>%
  filter(ForkLength!=0)%>% # 0 fork length means not measured, so removing those from length table so those fish can be redistributed among measured lengths
  group_by(CatchRowID)%>%
  mutate(TotalMeasured=sum(LengthFrequency, na.rm=T))%>%
  ungroup()%>%
  left_join(catch_fmwt, by="CatchRowID")%>%
  mutate(Count = (LengthFrequency/TotalMeasured)*Catch)%>%
  select(-CatchRowID, -Catch)%>%
  inner_join(sample_fmwt, by="SampleRowID")

usethis::use_data(FMWT, overwrite=TRUE)
