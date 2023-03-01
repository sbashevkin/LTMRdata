## Data retrieval script for CDFW's Summer Townet Survey.
## Database is too large to store on GitHub, so query now and save smaller csv files.

#########################################################################################
## Retrieve STN database copy, save tables, delete database.
## Only run this section when needed.

library(rvest)
library(dplyr)
library(readr)

## STN database url and file names:
dbName <- read_html("https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/TNS%20MS%20Access%20Data/TNS%20data/") %>%
  html_elements("a") %>%
  html_attr("href") %>%
  {.[[which(grepl("STN.*\\.accdb", .))]]} %>%
  gsub(".*\\/", "", .)

surveyURL <- paste0("https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl",
                    "/TNS%20MS%20Access%20Data/TNS%20data/",dbName)

download.file(url=surveyURL, destfile=file.path(tempdir(), dbName), mode="wb")

db_path <- file.path(tempdir(), dbName)

# Connecting to the Access db
source(file.path("data-raw", "bridgeAccess.R"))

keepTables <- c("Catch","Length","luMicrocystis","luOrganism","luStation",
                "luTide","luTowDirection","Sample","TowEffort",
                "Web_Local_Meter_Corrections")

STNTables <- bridgeAccess(db_path,
                          tables = keepTables,
                          script = file.path("data-raw", "connectAccess.R"))

# # If you've chosen to read csv --------------------------------------------
# STNTables <- list()
#
# STNTables$Catch <- read_csv(file.path("data-raw", "STN","Catch.csv"),
#                   col_types=cols_only(CatchRowID="d", TowRowID="d", OrganismCode="d", Catch="d"))
#
# STNTables$Length <- read_csv(file.path("data-raw", "STN","Length.csv"),
#                    col_types=cols_only(LengthRowID="d", CatchRowID="d", ForkLength="d", LengthFrequency="d"))
#
# STNTables$luStation <- read_csv(file.path("data-raw", "STN","luStation.csv"),
#                       col_types=cols_only(StationCodeSTN="c", LatD="d", LatM="d", LatS="d", LonD="d", LonM="d", LonS="d"))
#
# STNTables$luTide <- read_csv(file.path("data-raw", "STN","luTide.csv"),
#                    col_types=cols_only(TideDesc="c", TideRowID="d"))
#
# STNTables$luTowDirection <- read_csv(file.path("data-raw", "STN","luTowDirection.csv"),
#                            col_types=cols_only(TowDirection="c", TowDirectionID="d"))
#
# STNTables$Sample <- read_csv(file.path("data-raw", "STN","Sample.csv"),
#                    col_types=cols_only(SampleRowID="d", SampleDate="c", StationCode="c", Survey="d",
#                                        TemperatureTop="d", Secchi="d", ConductivityTop="d",
#                                        TideCode="d", DepthBottom="d", CableOut="d", TowDirection="d")) %>%
#   mutate(SampleDate = as.Date(SampleDate, "%m/%d/%Y"))
#
# STNTables$TowEffort <- read_csv(file.path("data-raw", "STN","TowEffort.csv"),
#                       col_types=cols_only(TimeStart="c", TowRowID="d", SampleRowID="d", TowNumber="d",
#                                           MeterSerial="d", MeterIn="d", MeterOut="d",
#                                           MeterDifference="d", MeterEstimate="d"))
#
# STNTables$Web_Local_Meter_Corrections <- read_csv(file.path("data-raw", "STN",
#                                                   "Web_Local_Meter_Corrections.csv"),
#                                         col_types=cols_only(`Study Year`="d", `Meter Serial`="d", `k factor`="d")) %>%
#   rename_with(~gsub("\\s", ".", .x), everything())

#########################################################################################
## Create and save compressed data files using raw tables.

library(lubridate)
library(wql)
require(stringr)
require(LTMRdata)

Catch <- STNTables$Catch%>%select(CatchRowID,TowRowID,OrganismCode,Catch)

Length <- STNTables$Length%>%select(LengthRowID,CatchRowID,ForkLength,LengthFrequency)

luStation <- STNTables$luStation%>%select(StationCodeSTN, LatD, LatM, LatS, LonD, LonM, LonS)

luTide <- STNTables$luTide%>%select(TideDesc,TideRowID)

luTowDirection <- STNTables$luTowDirection%>%select(TowDirection, TowDirectionID)

Sample <- STNTables$Sample%>%select(SampleRowID, SampleDate, StationCode, Survey,
                                    TemperatureTop, Secchi, ConductivityTop,TideCode,
                                    DepthBottom, CableOut,TowDirection)

TowEffort <- STNTables$TowEffort%>%select(TimeStart, TowRowID, SampleRowID, TowNumber,
                                       MeterSerial, MeterIn, MeterOut,
                                       MeterDifference, MeterEstimate)

Web_Local_Meter_Corrections <- STNTables$Web_Local_Meter_Corrections%>%select(Study.Year, Meter.Serial, k.factor)%>%rename("Study Year"=Study.Year,"Meter Serial"=Meter.Serial,"k factor"=k.factor)


suspect_fm_TowRowID <- c(6612,12815,2551,12786,8031,1857)
# subset(TowEffort, TowRowID %in% suspect_fm_TowRowID)


sampleSTN <- Sample %>%
  inner_join(TowEffort, by="SampleRowID",multiple="all") %>%
  left_join(luStation, by=c("StationCode"="StationCodeSTN")) %>%
  mutate(#Date=parse_date_time(SampleDate, "%m/%d/%Y %H:%M:%S",
                             # tz="America/Los_Angeles"),
         Year= year(SampleDate)) %>%
  left_join(Web_Local_Meter_Corrections,
            by=c("Year"="Study Year","MeterSerial"="Meter Serial")) %>%
  mutate(MeterTotal=ifelse((MeterOut - MeterIn) < 0,
                           (MeterOut + 1000000 - MeterIn),
                           (MeterOut - MeterIn)),
         TowVolm3=ifelse(is.na(MeterIn), 735, `k factor`*MeterTotal*1.49),
         TowVolm3=ifelse(is.na(TowVolm3), 735, TowVolm3),
         ## Use CDFW default tow volume when comments indicate
         ## something went wrong with the flow meter or resulting
         ## volume is unusually small or large:
         TowVolm3=ifelse(TowRowID %in% suspect_fm_TowRowID, 735,
                         TowVolm3)) %>%
  arrange(SampleDate, Survey, StationCode, TowNumber) %>%
  mutate(Source="STN",
         TowNumber=ifelse(SampleRowID==7078 & TowNumber==1 & TowRowID==12153, 2, TowNumber),
         SampleID=paste(Source, SampleDate, Survey, StationCode, TowNumber),
         Method="STN net",
         TowTime=str_split(TimeStart, " ")[[1]][2], #Select time which always follows a space
         Datetime=paste(SampleDate, TowTime),
         Datetime=parse_date_time(ifelse(is.na(TowTime), NA_character_,
                                          Datetime),
                                  "%Y-%m-%d %H:%M:%S",
                                  tz="America/Los_Angeles"),
         Depth=DepthBottom*0.3048, # Convert depth from feet to m
         Cable_length=CableOut*0.3048, # Convert to m from feet
         Temp_surf=TemperatureTop, # degrees Celsius
         ## Secchi is already in cm.

         ## Convert conductivity to salinity.
         ## ConductivityTop is in micro-S/cm at 25 degrees Celsius.
         ## Input for ec2pss should be in milli-S/cm.
         Sal_surf=wql::ec2pss(ConductivityTop/1000, t=25),
         Latitude=(LatD + LatM/60 + LatS/3600),
         Longitude= -(LonD + LonM/60 + LonS/3600)) %>%
  left_join(luTide, by=c("TideCode"="TideRowID")) %>%
  mutate(TideDesc=recode(TideDesc, `High Tide`="High Slack", `Low Tide`="Low Slack"))%>%
  left_join(luTowDirection, by=c("TowDirection"="TowDirectionID")) %>%
  rename(Tide=TideDesc,
         Tow_direction=TowDirection.y,
         TowNum=TowNumber,
         Tow_volume=TowVolm3,
         Station=StationCode,
         Date=SampleDate) %>%
  mutate(Tow_direction=recode(Tow_direction, `Against Current`="Against current", `With Current`="With current"))%>%
  select(TowRowID, Source, Station, Latitude, Longitude, Date, Datetime,
         Survey, TowNum, Depth, SampleID, Method, Tide, Sal_surf,
         Temp_surf, Secchi, Tow_volume, Tow_direction, Cable_length)


fish_totalCatch <- Catch %>%
  dplyr::filter(!is.na(Catch) & Catch > 0)

Length_measured<-Length%>%
  dplyr::filter(ForkLength!=0)%>%
  group_by(CatchRowID, ForkLength)%>%
  summarise(LengthFrequency=sum(LengthFrequency), .groups="drop")


fish_adjustedCount <- fish_totalCatch %>%
  left_join(Length_measured,
            by="CatchRowID",multiple="all") %>%
  group_by(TowRowID, CatchRowID, OrganismCode) %>%
  mutate(TotalMeasured=sum(LengthFrequency, na.rm=TRUE)) %>%
  ungroup() %>%
  ## Add total catch numbers:
  ## There are some cases where the number of fish measured is greater than the
  ## catch value in the Catch table. In these cases, use the number measured.
  mutate(CatchNew=ifelse(TotalMeasured > Catch, TotalMeasured, Catch)) %>%
  ## Calculate length-frequency-adjusted counts:
  mutate(Count=(LengthFrequency/TotalMeasured)*CatchNew) %>%
  left_join(Species %>% ## Add species names
              select(STN_Code, Taxa) %>%
              dplyr::filter(!is.na(STN_Code)),
            by=c("OrganismCode"="STN_Code"))


## Examine the cases where number measured > catch:
count_mismatch <- subset(fish_adjustedCount, CatchNew != Catch)
nrow(count_mismatch)


## Join sample and fish info:
names(sampleSTN)
names(fish_adjustedCount)
intersect(names(sampleSTN), names(fish_adjustedCount))

STN <- sampleSTN %>%
  left_join(fish_adjustedCount %>%
              select(TowRowID, OrganismCode, Taxa,
                     ForkLength, LengthFrequency,
                     Catch, CatchNew, Count),
            by="TowRowID",multiple="all") %>%
  ## Add reasoning for any NA lengths:
  mutate(Length_NA_flag=if_else(is.na(Catch), "No fish caught",
                                NA_character_),
         Station=as.character(Station),
         Taxa=stringr::str_remove(Taxa, " \\((.*)"))  # Remove life stage info from Taxa names)

## no lengths for calculating adjusted length frequencies (Count):
## use the Catch value from Catch as Count and change Length_NA_flag.
index_1 <- which(!is.na(STN$Catch) & is.na(STN$Count))
STN$Count[index_1] <- STN$Catch[index_1]
STN$Length_NA_flag[index_1] <- "Unknown length"


## Check that all organism code values are represented in Species:
all(STN$OrganismCode %in% Species$STN_Code)

STN<-STN%>%
  select(-CatchNew, Catch, LengthFrequency)%>%
  group_by(across(-Count))%>% # Add up any new multiples after removing lifestages
  summarise(Count=sum(Count), .groups="drop")%>%
  mutate(ForkLength=as.numeric(ForkLength),
         Count=if_else(Length_NA_flag=="No fish caught", 0, Count, missing=Count)) # Transform all counts for 'No fish caught' to 0.

## Create final measured lengths data frame:
STN_measured_lengths <- STN %>%
  select(SampleID, Taxa, ForkLength, LengthFrequency) %>%
  dplyr::filter(!is.na(LengthFrequency))%>% # Remove fish that weren't measured
  rename(Length=ForkLength,
         Count=LengthFrequency)

nrow(STN_measured_lengths)
ncol(STN_measured_lengths)
names(STN_measured_lengths)


## Create final catch data frame:
STN <- STN %>%
  rename(Length=ForkLength) %>%
  select(-TowRowID, -OrganismCode, -LengthFrequency, -Catch)

nrow(STN)
ncol(STN)
names(STN)


## Save compressed data to /data:
usethis::use_data(STN, STN_measured_lengths, overwrite=TRUE)
