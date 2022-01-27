
## Data retrieval script for CDFW's Summer Townet Survey.
## Database is too large to store on GitHub, so query now and save smaller csv files.

#########################################################################################
## Retrieve STN database copy, save tables, delete database.
## Only run this section when needed.

library(DBI)
library(odbc)

## STN database url and file names:
dbName <- "STN_Data1959-2020.accdb"
surveyURL <- paste0("https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl",
										"/TNS%20MS%20Access%20Data/TNS%20data/",dbName)
tmpFile <- file.path(tempdir(), dbName)

downloadCheck <- download.file(url=surveyURL, destfile=tmpFile, mode="wb")
downloadCheck

## Open connection to the database:
dbString <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
									 "Dbq=",tmpFile)
con <- DBI::dbConnect(drv=odbc::odbc(), .connection_string=dbString)

tables <- odbc::dbListTables(conn=con)
tables

## Save select tables:
keepTables <- c("Catch","Length","luMicrocystis","luOrganism","luStation",
								"luTide","luTowDirection","Sample","TowEffort",
								"Web_Local_Meter_Corrections")
for(tab in keepTables) {
	tmp <- DBI::dbReadTable(con, tab)
	write.csv(tmp, file=file.path("data-raw","STN",paste0(tab,".csv")), row.names=FALSE)
}

## Disconnect from database and remove original files:
DBI::dbDisconnect(conn=con)
unlink(tmpFile)


#########################################################################################
## Create and save compressed data files using raw tables.

library(dplyr)
library(lubridate)
library(wql)
require(readr)
require(stringr)
require(LTMRdata)

raw_data <- file.path("data-raw","STN")

Catch <- read_csv(file.path(raw_data,"Catch.csv"),
                  col_types=cols_only(CatchRowID="d", TowRowID="d", OrganismCode="d", Catch="d"))
Length <- read_csv(file.path(raw_data,"Length.csv"),
                   col_types=cols_only(LengthRowID="d", CatchRowID="d", ForkLength="d", LengthFrequency="d"))
luStation <- read_csv(file.path(raw_data,"luStation.csv"),
                      col_types=cols_only(StationCodeSTN="c", LatD="d", LatM="d", LatS="d", LonD="d", LonM="d", LonS="d"))
luTide <- read_csv(file.path(raw_data,"luTide.csv"),
                   col_types=cols_only(TideDesc="c", TideRowID="d"))
luTowDirection <- read_csv(file.path(raw_data,"luTowDirection.csv"),
                           col_types=cols_only(TowDirection="c", TowDirectionID="d"))
Sample <- read_csv(file.path(raw_data,"Sample.csv"),
                   col_types=cols_only(SampleRowID="d", SampleDate="c", StationCode="c", Survey="d",
                                       TemperatureTop="d", Secchi="d", ConductivityTop="d",
                                       TideCode="d", DepthBottom="d", CableOut="d", TowDirection="d"))
TowEffort <- read_csv(file.path(raw_data,"TowEffort.csv"),
                      col_types=cols_only(TimeStart="c", TowRowID="d", SampleRowID="d", TowNumber="d",
                                          MeterSerial="d", MeterIn="d", MeterOut="d",
                                          MeterDifference="d", MeterEstimate="d"))
Web_Local_Meter_Corrections <- read_csv(file.path(raw_data,
                                                  "Web_Local_Meter_Corrections.csv"),
                                        col_types=cols_only(`Study Year`="d", `Meter Serial`="d", `k factor`="d"))


suspect_fm_TowRowID <- c(6612,12815,2551,12786,8031,1857)
# subset(TowEffort, TowRowID %in% suspect_fm_TowRowID)


sampleSTN <- Sample %>%
	inner_join(TowEffort, by="SampleRowID") %>%
	left_join(luStation, by=c("StationCode"="StationCodeSTN")) %>%
  mutate(Date=parse_date_time(SampleDate, "%m/%d/%Y %H:%M:%S",
                                           tz="America/Los_Angeles"),
                Year=year(Date)) %>%
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
  arrange(Date, Survey, StationCode, TowNumber) %>%
  mutate(Source="STN",
                TowNumber=if_else(SampleRowID==7078 & TowNumber==1 & TowRowID==12153, 2, TowNumber),
                SampleID=paste(Source, Date, Survey, StationCode, TowNumber),
                Method="STN Net",
                TowTime=str_split(TimeStart, " ")[[1]][2], #Select time which always follows a space
                Datetime=paste(Date, TowTime),
                Datetime=parse_date_time(if_else(is.na(TowTime), NA_character_,
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
                Station=StationCode) %>%
  select(TowRowID, Source, Station, Latitude, Longitude, Date, Datetime,
                Survey, TowNum, Depth, SampleID, Method, Tide, Sal_surf,
                Temp_surf, Secchi, Tow_volume, Tow_direction, Cable_length)


fish_totalCatch <- Catch %>%
  filter(!is.na(Catch) & Catch > 0)

Length_measured<-Length%>%
  filter(ForkLength!=0)%>%
  group_by(CatchRowID, ForkLength)%>%
  summarise(LengthFrequency=sum(LengthFrequency), .groups="drop")


fish_adjustedCount <- fish_totalCatch %>%
  left_join(Length_measured,
									 by="CatchRowID") %>%
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
										filter(!is.na(STN_Code)),
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
									 by="TowRowID") %>%
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
  filter(!is.na(LengthFrequency))%>% # Remove fish that weren't measured
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


