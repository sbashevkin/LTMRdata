
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
require(LTMRdata)

raw_data <- file.path("data-raw","STN")

Catch <- read.csv(file.path(raw_data,"Catch.csv"), stringsAsFactors=FALSE)
Length <- read.csv(file.path(raw_data,"Length.csv"), stringsAsFactors=FALSE)
luMicrocystis <- read.csv(file.path(raw_data,"luMicrocystis.csv"), stringsAsFactors=FALSE)
luOrganism <- read.csv(file.path(raw_data,"luOrganism.csv"), stringsAsFactors=FALSE)
luStation <- read.csv(file.path(raw_data,"luStation.csv"), stringsAsFactors=FALSE)
luTide <- read.csv(file.path(raw_data,"luTide.csv"), stringsAsFactors=FALSE)
luTowDirection <- read.csv(file.path(raw_data,"luTowDirection.csv"), stringsAsFactors=FALSE)
Sample <- read.csv(file.path(raw_data,"Sample.csv"), stringsAsFactors=FALSE)
TowEffort <- read.csv(file.path(raw_data,"TowEffort.csv"), stringsAsFactors=FALSE)
Web_Local_Meter_Corrections <- read.csv(file.path(raw_data,
                                                  "Web_Local_Meter_Corrections.csv"),
                                        stringsAsFactors=FALSE)


suspect_fm_TowRowID <- c(6612,12815,2551,12786,8031,1857)
# subset(TowEffort, TowRowID %in% suspect_fm_TowRowID)


sampleSTN <- Sample %>%
	dplyr::inner_join(TowEffort, by="SampleRowID") %>%
	dplyr::left_join(luStation, by=c("StationCode"="StationCodeSTN")) %>%
  dplyr::mutate(SampleDate=as.Date(SampleDate),
                Year=lubridate::year(SampleDate)) %>%
	dplyr::left_join(Web_Local_Meter_Corrections,
	                 by=c("Year"="Study.Year","MeterSerial"="Meter.Serial")) %>%
	dplyr::mutate(MeterTotal=ifelse((MeterOut - MeterIn) < 0,
																	(MeterOut + 1000000 - MeterIn),
																	(MeterOut - MeterIn)),
								TowVolm3=ifelse(is.na(MeterIn), 735, k.factor*MeterTotal*1.49),
								TowVolm3=ifelse(is.na(TowVolm3), 735, TowVolm3),
								## Use CDFW default tow volume when comments indicate
								## something went wrong with the flow meter or resulting
								## volume is unusually small or large:
								TowVolm3=ifelse(TowRowID %in% suspect_fm_TowRowID, 735,
								                TowVolm3)) %>%
  dplyr::arrange(SampleDate, Survey, StationCode, TowNumber) %>%
  dplyr::mutate(Source="STN",
                TowNumber=if_else(SampleRowID==7078 & TowNumber==1 & TowRowID==12153, as.integer(2), TowNumber),
                SampleID=paste(Source, SampleDate, Survey, StationCode, TowNumber),
                Method="STN Net",
                TowTime=substring(TimeStart,12),
                Datetime=paste(SampleDate, TowTime),
                Date=parse_date_time(SampleDate, "%Y-%m-%d",
                                     tz="America/Los_Angeles"),
                Datetime=parse_date_time(if_else(is.na(TowTime), NA_character_,
                                                 Datetime),
                                         "%Y-%m-%d %%H:%M:%S",
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
  dplyr::left_join(luMicrocystis, by=c("Microcystis"="MicrocystisID")) %>%
  dplyr::left_join(luTide, by=c("TideCode"="TideRowID")) %>%
  dplyr::left_join(luTowDirection, by=c("TowDirection"="TowDirectionID")) %>%
  dplyr::rename(Tide=TideDesc,
                Tow_direction=TowDirection.y,
                TowNum=TowNumber,
                Tow_volume=TowVolm3,
                Station=StationCode) %>%
  dplyr::select(TowRowID, Source, Station, Latitude, Longitude, Date, Datetime,
                Survey, TowNum, Depth, SampleID, Method, Tide, Sal_surf,
                Temp_surf, Secchi, Tow_volume, Tow_direction, Cable_length)


fish_totalCatch <- Catch %>%
  dplyr::filter(!is.na(Catch) & Catch > 0)

Length_measured<-Length%>%
  filter(ForkLength!=0)%>%
  group_by(CatchRowID, ForkLength)%>%
  summarise(LengthFrequency=sum(LengthFrequency), .groups="drop")


fish_adjustedCount <- fish_totalCatch %>%
  dplyr::left_join(Length_measured,
									 by="CatchRowID") %>%
  dplyr::group_by(TowRowID, CatchRowID, OrganismCode) %>%
  dplyr::mutate(TotalMeasured=sum(LengthFrequency, na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
	## Add total catch numbers:
	## There are some cases where the number of fish measured is greater than the
	## catch value in the Catch table. In these cases, use the number measured.
	dplyr::mutate(CatchNew=ifelse(TotalMeasured > Catch, TotalMeasured, Catch)) %>%
	## Calculate length-frequency-adjusted counts:
  dplyr::mutate(Count=(LengthFrequency/TotalMeasured)*CatchNew) %>%
  dplyr::left_join(Species %>% ## Add species names
										dplyr::select(STN_Code, Taxa) %>%
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
	dplyr::left_join(fish_adjustedCount %>%
										dplyr::select(TowRowID, OrganismCode, Taxa,
										              ForkLength, LengthFrequency,
										              Catch, CatchNew, Count),
									 by="TowRowID") %>%
	## Add reasoning for any NA lengths:
  dplyr::mutate(Length_NA_flag=if_else(is.na(Catch), "No fish caught",
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
  mutate(ForkLength=as.numeric(ForkLength))

## Create final measured lengths data frame:
STN_measured_lengths <- STN %>%
	dplyr::select(SampleID, Taxa, ForkLength, LengthFrequency) %>%
  dplyr::rename(Length=ForkLength,
                Count=LengthFrequency)

nrow(STN_measured_lengths)
ncol(STN_measured_lengths)
names(STN_measured_lengths)


## Create final catch data frame:
STN <- STN %>%
  dplyr::rename(Length=ForkLength) %>%
  dplyr::select(-TowRowID, -OrganismCode, -LengthFrequency, -Catch)

nrow(STN)
ncol(STN)
names(STN)


## Save compressed data to /data:
usethis::use_data(STN, STN_measured_lengths, overwrite=TRUE)


