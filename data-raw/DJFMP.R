# Package ID: edi.244.11 Cataloging System:https://pasta.edirepository.org.
# Data set title: Interagency Ecological Program: Over four decades of juvenile fish monitoring data from the San Francisco Estuary, collected by the Delta Juvenile Fish Monitoring Program, 1976-2022.
# Data set creator:   Interagency Ecological Program (IEP) -
# Data set creator:  Jonathan Speegle - United States Fish and Wildlife Service
# Data set creator:  Ryan McKenzie - United States Fish and Wildlife Service
# Data set creator:  Adam Nanninga - United States Fish and Wildlife Service
# Data set creator:  Erika Holcombe - United States Fish and Wildlife Service
# Data set creator:  Jacob Stagg - United States Fish and Wildlife Service
# Data set creator:  Jackie Hagen - United States Fish and Wildlife Service
# Data set creator:  Eric Huber - United States Fish and Wildlife Service
# Data set creator:  Geoffrey Steinhart - United States Fish and Wildlife Service
# Data set creator:  Adriana Arrambide - United States Fish and Wildlife Service
# Contact:  Jonathan Speegle Data Manager -  United States Fish and Wildlife Service  - jonathan_speegle@fws.gov
# Contact:  Ryan McKenzie Fish Biologist -  United States Fish and Wildlife Service  - ryan_mckenzie@fws.gov
# Contact:  Adam Nanninga Field Crew Supervisor -  United States Fish and Wildlife Service  - adam_nanninga@fws.gov
# Contact:  Erika Holcombe Field Crew Supervisor -  United States Fish and Wildlife Service  - john_cook@fws.gov
# Contact:  Jacob Stagg Field Crew Supervisor -  United States Fish and Wildlife Service  - jacob_stagg@fws.gov
# Contact:  Jackie Hagen Small Craft Operator Supervisor -  United States Fish and Wildlife Service  - jackie_hagen@fws.gov
# Contact:  Eric Huber Supervisory Fish Biologist -  United States Fish and Wildlife Service  - eric_huber@fws.gov
# Contact:  Geoffrey Steinhart Supervisory Fish Biologist -  United States Fish and Wildlife Service  - geoffrey_stenhart@fws.gov
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu

require(wql)
require(LTMRdata)
require(readr)
require(dplyr)
require(lubridate)
require(hms)
require(stringr)

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/244/11/71c16ead9b8ffa4da7a52da180f601f4"
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F
               ,skip=1
               ,sep=","
               ,quot='"'
               , col.names=c(
                 "Location",
                 "RegionCode",
                 "StationCode",
                 "SampleDate",
                 "SampleTime",
                 "MethodCode",
                 "GearConditionCode",
                 "WeatherCode",
                 "DO",
                 "WaterTemp",
                 "Turbidity",
                 "Secchi",
                 "SpecificConductance",
                 "TowNumber",
                 "SamplingDirection",
                 "TowDuration",
                 "FlowDebris",
                 "SiteDisturbance",
                 "AlternateSite",
                 "SeineLength",
                 "SeineWidth",
                 "SeineDepth",
                 "FlowmeterStart",
                 "FlowmeterEnd",
                 "FlowmeterDifference",
                 "Volume",
                 "OrganismCode",
                 "IEPFishCode",
                 "CommonName",
                 "MarkCode",
                 "StageCode",
                 "Expression",
                 "ForkLength",
                 "RaceByLength",
                 "TagCode",
                 "RaceByTag",
                 "ArchivalID",
                 "SpecialStudyID",
                 "GeneticID",
                 "Probability1",
                 "GeneticID2",
                 "Probability2",
                 "SexGeneID",
                 "Ots28",
                 "Lab",
                 "GeneticTest",
                 "GeneticModel",
                 "Count"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Location)!="factor") dt1$Location<- as.factor(dt1$Location)
if (class(dt1$RegionCode)!="factor") dt1$RegionCode<- as.factor(dt1$RegionCode)
if (class(dt1$StationCode)!="factor") dt1$StationCode<- as.factor(dt1$StationCode)
# attempting to convert dt1$SampleDate dateTime string to R date structure (date or POSIXct)
tmpDateFormat<-"%Y-%m-%d"
tmp1SampleDate<-as.Date(dt1$SampleDate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1SampleDate) == length(tmp1SampleDate[!is.na(tmp1SampleDate)])){dt1$SampleDate <- tmp1SampleDate } else {print("Date conversion failed for dt1$SampleDate. Please inspect the data and do the date conversion yourself.")}
rm(tmpDateFormat,tmp1SampleDate)
if (class(dt1$MethodCode)!="factor") dt1$MethodCode<- as.factor(dt1$MethodCode)
if (class(dt1$GearConditionCode)!="factor") dt1$GearConditionCode<- as.factor(dt1$GearConditionCode)
if (class(dt1$WeatherCode)!="factor") dt1$WeatherCode<- as.factor(dt1$WeatherCode)
if (class(dt1$DO)=="factor") dt1$DO <-as.numeric(levels(dt1$DO))[as.integer(dt1$DO) ]
if (class(dt1$DO)=="character") dt1$DO <-as.numeric(dt1$DO)
if (class(dt1$WaterTemp)=="factor") dt1$WaterTemp <-as.numeric(levels(dt1$WaterTemp))[as.integer(dt1$WaterTemp) ]
if (class(dt1$WaterTemp)=="character") dt1$WaterTemp <-as.numeric(dt1$WaterTemp)
if (class(dt1$Turbidity)=="factor") dt1$Turbidity <-as.numeric(levels(dt1$Turbidity))[as.integer(dt1$Turbidity) ]
if (class(dt1$Turbidity)=="character") dt1$Turbidity <-as.numeric(dt1$Turbidity)
if (class(dt1$Secchi)=="factor") dt1$Secchi <-as.numeric(levels(dt1$Secchi))[as.integer(dt1$Secchi) ]
if (class(dt1$Secchi)=="character") dt1$Secchi <-as.numeric(dt1$Secchi)
if (class(dt1$SpecificConductance)=="factor") dt1$SpecificConductance <-as.numeric(levels(dt1$SpecificConductance))[as.integer(dt1$SpecificConductance) ]
if (class(dt1$SpecificConductance)=="character") dt1$SpecificConductance <-as.numeric(dt1$SpecificConductance)
if (class(dt1$TowNumber)=="factor") dt1$TowNumber <-as.numeric(levels(dt1$TowNumber))[as.integer(dt1$TowNumber) ]
if (class(dt1$TowNumber)=="character") dt1$TowNumber <-as.numeric(dt1$TowNumber)
if (class(dt1$SamplingDirection)!="factor") dt1$SamplingDirection<- as.factor(dt1$SamplingDirection)
if (class(dt1$TowDuration)=="factor") dt1$TowDuration <-as.numeric(levels(dt1$TowDuration))[as.integer(dt1$TowDuration) ]
if (class(dt1$TowDuration)=="character") dt1$TowDuration <-as.numeric(dt1$TowDuration)
if (class(dt1$FlowDebris)!="factor") dt1$FlowDebris<- as.factor(dt1$FlowDebris)
if (class(dt1$SiteDisturbance)!="factor") dt1$SiteDisturbance<- as.factor(dt1$SiteDisturbance)
if (class(dt1$AlternateSite)!="factor") dt1$AlternateSite<- as.factor(dt1$AlternateSite)
if (class(dt1$SeineLength)=="factor") dt1$SeineLength <-as.numeric(levels(dt1$SeineLength))[as.integer(dt1$SeineLength) ]
if (class(dt1$SeineLength)=="character") dt1$SeineLength <-as.numeric(dt1$SeineLength)
if (class(dt1$SeineWidth)=="factor") dt1$SeineWidth <-as.numeric(levels(dt1$SeineWidth))[as.integer(dt1$SeineWidth) ]
if (class(dt1$SeineWidth)=="character") dt1$SeineWidth <-as.numeric(dt1$SeineWidth)
if (class(dt1$SeineDepth)=="factor") dt1$SeineDepth <-as.numeric(levels(dt1$SeineDepth))[as.integer(dt1$SeineDepth) ]
if (class(dt1$SeineDepth)=="character") dt1$SeineDepth <-as.numeric(dt1$SeineDepth)
if (class(dt1$FlowmeterStart)=="factor") dt1$FlowmeterStart <-as.numeric(levels(dt1$FlowmeterStart))[as.integer(dt1$FlowmeterStart) ]
if (class(dt1$FlowmeterStart)=="character") dt1$FlowmeterStart <-as.numeric(dt1$FlowmeterStart)
if (class(dt1$FlowmeterEnd)=="factor") dt1$FlowmeterEnd <-as.numeric(levels(dt1$FlowmeterEnd))[as.integer(dt1$FlowmeterEnd) ]
if (class(dt1$FlowmeterEnd)=="character") dt1$FlowmeterEnd <-as.numeric(dt1$FlowmeterEnd)
if (class(dt1$FlowmeterDifference)=="factor") dt1$FlowmeterDifference <-as.numeric(levels(dt1$FlowmeterDifference))[as.integer(dt1$FlowmeterDifference) ]
if (class(dt1$FlowmeterDifference)=="character") dt1$FlowmeterDifference <-as.numeric(dt1$FlowmeterDifference)
if (class(dt1$Volume)=="factor") dt1$Volume <-as.numeric(levels(dt1$Volume))[as.integer(dt1$Volume) ]
if (class(dt1$Volume)=="character") dt1$Volume <-as.numeric(dt1$Volume)
if (class(dt1$OrganismCode)!="factor") dt1$OrganismCode<- as.factor(dt1$OrganismCode)
if (class(dt1$IEPFishCode)!="factor") dt1$IEPFishCode<- as.factor(dt1$IEPFishCode)
if (class(dt1$CommonName)!="factor") dt1$CommonName<- as.factor(dt1$CommonName)
if (class(dt1$MarkCode)!="factor") dt1$MarkCode<- as.factor(dt1$MarkCode)
if (class(dt1$StageCode)!="factor") dt1$StageCode<- as.factor(dt1$StageCode)
if (class(dt1$Expression)!="factor") dt1$Expression<- as.factor(dt1$Expression)
if (class(dt1$ForkLength)=="factor") dt1$ForkLength <-as.numeric(levels(dt1$ForkLength))[as.integer(dt1$ForkLength) ]
if (class(dt1$ForkLength)=="character") dt1$ForkLength <-as.numeric(dt1$ForkLength)
if (class(dt1$RaceByLength)!="factor") dt1$RaceByLength<- as.factor(dt1$RaceByLength)
if (class(dt1$TagCode)!="factor") dt1$TagCode<- as.factor(dt1$TagCode)
if (class(dt1$RaceByTag)!="factor") dt1$RaceByTag<- as.factor(dt1$RaceByTag)
if (class(dt1$ArchivalID)!="factor") dt1$ArchivalID<- as.factor(dt1$ArchivalID)
if (class(dt1$SpecialStudyID)!="factor") dt1$SpecialStudyID<- as.factor(dt1$SpecialStudyID)
if (class(dt1$GeneticID)!="factor") dt1$GeneticID<- as.factor(dt1$GeneticID)
if (class(dt1$Probability1)=="factor") dt1$Probability1 <-as.numeric(levels(dt1$Probability1))[as.integer(dt1$Probability1) ]
if (class(dt1$Probability1)=="character") dt1$Probability1 <-as.numeric(dt1$Probability1)
if (class(dt1$GeneticID2)!="factor") dt1$GeneticID2<- as.factor(dt1$GeneticID2)
if (class(dt1$Probability2)=="factor") dt1$Probability2 <-as.numeric(levels(dt1$Probability2))[as.integer(dt1$Probability2) ]
if (class(dt1$Probability2)=="character") dt1$Probability2 <-as.numeric(dt1$Probability2)
if (class(dt1$SexGeneID)!="factor") dt1$SexGeneID<- as.factor(dt1$SexGeneID)
if (class(dt1$Ots28)!="factor") dt1$Ots28<- as.factor(dt1$Ots28)
if (class(dt1$Lab)!="factor") dt1$Lab<- as.factor(dt1$Lab)
if (class(dt1$GeneticTest)!="factor") dt1$GeneticTest<- as.factor(dt1$GeneticTest)
if (class(dt1$GeneticModel)!="factor") dt1$GeneticModel<- as.factor(dt1$GeneticModel)
if (class(dt1$Count)=="factor") dt1$Count <-as.numeric(levels(dt1$Count))[as.integer(dt1$Count) ]
if (class(dt1$Count)=="character") dt1$Count <-as.numeric(dt1$Count)

# Convert Missing Values to NA for non-dates

dt1$Location <- as.factor(ifelse((trimws(as.character(dt1$Location))==trimws("NA")),NA,as.character(dt1$Location)))
dt1$RegionCode <- as.factor(ifelse((trimws(as.character(dt1$RegionCode))==trimws("NA")),NA,as.character(dt1$RegionCode)))
dt1$StationCode <- as.factor(ifelse((trimws(as.character(dt1$StationCode))==trimws("NA")),NA,as.character(dt1$StationCode)))
dt1$MethodCode <- as.factor(ifelse((trimws(as.character(dt1$MethodCode))==trimws("NA")),NA,as.character(dt1$MethodCode)))
dt1$GearConditionCode <- as.factor(ifelse((trimws(as.character(dt1$GearConditionCode))==trimws("NA")),NA,as.character(dt1$GearConditionCode)))
dt1$WeatherCode <- as.factor(ifelse((trimws(as.character(dt1$WeatherCode))==trimws("NA")),NA,as.character(dt1$WeatherCode)))
dt1$DO <- ifelse((trimws(as.character(dt1$DO))==trimws("NA")),NA,dt1$DO)
suppressWarnings(dt1$DO <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DO))==as.character(as.numeric("NA"))),NA,dt1$DO))
dt1$WaterTemp <- ifelse((trimws(as.character(dt1$WaterTemp))==trimws("NA")),NA,dt1$WaterTemp)
suppressWarnings(dt1$WaterTemp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$WaterTemp))==as.character(as.numeric("NA"))),NA,dt1$WaterTemp))
dt1$Turbidity <- ifelse((trimws(as.character(dt1$Turbidity))==trimws("NA")),NA,dt1$Turbidity)
suppressWarnings(dt1$Turbidity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Turbidity))==as.character(as.numeric("NA"))),NA,dt1$Turbidity))
dt1$Secchi <- ifelse((trimws(as.character(dt1$Secchi))==trimws("NA")),NA,dt1$Secchi)
suppressWarnings(dt1$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Secchi))==as.character(as.numeric("NA"))),NA,dt1$Secchi))
dt1$SpecificConductance <- ifelse((trimws(as.character(dt1$SpecificConductance))==trimws("NA")),NA,dt1$SpecificConductance)
suppressWarnings(dt1$SpecificConductance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SpecificConductance))==as.character(as.numeric("NA"))),NA,dt1$SpecificConductance))
dt1$TowNumber <- ifelse((trimws(as.character(dt1$TowNumber))==trimws("NA")),NA,dt1$TowNumber)
suppressWarnings(dt1$TowNumber <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TowNumber))==as.character(as.numeric("NA"))),NA,dt1$TowNumber))
dt1$SamplingDirection <- as.factor(ifelse((trimws(as.character(dt1$SamplingDirection))==trimws("NA")),NA,as.character(dt1$SamplingDirection)))
dt1$TowDuration <- ifelse((trimws(as.character(dt1$TowDuration))==trimws("NA")),NA,dt1$TowDuration)
suppressWarnings(dt1$TowDuration <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TowDuration))==as.character(as.numeric("NA"))),NA,dt1$TowDuration))
dt1$FlowDebris <- as.factor(ifelse((trimws(as.character(dt1$FlowDebris))==trimws("NA")),NA,as.character(dt1$FlowDebris)))
dt1$SiteDisturbance <- as.factor(ifelse((trimws(as.character(dt1$SiteDisturbance))==trimws("NA")),NA,as.character(dt1$SiteDisturbance)))
dt1$AlternateSite <- as.factor(ifelse((trimws(as.character(dt1$AlternateSite))==trimws("NA")),NA,as.character(dt1$AlternateSite)))
dt1$SeineLength <- ifelse((trimws(as.character(dt1$SeineLength))==trimws("NA")),NA,dt1$SeineLength)
suppressWarnings(dt1$SeineLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SeineLength))==as.character(as.numeric("NA"))),NA,dt1$SeineLength))
dt1$SeineWidth <- ifelse((trimws(as.character(dt1$SeineWidth))==trimws("NA")),NA,dt1$SeineWidth)
suppressWarnings(dt1$SeineWidth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SeineWidth))==as.character(as.numeric("NA"))),NA,dt1$SeineWidth))
dt1$SeineDepth <- ifelse((trimws(as.character(dt1$SeineDepth))==trimws("NA")),NA,dt1$SeineDepth)
suppressWarnings(dt1$SeineDepth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SeineDepth))==as.character(as.numeric("NA"))),NA,dt1$SeineDepth))
dt1$FlowmeterStart <- ifelse((trimws(as.character(dt1$FlowmeterStart))==trimws("NA")),NA,dt1$FlowmeterStart)
suppressWarnings(dt1$FlowmeterStart <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$FlowmeterStart))==as.character(as.numeric("NA"))),NA,dt1$FlowmeterStart))
dt1$FlowmeterEnd <- ifelse((trimws(as.character(dt1$FlowmeterEnd))==trimws("NA")),NA,dt1$FlowmeterEnd)
suppressWarnings(dt1$FlowmeterEnd <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$FlowmeterEnd))==as.character(as.numeric("NA"))),NA,dt1$FlowmeterEnd))
dt1$FlowmeterDifference <- ifelse((trimws(as.character(dt1$FlowmeterDifference))==trimws("NA")),NA,dt1$FlowmeterDifference)
suppressWarnings(dt1$FlowmeterDifference <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$FlowmeterDifference))==as.character(as.numeric("NA"))),NA,dt1$FlowmeterDifference))
dt1$Volume <- ifelse((trimws(as.character(dt1$Volume))==trimws("NA")),NA,dt1$Volume)
suppressWarnings(dt1$Volume <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Volume))==as.character(as.numeric("NA"))),NA,dt1$Volume))
dt1$OrganismCode <- as.factor(ifelse((trimws(as.character(dt1$OrganismCode))==trimws("NA")),NA,as.character(dt1$OrganismCode)))
dt1$IEPFishCode <- as.factor(ifelse((trimws(as.character(dt1$IEPFishCode))==trimws("NA")),NA,as.character(dt1$IEPFishCode)))
dt1$CommonName <- as.factor(ifelse((trimws(as.character(dt1$CommonName))==trimws("NA")),NA,as.character(dt1$CommonName)))
dt1$MarkCode <- as.factor(ifelse((trimws(as.character(dt1$MarkCode))==trimws("NA")),NA,as.character(dt1$MarkCode)))
dt1$StageCode <- as.factor(ifelse((trimws(as.character(dt1$StageCode))==trimws("NA")),NA,as.character(dt1$StageCode)))
dt1$Expression <- as.factor(ifelse((trimws(as.character(dt1$Expression))==trimws("NA")),NA,as.character(dt1$Expression)))
dt1$ForkLength <- ifelse((trimws(as.character(dt1$ForkLength))==trimws("NA")),NA,dt1$ForkLength)
suppressWarnings(dt1$ForkLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$ForkLength))==as.character(as.numeric("NA"))),NA,dt1$ForkLength))
dt1$RaceByLength <- as.factor(ifelse((trimws(as.character(dt1$RaceByLength))==trimws("NA")),NA,as.character(dt1$RaceByLength)))
dt1$TagCode <- as.factor(ifelse((trimws(as.character(dt1$TagCode))==trimws("NA")),NA,as.character(dt1$TagCode)))
dt1$RaceByTag <- as.factor(ifelse((trimws(as.character(dt1$RaceByTag))==trimws("NA")),NA,as.character(dt1$RaceByTag)))
dt1$ArchivalID <- as.factor(ifelse((trimws(as.character(dt1$ArchivalID))==trimws("NA")),NA,as.character(dt1$ArchivalID)))
dt1$SpecialStudyID <- as.factor(ifelse((trimws(as.character(dt1$SpecialStudyID))==trimws("NA")),NA,as.character(dt1$SpecialStudyID)))
dt1$GeneticID <- as.factor(ifelse((trimws(as.character(dt1$GeneticID))==trimws("NA")),NA,as.character(dt1$GeneticID)))
dt1$Probability1 <- ifelse((trimws(as.character(dt1$Probability1))==trimws("NA")),NA,dt1$Probability1)
suppressWarnings(dt1$Probability1 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Probability1))==as.character(as.numeric("NA"))),NA,dt1$Probability1))
dt1$GeneticID2 <- as.factor(ifelse((trimws(as.character(dt1$GeneticID2))==trimws("NA")),NA,as.character(dt1$GeneticID2)))
dt1$Probability2 <- ifelse((trimws(as.character(dt1$Probability2))==trimws("NA")),NA,dt1$Probability2)
suppressWarnings(dt1$Probability2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Probability2))==as.character(as.numeric("NA"))),NA,dt1$Probability2))
dt1$SexGeneID <- as.factor(ifelse((trimws(as.character(dt1$SexGeneID))==trimws("NA")),NA,as.character(dt1$SexGeneID)))
dt1$Ots28 <- as.factor(ifelse((trimws(as.character(dt1$Ots28))==trimws("NA")),NA,as.character(dt1$Ots28)))
dt1$Lab <- as.factor(ifelse((trimws(as.character(dt1$Lab))==trimws("NA")),NA,as.character(dt1$Lab)))
dt1$GeneticTest <- as.factor(ifelse((trimws(as.character(dt1$GeneticTest))==trimws("NA")),NA,as.character(dt1$GeneticTest)))
dt1$GeneticModel <- as.factor(ifelse((trimws(as.character(dt1$GeneticModel))==trimws("NA")),NA,as.character(dt1$GeneticModel)))
dt1$Count <- ifelse((trimws(as.character(dt1$Count))==trimws("NA")),NA,dt1$Count)
suppressWarnings(dt1$Count <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Count))==as.character(as.numeric("NA"))),NA,dt1$Count))


# Here is the structure of the input data frame:
str(dt1)
attach(dt1)

detach(dt1)


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/244/11/0f80e0390bfcbf548c50d40d952e03bc"
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F
               ,skip=1
               ,sep=","
               ,quot='"'
               , col.names=c(
                 "Location",
                 "RegionCode",
                 "StationCode",
                 "SampleDate",
                 "SampleTime",
                 "MethodCode",
                 "GearConditionCode",
                 "WeatherCode",
                 "DO",
                 "WaterTemp",
                 "Turbidity",
                 "Secchi",
                 "SpecificConductance",
                 "TowNumber",
                 "SamplingDirection",
                 "TowDuration",
                 "FlowDebris",
                 "SiteDisturbance",
                 "AlternateSite",
                 "SeineLength",
                 "SeineWidth",
                 "SeineDepth",
                 "FlowmeterStart",
                 "FlowmeterEnd",
                 "FlowmeterDifference",
                 "Volume",
                 "OrganismCode",
                 "IEPFishCode",
                 "CommonName",
                 "MarkCode",
                 "StageCode",
                 "Expression",
                 "ForkLength",
                 "RaceByLength",
                 "TagCode",
                 "RaceByTag",
                 "ArchivalID",
                 "SpecialStudyID",
                 "GeneticID",
                 "Probability1",
                 "GeneticID2",
                 "Probability2",
                 "SexGeneID",
                 "Ots28",
                 "Lab",
                 "GeneticTest",
                 "GeneticModel",
                 "Count"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$Location)!="factor") dt2$Location<- as.factor(dt2$Location)
if (class(dt2$RegionCode)!="factor") dt2$RegionCode<- as.factor(dt2$RegionCode)
if (class(dt2$StationCode)!="factor") dt2$StationCode<- as.factor(dt2$StationCode)
# attempting to convert dt2$SampleDate dateTime string to R date structure (date or POSIXct)
tmpDateFormat<-"%Y-%m-%d"
tmp2SampleDate<-as.Date(dt2$SampleDate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2SampleDate) == length(tmp2SampleDate[!is.na(tmp2SampleDate)])){dt2$SampleDate <- tmp2SampleDate } else {print("Date conversion failed for dt2$SampleDate. Please inspect the data and do the date conversion yourself.")}
rm(tmpDateFormat,tmp2SampleDate)
if (class(dt2$MethodCode)!="factor") dt2$MethodCode<- as.factor(dt2$MethodCode)
if (class(dt2$GearConditionCode)!="factor") dt2$GearConditionCode<- as.factor(dt2$GearConditionCode)
if (class(dt2$WeatherCode)!="factor") dt2$WeatherCode<- as.factor(dt2$WeatherCode)
if (class(dt2$DO)=="factor") dt2$DO <-as.numeric(levels(dt2$DO))[as.integer(dt2$DO) ]
if (class(dt2$DO)=="character") dt2$DO <-as.numeric(dt2$DO)
if (class(dt2$WaterTemp)=="factor") dt2$WaterTemp <-as.numeric(levels(dt2$WaterTemp))[as.integer(dt2$WaterTemp) ]
if (class(dt2$WaterTemp)=="character") dt2$WaterTemp <-as.numeric(dt2$WaterTemp)
if (class(dt2$Turbidity)=="factor") dt2$Turbidity <-as.numeric(levels(dt2$Turbidity))[as.integer(dt2$Turbidity) ]
if (class(dt2$Turbidity)=="character") dt2$Turbidity <-as.numeric(dt2$Turbidity)
if (class(dt2$Secchi)=="factor") dt2$Secchi <-as.numeric(levels(dt2$Secchi))[as.integer(dt2$Secchi) ]
if (class(dt2$Secchi)=="character") dt2$Secchi <-as.numeric(dt2$Secchi)
if (class(dt2$SpecificConductance)=="factor") dt2$SpecificConductance <-as.numeric(levels(dt2$SpecificConductance))[as.integer(dt2$SpecificConductance) ]
if (class(dt2$SpecificConductance)=="character") dt2$SpecificConductance <-as.numeric(dt2$SpecificConductance)
if (class(dt2$TowNumber)=="factor") dt2$TowNumber <-as.numeric(levels(dt2$TowNumber))[as.integer(dt2$TowNumber) ]
if (class(dt2$TowNumber)=="character") dt2$TowNumber <-as.numeric(dt2$TowNumber)
if (class(dt2$SamplingDirection)!="factor") dt2$SamplingDirection<- as.factor(dt2$SamplingDirection)
if (class(dt2$TowDuration)=="factor") dt2$TowDuration <-as.numeric(levels(dt2$TowDuration))[as.integer(dt2$TowDuration) ]
if (class(dt2$TowDuration)=="character") dt2$TowDuration <-as.numeric(dt2$TowDuration)
if (class(dt2$FlowDebris)!="factor") dt2$FlowDebris<- as.factor(dt2$FlowDebris)
if (class(dt2$SiteDisturbance)!="factor") dt2$SiteDisturbance<- as.factor(dt2$SiteDisturbance)
if (class(dt2$AlternateSite)!="factor") dt2$AlternateSite<- as.factor(dt2$AlternateSite)
if (class(dt2$SeineLength)=="factor") dt2$SeineLength <-as.numeric(levels(dt2$SeineLength))[as.integer(dt2$SeineLength) ]
if (class(dt2$SeineLength)=="character") dt2$SeineLength <-as.numeric(dt2$SeineLength)
if (class(dt2$SeineWidth)=="factor") dt2$SeineWidth <-as.numeric(levels(dt2$SeineWidth))[as.integer(dt2$SeineWidth) ]
if (class(dt2$SeineWidth)=="character") dt2$SeineWidth <-as.numeric(dt2$SeineWidth)
if (class(dt2$SeineDepth)=="factor") dt2$SeineDepth <-as.numeric(levels(dt2$SeineDepth))[as.integer(dt2$SeineDepth) ]
if (class(dt2$SeineDepth)=="character") dt2$SeineDepth <-as.numeric(dt2$SeineDepth)
if (class(dt2$FlowmeterStart)=="factor") dt2$FlowmeterStart <-as.numeric(levels(dt2$FlowmeterStart))[as.integer(dt2$FlowmeterStart) ]
if (class(dt2$FlowmeterStart)=="character") dt2$FlowmeterStart <-as.numeric(dt2$FlowmeterStart)
if (class(dt2$FlowmeterEnd)=="factor") dt2$FlowmeterEnd <-as.numeric(levels(dt2$FlowmeterEnd))[as.integer(dt2$FlowmeterEnd) ]
if (class(dt2$FlowmeterEnd)=="character") dt2$FlowmeterEnd <-as.numeric(dt2$FlowmeterEnd)
if (class(dt2$FlowmeterDifference)=="factor") dt2$FlowmeterDifference <-as.numeric(levels(dt2$FlowmeterDifference))[as.integer(dt2$FlowmeterDifference) ]
if (class(dt2$FlowmeterDifference)=="character") dt2$FlowmeterDifference <-as.numeric(dt2$FlowmeterDifference)
if (class(dt2$Volume)=="factor") dt2$Volume <-as.numeric(levels(dt2$Volume))[as.integer(dt2$Volume) ]
if (class(dt2$Volume)=="character") dt2$Volume <-as.numeric(dt2$Volume)
if (class(dt2$OrganismCode)!="factor") dt2$OrganismCode<- as.factor(dt2$OrganismCode)
if (class(dt2$IEPFishCode)!="factor") dt2$IEPFishCode<- as.factor(dt2$IEPFishCode)
if (class(dt2$CommonName)!="factor") dt2$CommonName<- as.factor(dt2$CommonName)
if (class(dt2$MarkCode)!="factor") dt2$MarkCode<- as.factor(dt2$MarkCode)
if (class(dt2$StageCode)!="factor") dt2$StageCode<- as.factor(dt2$StageCode)
if (class(dt2$Expression)!="factor") dt2$Expression<- as.factor(dt2$Expression)
if (class(dt2$ForkLength)=="factor") dt2$ForkLength <-as.numeric(levels(dt2$ForkLength))[as.integer(dt2$ForkLength) ]
if (class(dt2$ForkLength)=="character") dt2$ForkLength <-as.numeric(dt2$ForkLength)
if (class(dt2$RaceByLength)!="factor") dt2$RaceByLength<- as.factor(dt2$RaceByLength)
if (class(dt2$TagCode)!="factor") dt2$TagCode<- as.factor(dt2$TagCode)
if (class(dt2$RaceByTag)!="factor") dt2$RaceByTag<- as.factor(dt2$RaceByTag)
if (class(dt2$ArchivalID)!="factor") dt2$ArchivalID<- as.factor(dt2$ArchivalID)
if (class(dt2$SpecialStudyID)!="factor") dt2$SpecialStudyID<- as.factor(dt2$SpecialStudyID)
if (class(dt2$GeneticID)!="factor") dt2$GeneticID<- as.factor(dt2$GeneticID)
if (class(dt2$Probability1)=="factor") dt2$Probability1 <-as.numeric(levels(dt2$Probability1))[as.integer(dt2$Probability1) ]
if (class(dt2$Probability1)=="character") dt2$Probability1 <-as.numeric(dt2$Probability1)
if (class(dt2$GeneticID2)!="factor") dt2$GeneticID2<- as.factor(dt2$GeneticID2)
if (class(dt2$Probability2)=="factor") dt2$Probability2 <-as.numeric(levels(dt2$Probability2))[as.integer(dt2$Probability2) ]
if (class(dt2$Probability2)=="character") dt2$Probability2 <-as.numeric(dt2$Probability2)
if (class(dt2$SexGeneID)!="factor") dt2$SexGeneID<- as.factor(dt2$SexGeneID)
if (class(dt2$Ots28)!="factor") dt2$Ots28<- as.factor(dt2$Ots28)
if (class(dt2$Lab)!="factor") dt2$Lab<- as.factor(dt2$Lab)
if (class(dt2$GeneticTest)!="factor") dt2$GeneticTest<- as.factor(dt2$GeneticTest)
if (class(dt2$GeneticModel)!="factor") dt2$GeneticModel<- as.factor(dt2$GeneticModel)
if (class(dt2$Count)=="factor") dt2$Count <-as.numeric(levels(dt2$Count))[as.integer(dt2$Count) ]
if (class(dt2$Count)=="character") dt2$Count <-as.numeric(dt2$Count)

# Convert Missing Values to NA for non-dates

dt2$Location <- as.factor(ifelse((trimws(as.character(dt2$Location))==trimws("NA")),NA,as.character(dt2$Location)))
dt2$RegionCode <- as.factor(ifelse((trimws(as.character(dt2$RegionCode))==trimws("NA")),NA,as.character(dt2$RegionCode)))
dt2$StationCode <- as.factor(ifelse((trimws(as.character(dt2$StationCode))==trimws("NA")),NA,as.character(dt2$StationCode)))
dt2$MethodCode <- as.factor(ifelse((trimws(as.character(dt2$MethodCode))==trimws("NA")),NA,as.character(dt2$MethodCode)))
dt2$GearConditionCode <- as.factor(ifelse((trimws(as.character(dt2$GearConditionCode))==trimws("NA")),NA,as.character(dt2$GearConditionCode)))
dt2$WeatherCode <- as.factor(ifelse((trimws(as.character(dt2$WeatherCode))==trimws("NA")),NA,as.character(dt2$WeatherCode)))
dt2$DO <- ifelse((trimws(as.character(dt2$DO))==trimws("NA")),NA,dt2$DO)
suppressWarnings(dt2$DO <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$DO))==as.character(as.numeric("NA"))),NA,dt2$DO))
dt2$WaterTemp <- ifelse((trimws(as.character(dt2$WaterTemp))==trimws("NA")),NA,dt2$WaterTemp)
suppressWarnings(dt2$WaterTemp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$WaterTemp))==as.character(as.numeric("NA"))),NA,dt2$WaterTemp))
dt2$Turbidity <- ifelse((trimws(as.character(dt2$Turbidity))==trimws("NA")),NA,dt2$Turbidity)
suppressWarnings(dt2$Turbidity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Turbidity))==as.character(as.numeric("NA"))),NA,dt2$Turbidity))
dt2$Secchi <- ifelse((trimws(as.character(dt2$Secchi))==trimws("NA")),NA,dt2$Secchi)
suppressWarnings(dt2$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Secchi))==as.character(as.numeric("NA"))),NA,dt2$Secchi))
dt2$SpecificConductance <- ifelse((trimws(as.character(dt2$SpecificConductance))==trimws("NA")),NA,dt2$SpecificConductance)
suppressWarnings(dt2$SpecificConductance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SpecificConductance))==as.character(as.numeric("NA"))),NA,dt2$SpecificConductance))
dt2$TowNumber <- ifelse((trimws(as.character(dt2$TowNumber))==trimws("NA")),NA,dt2$TowNumber)
suppressWarnings(dt2$TowNumber <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TowNumber))==as.character(as.numeric("NA"))),NA,dt2$TowNumber))
dt2$SamplingDirection <- as.factor(ifelse((trimws(as.character(dt2$SamplingDirection))==trimws("NA")),NA,as.character(dt2$SamplingDirection)))
dt2$TowDuration <- ifelse((trimws(as.character(dt2$TowDuration))==trimws("NA")),NA,dt2$TowDuration)
suppressWarnings(dt2$TowDuration <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TowDuration))==as.character(as.numeric("NA"))),NA,dt2$TowDuration))
dt2$FlowDebris <- as.factor(ifelse((trimws(as.character(dt2$FlowDebris))==trimws("NA")),NA,as.character(dt2$FlowDebris)))
dt2$SiteDisturbance <- as.factor(ifelse((trimws(as.character(dt2$SiteDisturbance))==trimws("NA")),NA,as.character(dt2$SiteDisturbance)))
dt2$AlternateSite <- as.factor(ifelse((trimws(as.character(dt2$AlternateSite))==trimws("NA")),NA,as.character(dt2$AlternateSite)))
dt2$SeineLength <- ifelse((trimws(as.character(dt2$SeineLength))==trimws("NA")),NA,dt2$SeineLength)
suppressWarnings(dt2$SeineLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SeineLength))==as.character(as.numeric("NA"))),NA,dt2$SeineLength))
dt2$SeineWidth <- ifelse((trimws(as.character(dt2$SeineWidth))==trimws("NA")),NA,dt2$SeineWidth)
suppressWarnings(dt2$SeineWidth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SeineWidth))==as.character(as.numeric("NA"))),NA,dt2$SeineWidth))
dt2$SeineDepth <- ifelse((trimws(as.character(dt2$SeineDepth))==trimws("NA")),NA,dt2$SeineDepth)
suppressWarnings(dt2$SeineDepth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SeineDepth))==as.character(as.numeric("NA"))),NA,dt2$SeineDepth))
dt2$FlowmeterStart <- ifelse((trimws(as.character(dt2$FlowmeterStart))==trimws("NA")),NA,dt2$FlowmeterStart)
suppressWarnings(dt2$FlowmeterStart <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$FlowmeterStart))==as.character(as.numeric("NA"))),NA,dt2$FlowmeterStart))
dt2$FlowmeterEnd <- ifelse((trimws(as.character(dt2$FlowmeterEnd))==trimws("NA")),NA,dt2$FlowmeterEnd)
suppressWarnings(dt2$FlowmeterEnd <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$FlowmeterEnd))==as.character(as.numeric("NA"))),NA,dt2$FlowmeterEnd))
dt2$FlowmeterDifference <- ifelse((trimws(as.character(dt2$FlowmeterDifference))==trimws("NA")),NA,dt2$FlowmeterDifference)
suppressWarnings(dt2$FlowmeterDifference <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$FlowmeterDifference))==as.character(as.numeric("NA"))),NA,dt2$FlowmeterDifference))
dt2$Volume <- ifelse((trimws(as.character(dt2$Volume))==trimws("NA")),NA,dt2$Volume)
suppressWarnings(dt2$Volume <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Volume))==as.character(as.numeric("NA"))),NA,dt2$Volume))
dt2$OrganismCode <- as.factor(ifelse((trimws(as.character(dt2$OrganismCode))==trimws("NA")),NA,as.character(dt2$OrganismCode)))
dt2$IEPFishCode <- as.factor(ifelse((trimws(as.character(dt2$IEPFishCode))==trimws("NA")),NA,as.character(dt2$IEPFishCode)))
dt2$CommonName <- as.factor(ifelse((trimws(as.character(dt2$CommonName))==trimws("NA")),NA,as.character(dt2$CommonName)))
dt2$MarkCode <- as.factor(ifelse((trimws(as.character(dt2$MarkCode))==trimws("NA")),NA,as.character(dt2$MarkCode)))
dt2$StageCode <- as.factor(ifelse((trimws(as.character(dt2$StageCode))==trimws("NA")),NA,as.character(dt2$StageCode)))
dt2$Expression <- as.factor(ifelse((trimws(as.character(dt2$Expression))==trimws("NA")),NA,as.character(dt2$Expression)))
dt2$ForkLength <- ifelse((trimws(as.character(dt2$ForkLength))==trimws("NA")),NA,dt2$ForkLength)
suppressWarnings(dt2$ForkLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ForkLength))==as.character(as.numeric("NA"))),NA,dt2$ForkLength))
dt2$RaceByLength <- as.factor(ifelse((trimws(as.character(dt2$RaceByLength))==trimws("NA")),NA,as.character(dt2$RaceByLength)))
dt2$TagCode <- as.factor(ifelse((trimws(as.character(dt2$TagCode))==trimws("NA")),NA,as.character(dt2$TagCode)))
dt2$RaceByTag <- as.factor(ifelse((trimws(as.character(dt2$RaceByTag))==trimws("NA")),NA,as.character(dt2$RaceByTag)))
dt2$ArchivalID <- as.factor(ifelse((trimws(as.character(dt2$ArchivalID))==trimws("NA")),NA,as.character(dt2$ArchivalID)))
dt2$SpecialStudyID <- as.factor(ifelse((trimws(as.character(dt2$SpecialStudyID))==trimws("NA")),NA,as.character(dt2$SpecialStudyID)))
dt2$GeneticID <- as.factor(ifelse((trimws(as.character(dt2$GeneticID))==trimws("NA")),NA,as.character(dt2$GeneticID)))
dt2$Probability1 <- ifelse((trimws(as.character(dt2$Probability1))==trimws("NA")),NA,dt2$Probability1)
suppressWarnings(dt2$Probability1 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Probability1))==as.character(as.numeric("NA"))),NA,dt2$Probability1))
dt2$GeneticID2 <- as.factor(ifelse((trimws(as.character(dt2$GeneticID2))==trimws("NA")),NA,as.character(dt2$GeneticID2)))
dt2$Probability2 <- ifelse((trimws(as.character(dt2$Probability2))==trimws("NA")),NA,dt2$Probability2)
suppressWarnings(dt2$Probability2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Probability2))==as.character(as.numeric("NA"))),NA,dt2$Probability2))
dt2$SexGeneID <- as.factor(ifelse((trimws(as.character(dt2$SexGeneID))==trimws("NA")),NA,as.character(dt2$SexGeneID)))
dt2$Ots28 <- as.factor(ifelse((trimws(as.character(dt2$Ots28))==trimws("NA")),NA,as.character(dt2$Ots28)))
dt2$Lab <- as.factor(ifelse((trimws(as.character(dt2$Lab))==trimws("NA")),NA,as.character(dt2$Lab)))
dt2$GeneticTest <- as.factor(ifelse((trimws(as.character(dt2$GeneticTest))==trimws("NA")),NA,as.character(dt2$GeneticTest)))
dt2$GeneticModel <- as.factor(ifelse((trimws(as.character(dt2$GeneticModel))==trimws("NA")),NA,as.character(dt2$GeneticModel)))
dt2$Count <- ifelse((trimws(as.character(dt2$Count))==trimws("NA")),NA,dt2$Count)
suppressWarnings(dt2$Count <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Count))==as.character(as.numeric("NA"))),NA,dt2$Count))


# Here is the structure of the input data frame:
str(dt2)
attach(dt2)
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.

detach(dt2)


inUrl3  <- "https://pasta.lternet.edu/package/data/eml/edi/244/11/644fe41db87336bdbe917c528ac4e4cb"
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl"))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")


dt3 <-read.csv(infile3,header=F
               ,skip=1
               ,sep=","
               ,quot='"'
               , col.names=c(
                 "Location",
                 "RegionCode",
                 "StationCode",
                 "SampleDate",
                 "SampleTime",
                 "MethodCode",
                 "GearConditionCode",
                 "WeatherCode",
                 "DO",
                 "WaterTemp",
                 "Turbidity",
                 "Secchi",
                 "SpecificConductance",
                 "TowNumber",
                 "SamplingDirection",
                 "TowDuration",
                 "FlowDebris",
                 "SiteDisturbance",
                 "AlternateSite",
                 "SeineLength",
                 "SeineWidth",
                 "SeineDepth",
                 "FlowmeterStart",
                 "FlowmeterEnd",
                 "FlowmeterDifference",
                 "Volume",
                 "OrganismCode",
                 "IEPFishCode",
                 "CommonName",
                 "MarkCode",
                 "StageCode",
                 "Expression",
                 "ForkLength",
                 "RaceByLength",
                 "TagCode",
                 "RaceByTag",
                 "ArchivalID",
                 "SpecialStudyID",
                 "GeneticID",
                 "Probability1",
                 "GeneticID2",
                 "Probability2",
                 "SexGeneID",
                 "Ots28",
                 "Lab",
                 "GeneticTest",
                 "GeneticModel",
                 "Count"    ), check.names=TRUE)

unlink(infile3)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$Location)!="factor") dt3$Location<- as.factor(dt3$Location)
if (class(dt3$RegionCode)!="factor") dt3$RegionCode<- as.factor(dt3$RegionCode)
if (class(dt3$StationCode)!="factor") dt3$StationCode<- as.factor(dt3$StationCode)
# attempting to convert dt3$SampleDate dateTime string to R date structure (date or POSIXct)
tmpDateFormat<-"%Y-%m-%d"
tmp3SampleDate<-as.Date(dt3$SampleDate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp3SampleDate) == length(tmp3SampleDate[!is.na(tmp3SampleDate)])){dt3$SampleDate <- tmp3SampleDate } else {print("Date conversion failed for dt3$SampleDate. Please inspect the data and do the date conversion yourself.")}
rm(tmpDateFormat,tmp3SampleDate)
if (class(dt3$MethodCode)!="factor") dt3$MethodCode<- as.factor(dt3$MethodCode)
if (class(dt3$GearConditionCode)!="factor") dt3$GearConditionCode<- as.factor(dt3$GearConditionCode)
if (class(dt3$WeatherCode)!="factor") dt3$WeatherCode<- as.factor(dt3$WeatherCode)
if (class(dt3$DO)=="factor") dt3$DO <-as.numeric(levels(dt3$DO))[as.integer(dt3$DO) ]
if (class(dt3$DO)=="character") dt3$DO <-as.numeric(dt3$DO)
if (class(dt3$WaterTemp)=="factor") dt3$WaterTemp <-as.numeric(levels(dt3$WaterTemp))[as.integer(dt3$WaterTemp) ]
if (class(dt3$WaterTemp)=="character") dt3$WaterTemp <-as.numeric(dt3$WaterTemp)
if (class(dt3$Turbidity)=="factor") dt3$Turbidity <-as.numeric(levels(dt3$Turbidity))[as.integer(dt3$Turbidity) ]
if (class(dt3$Turbidity)=="character") dt3$Turbidity <-as.numeric(dt3$Turbidity)
if (class(dt3$Secchi)=="factor") dt3$Secchi <-as.numeric(levels(dt3$Secchi))[as.integer(dt3$Secchi) ]
if (class(dt3$Secchi)=="character") dt3$Secchi <-as.numeric(dt3$Secchi)
if (class(dt3$SpecificConductance)=="factor") dt3$SpecificConductance <-as.numeric(levels(dt3$SpecificConductance))[as.integer(dt3$SpecificConductance) ]
if (class(dt3$SpecificConductance)=="character") dt3$SpecificConductance <-as.numeric(dt3$SpecificConductance)
if (class(dt3$TowNumber)=="factor") dt3$TowNumber <-as.numeric(levels(dt3$TowNumber))[as.integer(dt3$TowNumber) ]
if (class(dt3$TowNumber)=="character") dt3$TowNumber <-as.numeric(dt3$TowNumber)
if (class(dt3$SamplingDirection)!="factor") dt3$SamplingDirection<- as.factor(dt3$SamplingDirection)
if (class(dt3$TowDuration)=="factor") dt3$TowDuration <-as.numeric(levels(dt3$TowDuration))[as.integer(dt3$TowDuration) ]
if (class(dt3$TowDuration)=="character") dt3$TowDuration <-as.numeric(dt3$TowDuration)
if (class(dt3$FlowDebris)!="factor") dt3$FlowDebris<- as.factor(dt3$FlowDebris)
if (class(dt3$SiteDisturbance)!="factor") dt3$SiteDisturbance<- as.factor(dt3$SiteDisturbance)
if (class(dt3$AlternateSite)!="factor") dt3$AlternateSite<- as.factor(dt3$AlternateSite)
if (class(dt3$SeineLength)=="factor") dt3$SeineLength <-as.numeric(levels(dt3$SeineLength))[as.integer(dt3$SeineLength) ]
if (class(dt3$SeineLength)=="character") dt3$SeineLength <-as.numeric(dt3$SeineLength)
if (class(dt3$SeineWidth)=="factor") dt3$SeineWidth <-as.numeric(levels(dt3$SeineWidth))[as.integer(dt3$SeineWidth) ]
if (class(dt3$SeineWidth)=="character") dt3$SeineWidth <-as.numeric(dt3$SeineWidth)
if (class(dt3$SeineDepth)=="factor") dt3$SeineDepth <-as.numeric(levels(dt3$SeineDepth))[as.integer(dt3$SeineDepth) ]
if (class(dt3$SeineDepth)=="character") dt3$SeineDepth <-as.numeric(dt3$SeineDepth)
if (class(dt3$FlowmeterStart)=="factor") dt3$FlowmeterStart <-as.numeric(levels(dt3$FlowmeterStart))[as.integer(dt3$FlowmeterStart) ]
if (class(dt3$FlowmeterStart)=="character") dt3$FlowmeterStart <-as.numeric(dt3$FlowmeterStart)
if (class(dt3$FlowmeterEnd)=="factor") dt3$FlowmeterEnd <-as.numeric(levels(dt3$FlowmeterEnd))[as.integer(dt3$FlowmeterEnd) ]
if (class(dt3$FlowmeterEnd)=="character") dt3$FlowmeterEnd <-as.numeric(dt3$FlowmeterEnd)
if (class(dt3$FlowmeterDifference)=="factor") dt3$FlowmeterDifference <-as.numeric(levels(dt3$FlowmeterDifference))[as.integer(dt3$FlowmeterDifference) ]
if (class(dt3$FlowmeterDifference)=="character") dt3$FlowmeterDifference <-as.numeric(dt3$FlowmeterDifference)
if (class(dt3$Volume)=="factor") dt3$Volume <-as.numeric(levels(dt3$Volume))[as.integer(dt3$Volume) ]
if (class(dt3$Volume)=="character") dt3$Volume <-as.numeric(dt3$Volume)
if (class(dt3$OrganismCode)!="factor") dt3$OrganismCode<- as.factor(dt3$OrganismCode)
if (class(dt3$IEPFishCode)!="factor") dt3$IEPFishCode<- as.factor(dt3$IEPFishCode)
if (class(dt3$CommonName)!="factor") dt3$CommonName<- as.factor(dt3$CommonName)
if (class(dt3$MarkCode)!="factor") dt3$MarkCode<- as.factor(dt3$MarkCode)
if (class(dt3$StageCode)!="factor") dt3$StageCode<- as.factor(dt3$StageCode)
if (class(dt3$Expression)!="factor") dt3$Expression<- as.factor(dt3$Expression)
if (class(dt3$ForkLength)=="factor") dt3$ForkLength <-as.numeric(levels(dt3$ForkLength))[as.integer(dt3$ForkLength) ]
if (class(dt3$ForkLength)=="character") dt3$ForkLength <-as.numeric(dt3$ForkLength)
if (class(dt3$RaceByLength)!="factor") dt3$RaceByLength<- as.factor(dt3$RaceByLength)
if (class(dt3$TagCode)!="factor") dt3$TagCode<- as.factor(dt3$TagCode)
if (class(dt3$RaceByTag)!="factor") dt3$RaceByTag<- as.factor(dt3$RaceByTag)
if (class(dt3$ArchivalID)!="factor") dt3$ArchivalID<- as.factor(dt3$ArchivalID)
if (class(dt3$SpecialStudyID)!="factor") dt3$SpecialStudyID<- as.factor(dt3$SpecialStudyID)
if (class(dt3$GeneticID)!="factor") dt3$GeneticID<- as.factor(dt3$GeneticID)
if (class(dt3$Probability1)=="factor") dt3$Probability1 <-as.numeric(levels(dt3$Probability1))[as.integer(dt3$Probability1) ]
if (class(dt3$Probability1)=="character") dt3$Probability1 <-as.numeric(dt3$Probability1)
if (class(dt3$GeneticID2)!="factor") dt3$GeneticID2<- as.factor(dt3$GeneticID2)
if (class(dt3$Probability2)=="factor") dt3$Probability2 <-as.numeric(levels(dt3$Probability2))[as.integer(dt3$Probability2) ]
if (class(dt3$Probability2)=="character") dt3$Probability2 <-as.numeric(dt3$Probability2)
if (class(dt3$SexGeneID)!="factor") dt3$SexGeneID<- as.factor(dt3$SexGeneID)
if (class(dt3$Ots28)!="factor") dt3$Ots28<- as.factor(dt3$Ots28)
if (class(dt3$Lab)!="factor") dt3$Lab<- as.factor(dt3$Lab)
if (class(dt3$GeneticTest)!="factor") dt3$GeneticTest<- as.factor(dt3$GeneticTest)
if (class(dt3$GeneticModel)!="factor") dt3$GeneticModel<- as.factor(dt3$GeneticModel)
if (class(dt3$Count)=="factor") dt3$Count <-as.numeric(levels(dt3$Count))[as.integer(dt3$Count) ]
if (class(dt3$Count)=="character") dt3$Count <-as.numeric(dt3$Count)

# Convert Missing Values to NA for non-dates

dt3$Location <- as.factor(ifelse((trimws(as.character(dt3$Location))==trimws("NA")),NA,as.character(dt3$Location)))
dt3$RegionCode <- as.factor(ifelse((trimws(as.character(dt3$RegionCode))==trimws("NA")),NA,as.character(dt3$RegionCode)))
dt3$StationCode <- as.factor(ifelse((trimws(as.character(dt3$StationCode))==trimws("NA")),NA,as.character(dt3$StationCode)))
dt3$MethodCode <- as.factor(ifelse((trimws(as.character(dt3$MethodCode))==trimws("NA")),NA,as.character(dt3$MethodCode)))
dt3$GearConditionCode <- as.factor(ifelse((trimws(as.character(dt3$GearConditionCode))==trimws("NA")),NA,as.character(dt3$GearConditionCode)))
dt3$WeatherCode <- as.factor(ifelse((trimws(as.character(dt3$WeatherCode))==trimws("NA")),NA,as.character(dt3$WeatherCode)))
dt3$DO <- ifelse((trimws(as.character(dt3$DO))==trimws("NA")),NA,dt3$DO)
suppressWarnings(dt3$DO <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$DO))==as.character(as.numeric("NA"))),NA,dt3$DO))
dt3$WaterTemp <- ifelse((trimws(as.character(dt3$WaterTemp))==trimws("NA")),NA,dt3$WaterTemp)
suppressWarnings(dt3$WaterTemp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$WaterTemp))==as.character(as.numeric("NA"))),NA,dt3$WaterTemp))
dt3$Turbidity <- ifelse((trimws(as.character(dt3$Turbidity))==trimws("NA")),NA,dt3$Turbidity)
suppressWarnings(dt3$Turbidity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Turbidity))==as.character(as.numeric("NA"))),NA,dt3$Turbidity))
dt3$Secchi <- ifelse((trimws(as.character(dt3$Secchi))==trimws("NA")),NA,dt3$Secchi)
suppressWarnings(dt3$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Secchi))==as.character(as.numeric("NA"))),NA,dt3$Secchi))
dt3$SpecificConductance <- ifelse((trimws(as.character(dt3$SpecificConductance))==trimws("NA")),NA,dt3$SpecificConductance)
suppressWarnings(dt3$SpecificConductance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$SpecificConductance))==as.character(as.numeric("NA"))),NA,dt3$SpecificConductance))
dt3$TowNumber <- ifelse((trimws(as.character(dt3$TowNumber))==trimws("NA")),NA,dt3$TowNumber)
suppressWarnings(dt3$TowNumber <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$TowNumber))==as.character(as.numeric("NA"))),NA,dt3$TowNumber))
dt3$SamplingDirection <- as.factor(ifelse((trimws(as.character(dt3$SamplingDirection))==trimws("NA")),NA,as.character(dt3$SamplingDirection)))
dt3$TowDuration <- ifelse((trimws(as.character(dt3$TowDuration))==trimws("NA")),NA,dt3$TowDuration)
suppressWarnings(dt3$TowDuration <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$TowDuration))==as.character(as.numeric("NA"))),NA,dt3$TowDuration))
dt3$FlowDebris <- as.factor(ifelse((trimws(as.character(dt3$FlowDebris))==trimws("NA")),NA,as.character(dt3$FlowDebris)))
dt3$SiteDisturbance <- as.factor(ifelse((trimws(as.character(dt3$SiteDisturbance))==trimws("NA")),NA,as.character(dt3$SiteDisturbance)))
dt3$AlternateSite <- as.factor(ifelse((trimws(as.character(dt3$AlternateSite))==trimws("NA")),NA,as.character(dt3$AlternateSite)))
dt3$SeineLength <- ifelse((trimws(as.character(dt3$SeineLength))==trimws("NA")),NA,dt3$SeineLength)
suppressWarnings(dt3$SeineLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$SeineLength))==as.character(as.numeric("NA"))),NA,dt3$SeineLength))
dt3$SeineWidth <- ifelse((trimws(as.character(dt3$SeineWidth))==trimws("NA")),NA,dt3$SeineWidth)
suppressWarnings(dt3$SeineWidth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$SeineWidth))==as.character(as.numeric("NA"))),NA,dt3$SeineWidth))
dt3$SeineDepth <- ifelse((trimws(as.character(dt3$SeineDepth))==trimws("NA")),NA,dt3$SeineDepth)
suppressWarnings(dt3$SeineDepth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$SeineDepth))==as.character(as.numeric("NA"))),NA,dt3$SeineDepth))
dt3$FlowmeterStart <- ifelse((trimws(as.character(dt3$FlowmeterStart))==trimws("NA")),NA,dt3$FlowmeterStart)
suppressWarnings(dt3$FlowmeterStart <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$FlowmeterStart))==as.character(as.numeric("NA"))),NA,dt3$FlowmeterStart))
dt3$FlowmeterEnd <- ifelse((trimws(as.character(dt3$FlowmeterEnd))==trimws("NA")),NA,dt3$FlowmeterEnd)
suppressWarnings(dt3$FlowmeterEnd <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$FlowmeterEnd))==as.character(as.numeric("NA"))),NA,dt3$FlowmeterEnd))
dt3$FlowmeterDifference <- ifelse((trimws(as.character(dt3$FlowmeterDifference))==trimws("NA")),NA,dt3$FlowmeterDifference)
suppressWarnings(dt3$FlowmeterDifference <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$FlowmeterDifference))==as.character(as.numeric("NA"))),NA,dt3$FlowmeterDifference))
dt3$Volume <- ifelse((trimws(as.character(dt3$Volume))==trimws("NA")),NA,dt3$Volume)
suppressWarnings(dt3$Volume <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Volume))==as.character(as.numeric("NA"))),NA,dt3$Volume))
dt3$OrganismCode <- as.factor(ifelse((trimws(as.character(dt3$OrganismCode))==trimws("NA")),NA,as.character(dt3$OrganismCode)))
dt3$IEPFishCode <- as.factor(ifelse((trimws(as.character(dt3$IEPFishCode))==trimws("NA")),NA,as.character(dt3$IEPFishCode)))
dt3$CommonName <- as.factor(ifelse((trimws(as.character(dt3$CommonName))==trimws("NA")),NA,as.character(dt3$CommonName)))
dt3$MarkCode <- as.factor(ifelse((trimws(as.character(dt3$MarkCode))==trimws("NA")),NA,as.character(dt3$MarkCode)))
dt3$StageCode <- as.factor(ifelse((trimws(as.character(dt3$StageCode))==trimws("NA")),NA,as.character(dt3$StageCode)))
dt3$Expression <- as.factor(ifelse((trimws(as.character(dt3$Expression))==trimws("NA")),NA,as.character(dt3$Expression)))
dt3$ForkLength <- ifelse((trimws(as.character(dt3$ForkLength))==trimws("NA")),NA,dt3$ForkLength)
suppressWarnings(dt3$ForkLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$ForkLength))==as.character(as.numeric("NA"))),NA,dt3$ForkLength))
dt3$RaceByLength <- as.factor(ifelse((trimws(as.character(dt3$RaceByLength))==trimws("NA")),NA,as.character(dt3$RaceByLength)))
dt3$TagCode <- as.factor(ifelse((trimws(as.character(dt3$TagCode))==trimws("NA")),NA,as.character(dt3$TagCode)))
dt3$RaceByTag <- as.factor(ifelse((trimws(as.character(dt3$RaceByTag))==trimws("NA")),NA,as.character(dt3$RaceByTag)))
dt3$ArchivalID <- as.factor(ifelse((trimws(as.character(dt3$ArchivalID))==trimws("NA")),NA,as.character(dt3$ArchivalID)))
dt3$SpecialStudyID <- as.factor(ifelse((trimws(as.character(dt3$SpecialStudyID))==trimws("NA")),NA,as.character(dt3$SpecialStudyID)))
dt3$GeneticID <- as.factor(ifelse((trimws(as.character(dt3$GeneticID))==trimws("NA")),NA,as.character(dt3$GeneticID)))
dt3$Probability1 <- ifelse((trimws(as.character(dt3$Probability1))==trimws("NA")),NA,dt3$Probability1)
suppressWarnings(dt3$Probability1 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Probability1))==as.character(as.numeric("NA"))),NA,dt3$Probability1))
dt3$GeneticID2 <- as.factor(ifelse((trimws(as.character(dt3$GeneticID2))==trimws("NA")),NA,as.character(dt3$GeneticID2)))
dt3$Probability2 <- ifelse((trimws(as.character(dt3$Probability2))==trimws("NA")),NA,dt3$Probability2)
suppressWarnings(dt3$Probability2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Probability2))==as.character(as.numeric("NA"))),NA,dt3$Probability2))
dt3$SexGeneID <- as.factor(ifelse((trimws(as.character(dt3$SexGeneID))==trimws("NA")),NA,as.character(dt3$SexGeneID)))
dt3$Ots28 <- as.factor(ifelse((trimws(as.character(dt3$Ots28))==trimws("NA")),NA,as.character(dt3$Ots28)))
dt3$Lab <- as.factor(ifelse((trimws(as.character(dt3$Lab))==trimws("NA")),NA,as.character(dt3$Lab)))
dt3$GeneticTest <- as.factor(ifelse((trimws(as.character(dt3$GeneticTest))==trimws("NA")),NA,as.character(dt3$GeneticTest)))
dt3$GeneticModel <- as.factor(ifelse((trimws(as.character(dt3$GeneticModel))==trimws("NA")),NA,as.character(dt3$GeneticModel)))
dt3$Count <- ifelse((trimws(as.character(dt3$Count))==trimws("NA")),NA,dt3$Count)
suppressWarnings(dt3$Count <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Count))==as.character(as.numeric("NA"))),NA,dt3$Count))


# Here is the structure of the input data frame:
str(dt3)
attach(dt3)
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.

detach(dt3)


inUrl4  <- "https://pasta.lternet.edu/package/data/eml/edi/244/11/17c9974d9b7b0125c146a887f3c64bd8"
infile4 <- tempfile()
try(download.file(inUrl4,infile4,method="curl"))
if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")


dt4 <-read.csv(infile4,header=F
               ,skip=1
               ,sep=","
               ,quot='"'
               , col.names=c(
                 "OrganismCode",
                 "IEPFishCode",
                 "CommonName",
                 "NonNative",
                 "Phylum",
                 "Class",
                 "Order",
                 "Family",
                 "Genus",
                 "Species",
                 "Active"    ), check.names=TRUE)

unlink(infile4)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt4$OrganismCode)!="factor") dt4$OrganismCode<- as.factor(dt4$OrganismCode)
if (class(dt4$IEPFishCode)!="factor") dt4$IEPFishCode<- as.factor(dt4$IEPFishCode)
if (class(dt4$CommonName)!="factor") dt4$CommonName<- as.factor(dt4$CommonName)
if (class(dt4$NonNative)!="factor") dt4$NonNative<- as.factor(dt4$NonNative)
if (class(dt4$Phylum)!="factor") dt4$Phylum<- as.factor(dt4$Phylum)
if (class(dt4$Class)!="factor") dt4$Class<- as.factor(dt4$Class)
if (class(dt4$Order)!="factor") dt4$Order<- as.factor(dt4$Order)
if (class(dt4$Family)!="factor") dt4$Family<- as.factor(dt4$Family)
if (class(dt4$Genus)!="factor") dt4$Genus<- as.factor(dt4$Genus)
if (class(dt4$Species)!="factor") dt4$Species<- as.factor(dt4$Species)
if (class(dt4$Active)!="factor") dt4$Active<- as.factor(dt4$Active)

# Convert Missing Values to NA for non-dates

dt4$IEPFishCode <- as.factor(ifelse((trimws(as.character(dt4$IEPFishCode))==trimws("NA")),NA,as.character(dt4$IEPFishCode)))
dt4$Phylum <- as.factor(ifelse((trimws(as.character(dt4$Phylum))==trimws("NA")),NA,as.character(dt4$Phylum)))
dt4$Class <- as.factor(ifelse((trimws(as.character(dt4$Class))==trimws("NA")),NA,as.character(dt4$Class)))
dt4$Order <- as.factor(ifelse((trimws(as.character(dt4$Order))==trimws("NA")),NA,as.character(dt4$Order)))
dt4$Family <- as.factor(ifelse((trimws(as.character(dt4$Family))==trimws("NA")),NA,as.character(dt4$Family)))
dt4$Genus <- as.factor(ifelse((trimws(as.character(dt4$Genus))==trimws("NA")),NA,as.character(dt4$Genus)))
dt4$Species <- as.factor(ifelse((trimws(as.character(dt4$Species))==trimws("NA")),NA,as.character(dt4$Species)))


# Here is the structure of the input data frame:
str(dt4)
attach(dt4)
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.


detach(dt4)


inUrl5  <- "https://pasta.lternet.edu/package/data/eml/edi/244/11/99a038d691f27cd306ff93fdcbc03b77"
infile5 <- tempfile()
try(download.file(inUrl5,infile5,method="curl"))
if (is.na(file.size(infile5))) download.file(inUrl5,infile5,method="auto")


dt5 <-read.csv(infile5,header=F
               ,skip=1
               ,sep=","
               ,quot='"'
               , col.names=c(
                 "MethodCode",
                 "Location",
                 "StationCode",
                 "Latitude",
                 "Longitude"    ), check.names=TRUE)

unlink(infile5)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt5$MethodCode)!="factor") dt5$MethodCode<- as.factor(dt5$MethodCode)
if (class(dt5$Location)!="factor") dt5$Location<- as.factor(dt5$Location)
if (class(dt5$StationCode)!="factor") dt5$StationCode<- as.factor(dt5$StationCode)
if (class(dt5$Latitude)=="factor") dt5$Latitude <-as.numeric(levels(dt5$Latitude))[as.integer(dt5$Latitude) ]
if (class(dt5$Latitude)=="character") dt5$Latitude <-as.numeric(dt5$Latitude)
if (class(dt5$Longitude)=="factor") dt5$Longitude <-as.numeric(levels(dt5$Longitude))[as.integer(dt5$Longitude) ]
if (class(dt5$Longitude)=="character") dt5$Longitude <-as.numeric(dt5$Longitude)

# Convert Missing Values to NA for non-dates

dt5$MethodCode <- as.factor(ifelse((trimws(as.character(dt5$MethodCode))==trimws("NA")),NA,as.character(dt5$MethodCode)))
dt5$Location <- as.factor(ifelse((trimws(as.character(dt5$Location))==trimws("NA")),NA,as.character(dt5$Location)))
dt5$StationCode <- as.factor(ifelse((trimws(as.character(dt5$StationCode))==trimws("NA")),NA,as.character(dt5$StationCode)))
dt5$Latitude <- ifelse((trimws(as.character(dt5$Latitude))==trimws("NA")),NA,dt5$Latitude)
suppressWarnings(dt5$Latitude <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt5$Latitude))==as.character(as.numeric("NA"))),NA,dt5$Latitude))
dt5$Longitude <- ifelse((trimws(as.character(dt5$Longitude))==trimws("NA")),NA,dt5$Longitude)
suppressWarnings(dt5$Longitude <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt5$Longitude))==as.character(as.numeric("NA"))),NA,dt5$Longitude))

# Here is the structure of the input data frame:
str(dt5)
attach(dt5)
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.

# Get more details on character variables


detach(dt5)

DJFMP_station<-dt5%>%
  select(StationCode,Latitude,Longitude)%>%
  rename(Station=StationCode)

DJFMP<-bind_rows(dt1%>%select(StationCode,SampleDate,SampleTime,
                              TowNumber,MethodCode,GearConditionCode,FlowDebris,
                              SpecificConductance,WaterTemp, Secchi, SeineDepth,
                              Volume, SamplingDirection, MarkCode, RaceByLength,
                              OrganismCode,ForkLength,Count),
                 dt2%>%select(StationCode,SampleDate,SampleTime,
                              TowNumber,MethodCode,GearConditionCode,FlowDebris,
                              SpecificConductance,WaterTemp, Secchi, SeineDepth,
                              Volume, SamplingDirection, MarkCode, RaceByLength,
                              OrganismCode,ForkLength,Count),
                 dt3%>%select(StationCode,SampleDate,SampleTime,
                              TowNumber,MethodCode,GearConditionCode,FlowDebris,
                              SpecificConductance,WaterTemp, Secchi, SeineDepth,
                              Volume, SamplingDirection, MarkCode, RaceByLength,
                              OrganismCode,ForkLength,Count))%>%
  rename(Station = StationCode, Date = SampleDate, Time = SampleTime, Temp_surf = WaterTemp,
         Method = MethodCode, Tow_volume = Volume, Depth=SeineDepth,
         Tow_direction = SamplingDirection, Length = ForkLength,Conductivity=SpecificConductance) %>%
  dplyr::filter(is.na(GearConditionCode) | !GearConditionCode%in%c(3,4,9))%>%
  mutate(Tow_volume = if_else(FlowDebris=="Y", NA_real_, Tow_volume, missing=Tow_volume),
         Secchi = Secchi*100, # convert Secchi to cm
         Source = "DJFMP",
         Date = parse_date_time(Date, "%Y-%m-%d", tz = "America/Los_Angeles"),
         Time = parse_date_time(Time, "%H:%M:%S", tz = "America/Los_Angeles"),
         Datetime = parse_date_time(ifelse(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
         # Removing conductivity data from dates before it was standardized
         Conductivity = ifelse(Date<parse_date_time("2019-06-01", "%Y-%m-%d", tz="America/Los_Angeles"), NA_real_, Conductivity),
         Sal_surf = ec2pss(Conductivity/1000, t=25),
         Method = recode(Method, MWTR="Midwater trawl", KDTR="Kodiak trawl", SEIN="Beach seine"),
         Tow_direction = recode(Tow_direction, U="Upstream", D="Downstream", X="Neither"),
         SampleID = paste(Datetime, Station, TowNumber, Method),
         MarkCode = ifelse(OrganismCode=="NOFISH", "None", MarkCode),
         # Set up code for sub-groups to apply plus counts. Untagged Chinoook Salmon are grouped by RaceByLength and any tagged fish are not incorporated into the process
         Group = case_when(MarkCode=="None" & OrganismCode=="CHN" ~ RaceByLength,
                           MarkCode!="None" ~ paste("Tag", 1:nrow(.)),
                           TRUE ~ NA_character_))

select(-Time, -MarkCode, -RaceByLength, -GearConditionCode, -FlowDebris) %>%
  group_by(across(-Count))%>% # Some species are recorded with the same length multiple times
  summarise(Count=sum(Count), .groups="drop")%>%
  group_by(SampleID, OrganismCode, Group)%>%
  mutate(TotalMeasured=sum(Count[which(Length!=0)]), # Calculate total number of fish of each species measured
         Total=sum(Count), # Calculate total number of fish of each species caught
         Count=(Count/TotalMeasured)*Total)%>% # Calculate the adjusted length frequency
  ungroup()%>%
  mutate(Length=ifelse(is.infinite(Count) & Length==0, NA_real_, Length), # Some Chinook were not measured, so these lines fix some after-effects of that
         Length_NA_flag=case_when(
           is.infinite(Count) ~ "Unknown length",
           is.na(Length)~ "No fish caught",
           TRUE ~ NA_character_), # Add reasoning for an NA lengths (all "No Fish Caught" for FMWT)
         Count=ifelse(is.infinite(Count), Total, Count))%>%
  dplyr::filter(Length!=0 | is.na(Length))%>%
  select(-Total, -TotalMeasured, -Group)%>%
  left_join(DJFMP_stations, by = "Station") %>%
  # Add species names
  left_join(Species %>%
              select(USFWS_Code, Taxa) %>%
              dplyr::filter(!is.na(USFWS_Code)),
            by=c("OrganismCode"="USFWS_Code")) %>%
  mutate(SampleID=paste(Source, SampleID), # Add variable for unique (across all studies) sampleID
         Taxa=str_remove(Taxa, " \\((.*)"))%>% # Remove life stage info from Taxa names
  select(-OrganismCode)%>%
  group_by(across(-Count))%>% # Add up any new multiples after removing Group
  summarise(Count=sum(Count), .groups="drop")%>%
  mutate(Count=ifelse(Length_NA_flag=="No fish caught", 0, Count, missing=Count))%>% # Transform all counts for 'No fish caught' to 0.
  select(Source, Station, Latitude, Longitude, Date, Datetime, Depth, SampleID, Method, Sal_surf,
         Temp_surf, Secchi, Tow_volume, Tow_direction, Taxa, Length, Count, Length_NA_flag)


usethis::use_data(DJFMP, overwrite=TRUE, compress="xz")
