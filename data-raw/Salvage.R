require(wql)
require(dplyr)
require(tidyr)
require(lubridate)
require(LTMRdata)
require(stringr)
require(readr)
require(DBI)
require(RODBC)


# MS Access database driver.
driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"

# File path to Access database (Salvage)
db_path <- paste0(" C:/Users/ETham/Downloads/Salvage_StandaloneAccess_V1.accdb")

# Just pastes driver info and file path together.
full_path <- paste0(driver,"DBQ=",db_path)
full_path
# Connect to database using information above.
conn <- odbcDriverConnect(full_path)

# See a list of all tables in the database. Scroll past the system tables to see actual tables.
names<-sqlTables(conn)

# Compliling table into one large list for organization
SalvageTables<-list()

SalvageTables$Building <- sqlFetch(conn, "Building")%>%dplyr::select(BuildingRowID,SampleRowID,BuildingCode)
SalvageTables$DNAandCWTRace <- sqlFetch(conn, "DNAandCWTRace")
SalvageTables$LarvalFishLength <- sqlFetch(conn, "LarvalFishLength")
SalvageTables$Length <- sqlFetch(conn, "Length")
SalvageTables$OrganismsLookUp <- sqlFetch(conn, "OrganismsLookUp")
SalvageTables$Sample <- sqlFetch(conn, "Sample")%>%dplyr::select(SampleRowID,SampleDate,SampleTime,SampleMethod,StudyRowID,AcreFeet,MinutesPumping,SampleTimeLength,WaterTemperature)
SalvageTables$StationsLookUp <- sqlFetch(conn, "StationsLookUp")%>%dplyr::select(FacilityCode,Comments)
SalvageTables$Catch<-sqlFetch(conn,"Catch")
SalvageTables$StudiesLookUp <- sqlFetch(conn, "StudiesLookUp")
SalvageTables$VariableCodesLookUp <- sqlFetch(conn, "VariableCodesLookUp")
SalvageTables$VariablesLookUp <- sqlFetch(conn, "VariablesLookUp")

# setting up the combined table to link with correct ID"s

options(timeout = 9999)
Salvage<-left_join(SalvageTables$Catch,SalvageTables$OrganismsLookUp[c("OrganismCode","CommonName","Genus","Species")],by="OrganismCode")%>%
  left_join(SalvageTables$Length,by="CatchRowID",multiple="all")%>%
  left_join(SalvageTables$Building,by="BuildingRowID")%>%
  left_join(SalvageTables$Sample,by="SampleRowID")%>%
  left_join(SalvageTables$StudiesLookUp,by="StudyRowID")%>%
  dplyr::select(SampleDate,SampleTime,SampleMethod,BuildingCode,
                AcreFeet, MinutesPumping,SampleTimeLength,StudyRowID,WaterTemperature,
                OrganismCode,CommonName,Genus,Species,
                ForkLength,LengthFrequency,AdiposeClip)%>%
  rename(SampleDuration=SampleTimeLength)%>%
  mutate(Taxa=paste(Genus,Species,sep=" "))%>%
  dplyr::select(SampleDate,SampleTime,SampleMethod,AcreFeet,
                BuildingCode,MinutesPumping,SampleDuration,StudyRowID,WaterTemperature,
                OrganismCode,CommonName,Taxa,
                ForkLength,LengthFrequency,AdiposeClip)%>%
  dplyr::mutate(ExpansionFactor=ifelse(StudyRowID==0,as.numeric(MinutesPumping/SampleDuration),
                                       ifelse(StudyRowID==9999,1,as.numeric(NA))),
                ExpandedSalvage=LengthFrequency*ExpansionFactor,
                Year=lubridate::year(SampleDate),
                SampleDate=as.Date(SampleDate),
                CalDayStart = as.Date(paste(Year,"01-01",sep="-")),
                CalDay = as.numeric(difftime(SampleDate,CalDayStart,units="days")),
                WYear= ifelse(SampleDate<as.Date(paste(Year,"10-01",sep="-")),Year,Year+1)
  )
