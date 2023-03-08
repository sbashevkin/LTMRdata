require(wql)
require(dplyr)
require(tidyr)
require(lubridate)
require(LTMRdata)
require(stringr)
require(readr)
require(DBI)
require(RODBC)

options(timeout = 999999)
Path<-file.path(tempdir(), "Salvage_data_FTP.accdb")
Path_origin<-file.path(tempdir())
#Downloading MWT_data.zip----
download.file("https://filelib.wildlife.ca.gov/Public/Salvage/Salvage_data_FTP.accdb", Path, mode="wb",method="libcurl")


# MS access database set up----
# File path to Access database (Salvage)
db_path <- file.path(tempdir(),"Salvage_data_FTP.accdb")

source(file.path("data-raw", "bridgeAccess.R"))

keepTables <- c("Building", "DNAandCWTRace", "LarvalFishLength", "Length",
                "OrganismsLookUp", "Sample", "StationsLookUp", "Catch",
                "StudiesLookUp", "VariableCodesLookUp", "VariablesLookUp")

SalvageTables <- bridgeAccess(db_path,
                            tables = keepTables,
                            script = file.path("data-raw", "connectAccess.R"))


# ----
# setting up the combined table to link with correct ID"s

options(timeout = 9999)
Salvage<-left_join(SalvageTables$Catch,SalvageTables$OrganismsLookUp[c("OrganismCode","CommonName","Genus","Species")],by="OrganismCode")%>%
  left_join(SalvageTables$Length,by="CatchRowID",multiple="all")%>%
  left_join(SalvageTables$Building,by="BuildingRowID")%>%
  left_join(SalvageTables$Sample,by="SampleRowID")%>%
  left_join(SalvageTables$StudiesLookUp,by="StudyRowID")%>%
  dplyr::select(SampleDate,SampleTime,SampleMethod,BuildingCode,
                PrimaryDepth,PrimaryFlow,SampleRowID,BuildingRowID,CatchRowID,
                AcreFeet, MinutesPumping,SampleTimeLength,StudyRowID,WaterTemperature,
                OrganismCode,CommonName,Genus,Species,
                ForkLength,LengthFrequency,AdiposeClip,Count,
                BayPump1,BayPump2,BayPump3,BayPump4,BayPump5)%>%
  #dplyr::rename(SampleDuration=SampleTimeLength)%>%
  mutate(Taxa=paste(Genus,Species,sep=" "))%>%
  dplyr::select(SampleDate,StudyRowID,MinutesPumping,SampleTime,
                PrimaryDepth,PrimaryFlow,WaterTemperature,SampleRowID,
                BuildingRowID,OrganismCode,Count,CommonName,SampleTimeLength,
                BuildingCode,CatchRowID,ForkLength,LengthFrequency,
                BayPump1,BayPump2,BayPump3,BayPump4,BayPump5)%>%
  dplyr::mutate(ExpansionFactor=ifelse(StudyRowID=="0000",as.numeric(MinutesPumping/SampleTimeLength),
                                       ifelse(StudyRowID=="9999",1,as.numeric(NA))),
                ExpandedSalvage=LengthFrequency*ExpansionFactor,
                Year=lubridate::year(SampleDate),
                SampleDate=as.Date(SampleDate),
                #CalDayStart = as.Date(paste(Year,"01-01",sep="-")),
                #CalDay = as.numeric(difftime(SampleDate,CalDayStart,units="days")),
                #WYear= ifelse(SampleDate<as.Date(paste(Year,"10-01",sep="-")),Year,Year+1)
  )

usethis::use_data(Salvage, overwrite=TRUE)
