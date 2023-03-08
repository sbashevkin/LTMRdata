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
Salvage<-left_join(SalvageTables$Sample,SalvageTables$Building,by="SampleRowID",multiple="all")%>%
  left_join(SalvageTables$Catch,by="BuildingRowID",multiple="all")%>%
  left_join(SalvageTables$Length,by="CatchRowID",multiple="all")%>%
  left_join(SalvageTables$OrganismsLookUp, by="OrganismCode",multiple="all")%>%
  dplyr::select(SampleDate,StudyRowID,MinutesPumping,SampleTimeLength,
                PrimaryDepth,PrimaryFlow,WaterTemperature,SampleRowID,
                BuildingRowID,OrganismCode,Count,CommonName,
                BuildingCode,CatchRowID,ForkLength,LengthFrequency,
                BayPump1,BayPump2,BayPump3,BayPump4,BayPump5)

usethis::use_data(Salvage, overwrite=TRUE)
