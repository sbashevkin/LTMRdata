require(wql)
require(LTMRdata)
require(readr)
require(dplyr)
require(lubridate)
require(tidyr)
require(stringr)
require(RODBC)

Path<-file.path(tempdir(), "20mm_New.zip")
Path_origin<-file.path(tempdir())
#Downloading MWT_data.zip----
download.file("https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/20mm_New.zip", Path, mode="wb",method="libcurl")
unzip(Path,files="20mm_New.accdb",exdir=Path_origin)

# MS access database set up----
# MS Access database driver.
driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"

# File path to Access database (Salvage)
db_path <- file.path(tempdir(),"20mm_New.accdb")

# Just pastes driver info and file path together.
full_path <- paste0(driver,"DBQ=",db_path)
full_path
# Connect to database using information above.
conn <- odbcDriverConnect(full_path)
names<-sqlTables(conn)

#MWT data setup ----
TMM_Data<-list()
TMM_Data$FishLength <- sqlFetch(conn, "FishLength")
TMM_Data$FishSample <- sqlFetch(conn, "FishSample")
TMM_Data$Gear <- sqlFetch(conn, "Gear")
TMM_Data$GearCodesLookup <- sqlFetch(conn, "GearCodesLkp")
TMM_Data$MeterCorrections <- sqlFetch(conn, "MeterCorrections")
TMM_Data$SampleCode <- sqlFetch(conn, "SampleCode")
TMM_Data$Station <- sqlFetch(conn, "Station")
TMM_Data$Survey <- sqlFetch(conn, "Survey")
TMM_Data$Tow <- sqlFetch(conn, "Tow")
TMM_Data$StationsLookup <- sqlFetch(conn, "20mmStations")
odbcCloseAll()
rm(names,conn,db_path,full_path,Path,Path_origin)
