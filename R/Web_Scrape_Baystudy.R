require(wql)
require(LTMRdata)
require(readr)
require(dplyr)
require(lubridate)
require(tidyr)
require(stringr)
require(RODBC)

Path<-file.path(tempdir(), "BayStudy_AccessDatabase_1980-2021.zip")
Path_origin<-file.path(tempdir())
#Downloading MWT_data.zip----
download.file("https://filelib.wildlife.ca.gov/Public/BayStudy/Access_Database/BayStudy_AccessDatabase_1980-2021.zip", Path, mode="wb",method="libcurl")
unzip(Path,files="CDFW_SFBayStudy_FishData_Sept2022_Public.accdb",exdir=Path_origin)

# MS access database set up----
# MS Access database driver.
driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"

# File path to Access database (Salvage)
db_path <- file.path(tempdir(),"CDFW_SFBayStudy_FishData_Sept2022_Public.accdb")

# Just pastes driver info and file path together.
full_path <- paste0(driver,"DBQ=",db_path)
full_path
# Connect to database using information above.
conn <- odbcDriverConnect(full_path)
names<-sqlTables(conn)

#MWT data setup ----
Baystudy_Data<-list()
Baystudy_Data$BoatStation <- sqlFetch(conn, "BoatStation")
Baystudy_Data$BoatTow <- sqlFetch(conn, "BoatTow")
Baystudy_Data$CloudCover_LookUp <- sqlFetch(conn, "CloudCover_LookUp")
Baystudy_Data$FishCatch <- sqlFetch(conn, "Fish Catch Data")
Baystudy_Data$FishLength <- sqlFetch(conn, "Fish Length Data")
Baystudy_Data$SalinTemp <- sqlFetch(conn, "SalinTemp")
Baystudy_Data$TideCodes_LookUp <- sqlFetch(conn, "TideCodes_LookUp")
Baystudy_Data$WaveCodes_LookUp <- sqlFetch(conn, "WaveCodes_LookUp")
odbcCloseAll()
rm(names,conn,db_path,full_path,Path,Path_origin)
