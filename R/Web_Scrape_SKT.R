require(wql)
require(LTMRdata)
require(readr)
require(dplyr)
require(lubridate)
require(tidyr)
require(stringr)
require(RODBC)

Path<-file.path(tempdir(), "SKT.zip")
Path_origin<-file.path(tempdir())
#Downloading MWT_data.zip----
download.file("https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SKT.zip", Path, mode="wb",method="libcurl")
unzip(Path,files="SKT.accdb",exdir=Path_origin)

# MS access database set up----
# MS Access database driver.
driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"

# File path to Access database (Salvage)
db_path <- file.path(tempdir(),"Skt.accdb")

# Just pastes driver info and file path together.
full_path <- paste0(driver,"DBQ=",db_path)
full_path
# Connect to database using information above.
conn <- odbcDriverConnect(full_path)
names<-sqlTables(conn)

#MWT data setup ----
SKT_Data<-list()
SKT_Data$Catch <- sqlFetch(conn, "tblCatch")
SKT_Data$FishInfo <- sqlFetch(conn, "tblFishInfo")
SKT_Data$Sample <- sqlFetch(conn, "tblSample")
SKT_Data$StationsSKT <- sqlFetch(conn, "lktblStationsSKT")
odbcCloseAll()
rm(names,conn,db_path,full_path,Path,Path_origin)
