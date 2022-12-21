require(wql)
require(LTMRdata)
require(readr)
require(dplyr)
require(lubridate)
require(tidyr)
require(stringr)
require(RODBC)

Path<-file.path(tempdir(), "MWT_data.zip")
Path_origin<-file.path(tempdir())
#Downloading MWT_data.zip----
download.file("https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/FMWT%20Data/MWT_data.zip", Path, mode="wb",method="libcurl")
unzip(Path,files="MWT_data.accdb",exdir=Path_origin)

# MS access database set up----
# MS Access database driver.
driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"

# File path to Access database (Salvage)
db_path <- file.path(tempdir(),"MWT_data.accdb")

# Just pastes driver info and file path together.
full_path <- paste0(driver,"DBQ=",db_path)
full_path
# Connect to database using information above.
conn <- odbcDriverConnect(full_path)
names<-sqlTables(conn)

#MWT data setup ----
MWT_Data<-list()
MWT_Data$Catch <- sqlFetch(conn, "Catch")
MWT_Data$Length <- sqlFetch(conn, "Length")
MWT_Data$Sample <- sqlFetch(conn, "Sample")
MWT_Data$StationsLookUp <- sqlFetch(conn, "StationsLookUp")
odbcCloseAll()
rm(names,conn,db_path,full_path,Path,Path_origin)

