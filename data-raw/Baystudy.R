## code to prepare `Baystudy` dataset goes here

require(readr)
require(wql)
require(dplyr)
require(tidyr)
require(lubridate)
require(readxl)
require(LTMRdata)
require(stringr)

# Station locations -------------------------------------------------------

stations_baystudy <- read_excel(file.path("data-raw", "Baystudy", "Bay Study_Station Coordinates for Distribution_04May2020.xlsx"))%>%
  separate(Latitude, into=c("Lat_Deg", "Lat_Min"), sep = "°", convert=T)%>% # Convert to decimal degrees
  separate(Longitude, into=c("Lon_Deg", "Lon_Min"), sep = "°", convert=T)%>%
  mutate(Latitude=Lat_Deg+Lat_Min/60,
         Longitude=Lon_Deg-Lon_Min/60)%>%
  dplyr::select(Station, Latitude, Longitude)%>%
  dplyr::filter(Station!="211E")%>% # Kathy said W location of station 211 is more often used, so using those coordinates
  mutate(Station=recode(Station, `211W`="211"))%>%
  mutate(Station=as.integer(Station))



# Import lookup tables ----------------------------------------------------
Path<-file.path(tempdir(), "BayStudy_AccessDatabase_1980-2021.zip")
Path_origin<-file.path(tempdir())
#Downloading MWT_data.zip----
download.file("https://filelib.wildlife.ca.gov/Public/BayStudy/Access_Database/BayStudy_AccessDatabase_1980-2021.zip", Path, mode="wb",method="libcurl")
unzip(Path,files="CDFW_SFBayStudy_FishData_Sept2022_Public.accdb",exdir=Path_origin)


driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"

# File path to Access database (Salvage)
db_path <- file.path(tempdir(),"CDFW_SFBayStudy_FishData_Sept2022_Public.accdb")

connectAccess <- function(file,
                          driver = "Microsoft Access Driver (*.mdb, *.accdb)", uid = "", pwd = "", ...) {

  file <- normalizePath(file, winslash = "\\")

  # Driver and path required to connect from RStudio to Access
  dbString <- paste0("Driver={", driver,
                     "};Dbq=", file,
                     ";Uid=", uid,
                     ";Pwd=", pwd,
                     ";")

  tryCatch(DBI::dbConnect(drv = odbc::odbc(), .connection_string = dbString),
           error = function(cond) {
             if (all(stringr::str_detect(cond$message, c("IM002", "ODBC Driver Manager")))) {
               message(cond, "\n")
               message("IM002 and ODBC Driver Manager error generally means a 32-bit R needs to be installed or used.")
             } else {
               message(cond)
             }
           })
  # RODBC::odbcDriverConnect(con, ...)
}
Conn<-connectAccess(file=db_path)



# Just pastes driver info and file path together.
full_path <- paste0(driver,"DBQ=",db_path)
full_path
# Connect to database using information above.
conn <- odbcDriverConnect(full_path)

names<-sqlTables(conn,tableType = c("TABLE","VIEW"))["TABLE_NAME"]



extractTables <- function(con, tables, out) {

  # Pulling just the table names
  # tableNames <- RODBC::sqlTables(con, tableType = c("TABLE", "VIEW"))["TABLE_NAME"]
  tableNames <- odbc::dbListTables(conn = con)

  # Includes system tables which cannot be read, excluding them below with negate
  # tableNames <- stringr::str_subset(tableNames, "MSys", negate = T)
  if (length(tables) == 1 & all(tables %in% "check")) {
    # If no table names are specified, then simply return the names of the possible databases for the user to pic

    # RODBC::odbcClose(con)
    DBI::dbDisconnect(con)

    cat("Specify at least one table to pull from: \n")

    return(print(tableNames))
  }

  # Apply the dbReadTable to each readable table in db
  # returnedTables <- mapply(RODBC::sqlQuery,
  #                          query = paste("dplyr::select * FROM", tables),
  #                          MoreArgs = list(channel = con),
  #                          SIMPLIFY = F)
  returnedTables <- mapply(DBI::dbReadTable,
                           name = tables,
                           MoreArgs = list(conn = con),
                           SIMPLIFY = F)

  # names(returnedTables) <- tables

  DBI::dbDisconnect(con)
  # RODBC::odbcClose(con)

  if (length(tables) != 1 & all(tables %in% "check")) {
    # Save the table to be read back into R
    saveRDS(returnedTables, file = file.path(out, "savedAccessTables.rds"))
  } else {
    returnedTables
  }
}
BayStudyTables<-extractTables(con=Conn,tables=c("TideCodes_LookUp","WaveCodes_LookUp","CloudCover_LookUp",
                                                "SalinTemp","BoatStation","BoatTow",
                                                "Fish Catch Data","Fish Length Data"),out=Path_origin)
# Environ data ----
tidecodes_baystudy <- BayStudyTables$TideCodes_LookUp%>%select(Tide,Description)

wavecodes_baystudy <- BayStudyTables$WaveCodes_LookUp%>%select(Waves,Description)

cloudcovercodes_baystudy <- BayStudyTables$CloudCover_LookUp%>%select(CloudCover,Description)



# Sample-level data -------------------------------------------------------


salintemp_baystudy<-BayStudyTables$SalinTemp%>%select(Year,Survey,Station,
                                                      ECSurf,ECAvg,ECBott,
                                                      TempSurf,TempAvg,TempBott)

# Station-visit-level data -----

boatstation_baystudy <- BayStudyTables$BoatStation%>%select(Year, Survey, Station,
                                                            Date, Depth, Secchi,
                                                            SubstrateCode, Waves, CloudCover,
                                                            Tide, StationComment)%>%
  #mutate(Date=parse_date_time(Date, orders="%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
  left_join(tidecodes_baystudy, by="Tide")%>% # Convert tide codes to values
  dplyr::select(-Tide, -SubstrateCode)%>%
  rename(Tidestation=Description)%>%
  left_join(wavecodes_baystudy, by="Waves")%>% # Convert wave codes to values
  dplyr::select(-Waves)%>%
  rename(Waves=Description)%>%
  left_join(cloudcovercodes_baystudy, by="CloudCover")%>% # Convert cloud cover codes to values
  dplyr::select(-CloudCover)%>%
  rename(CloudCover=Description)


# Tow-level data -----

boattow_baystudy<-BayStudyTables$BoatTow%>%select(Year, Survey,Station, Net,
                                                  Tow, Time, Bearing, Tide,
                                                  Direction, CatchCode, Duration,
                                                  StartMeter, EndMeter, TotalMeter,
                                                  StartLong, EndLong, StartLat,
                                                  EndLat, Distance, TowComment)%>%
  dplyr::select(-StartLong, -EndLong, -StartLat, -EndLat, -StartMeter, -EndMeter)%>% # Removing survey lats/longs and start/end meters for now
  mutate(Tow_direction=recode(Direction, `1`="With current", `2`="Against current", `3`="Slack or cross-current"))%>% # Convert tow direction codes to values
  left_join(tidecodes_baystudy, by="Tide")%>% # Convert tide codes to values
  dplyr::select(-Tide, -Direction)%>%
  rename(Method=Net, Tidetow=Description)%>%
  mutate(Method=recode(Method, `1`="Midwater trawl", `2`="Otter trawl", `3`="EL"))%>% # Convert method codes to values
  mutate(Tow_volume=if_else(Method=="Midwater trawl", TotalMeter * 0.02687 * 10.7, NA_real_),# Calculate tow volume using formulas in database metadata
         Tow_area=if_else(Method=="Otter trawl", Distance*1852*3.42, NA_real_))%>% # Calculate tow area using formulas in database metadata, after first converting nautical miles to meters)
  dplyr::filter(Method%in%c("Midwater trawl", "Otter trawl"))%>% # Only include midwater and otter trawls. This currently does not do anything since those are the only two methods in the data.
  mutate(TowStatus=recode(Tow, `0`="Invalid", `1`="Valid", `2`="Valid", `51`="Valid", `52`="Invalid",
                          `53`="Invalid", `54`="Valid", `55`="Valid", `56`="Valid", `57`="Valid", `58`="Invalid"))%>% # Classify TowStatus into Valid or Invalid tows
  dplyr::select(-Tow, -TotalMeter, -Distance) # Remove unneeded variables

# All sample-level data -----

env_baystudy <- left_join(boattow_baystudy, boatstation_baystudy, by=c("Year", "Survey", "Station"))%>% # Join together station-visit and tow - level data
  mutate(Tide=if_else(is.na(Tidetow), Tidestation, Tidetow), # Tide was sometimes recorded at each station visit and sometimes at each tow
         Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"),
         SampleID=1:nrow(.))%>% # Create identifier for each sample (tow)
  left_join(stations_baystudy, by="Station")%>% # Add station locations
  left_join(salintemp_baystudy, by=c("Year", "Survey", "Station"))%>% #Add salinity and temperature data
  dplyr::select(-Tidestation, -Tidetow, -Time) # Remove unneeded variables

rm(tidecodes_baystudy, wavecodes_baystudy, cloudcovercodes_baystudy, boattow_baystudy, boatstation_baystudy, salintemp_baystudy, stations_baystudy) # Clean up


# Catch data --------------------------------------------------------------


catch_baystudy <- BayStudyTables$`Fish Catch Data`%>%select(Year, Survey, Station,
                                                            Net, AlphaCode,
                                                            SizeGroup, QtsCaught, QtsSubsampled,
                                                            PlusCount)%>%
  rename(Method=Net)%>%
  mutate(Method=recode(Method, `1`="Midwater trawl", `2`="Otter trawl", `3`="EL"))%>% # Convert method codes to values
  dplyr::filter(Method%in%c("Midwater trawl", "Otter trawl"))%>% #Only keep midwater and otter trawls
  left_join(Species%>% # Add species names
              dplyr::select(AlphaCode=Baystudy_Code, Taxa)%>%
              dplyr::filter(!is.na(AlphaCode)),
            by="AlphaCode")%>%
  dplyr::select(-AlphaCode) # Remove unneeded variable


# Length data -------------------------------------------------------------


length_baystudy <- BayStudyTables$`Fish Length Data`%>%select(Year, Survey, Station,
                                                              Net, AlphaCode,
                                                              SizeGroup, Length, Frequency)%>%
  rename(Method=Net)%>%
  mutate(Method=recode(Method, `1`="Midwater trawl", `2`="Otter trawl", `3`="EL"), # Convert method codes to values
         AlphaCode = toupper(AlphaCode))%>% # Make alphacodes consistent
  dplyr::filter(Method%in%c("Midwater trawl", "Otter trawl"))%>% #Only keep midwater and otter trawls
  left_join(Species%>% # Add species names
              dplyr::select(AlphaCode=Baystudy_Code, Taxa)%>%
              dplyr::filter(!is.na(AlphaCode)),
            by="AlphaCode")%>%
  dplyr::select(-AlphaCode) # Remove unneeded variable

# Join length and catch data ----

lengthcatch_baystudy<-catch_baystudy%>%
  full_join(length_baystudy%>% # This first join will just include total number measured
               group_by(Year, Survey, Station, Method, SizeGroup, Taxa)%>%
               summarize(TotalMeasured=sum(Frequency))%>% # Calculate total number of each species of fish measured in each sample
               ungroup(),
             by=c("Year", "Survey", "Station", "Method", "SizeGroup", "Taxa"))%>%
  dplyr::filter(!is.na(TotalMeasured) & !is.na(PlusCount))%>% # Remove any rows corresponding to 0 fish measured and 0 fish counted
  mutate(TotalCatch=(TotalMeasured+PlusCount)*(QtsCaught/QtsSubsampled))%>% # Calculate total number of each species caught in each sample
  dplyr::select(-PlusCount, -QtsCaught, -QtsSubsampled)%>% # Remove unneeded variables
  right_join(length_baystudy, by=c("Year", "Survey", "Station", "Method", "SizeGroup", "Taxa"))%>% # Now join all measurements
  mutate(Count = (Frequency/TotalMeasured)*TotalCatch)%>% # Calculate adjusted size frequency
  dplyr::select(-TotalMeasured, -TotalCatch, -Frequency) # Remove unneeded variables


# Create final datasets ---------------------------------------------------


Baystudy <- env_baystudy%>% # Start with sample-level data to retain samples with no catch (nets empty of fish)
  left_join(lengthcatch_baystudy, by=c("Year", "Survey", "Station", "Method"))%>% # Add length and catch data
  mutate(Sal_surf=ec2pss(ECSurf/1000, t=25), # Calculate salinity from conductivity
         Sal_avg=ec2pss(ECAvg/1000, t=25),
         Sal_bott=ec2pss(ECBott/1000, t=25),
         Source="Bay Study",
         SampleID=paste(Source, SampleID), # Add unique identifier for each sample across all studies
         Length_NA_flag=if_else(is.na(Length), "No fish caught", NA_character_), # Add reasoning for an NA lengths (all "No Fish Caught" for Baystudy)
         Taxa=stringr::str_remove(Taxa, " \\((.*)"))%>% # Remove life stage info from Taxa
  dplyr::select(-ECSurf, -ECAvg, -ECBott, -CatchCode, -SizeGroup, -StationComment)%>% # Remove unneeded variables
  group_by_at(vars(-Count))%>%
  summarise(Count=sum(Count), .groups="drop")%>% # Add up counts when the same species and length were recorded multiple times for a sample
  rename(Notes_tow=TowComment, Tow_status=TowStatus,
         Weather=CloudCover, Temp_surf=TempSurf, Temp_avg=TempAvg,
         Temp_bott=TempBott, Tow_duration=Duration)%>%
  dplyr::select(-Bearing, -Waves, -Weather, -Temp_avg, -Temp_bott, -Sal_avg, -Sal_bott)%>% # Remove extra environmental variables
  dplyr::filter(Tow_status=="Valid")%>% # Remove invalid tows
  mutate(Count=if_else(Length_NA_flag=="No fish caught", 0, Count, missing=Count))%>% # Transform all counts for 'No fish caught' to 0.
  dplyr::select(Source, Station, Latitude, Longitude, Date, Datetime, Year, Survey, # Reorder variables for consistency
         Depth, SampleID, Method, Tide, Sal_surf, Temp_surf, Secchi,
         Tow_duration, Tow_area, Tow_volume, Tow_direction, Taxa, Length, Count, Length_NA_flag, Notes_tow)

# Just measured lengths

Baystudy_measured_lengths<-length_baystudy%>%
  inner_join(Baystudy%>% # Inner join to remove invalid tows
              dplyr::select(SampleID, Year, Survey, Station, Method)%>% # Add SampleID and Method to length data
              distinct(),
            by=c("Year", "Survey", "Station", "Method"))%>%
  mutate(Taxa=stringr::str_remove(Taxa, " \\((.*)"))%>% #Remove life stage from Taxa
  dplyr::select(SampleID, Taxa, Size_group=SizeGroup, Length, Count=Frequency)# Reorder variables for consistency

Baystudy <- Baystudy%>%
  dplyr::select(-Year)# Remove unneeded variables

usethis::use_data(Baystudy, Baystudy_measured_lengths, overwrite = TRUE, compress="xz") # Save compressed data to /data folder
