
###################################################################
##  code to prepare `SKT` dataset as prepared by Sam Bashevkin   ##
###################################################################

require(wql)
require(LTMRdata)
require(readr)
require(dplyr)
require(lubridate)
require(tidyr)
require(stringr)
require(RODBC) #suggest using ODBC w/ another package (search)

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

# Connect to database using information above.
conn <- odbcDriverConnect(full_path)
names<-sqlTables(conn)

#MWT data setup ----
SKT_Data<-list()

# Station locations -------------------------------------------------------
# read table with station latitude and longitude (one row per station)
SKT_Data$StationsSKT <- sqlFetch(conn, "lktblStationsSKT")%>%
  select(Station,LatDeg,LatMin,LatSec,LongDec,LongMin,LongSec)
{
  SKT_Data$StationsSKT$Station<-as.character(SKT_Data$StationsSKT$Station)
  SKT_Data$StationsSKT$LatMin<-as.numeric(SKT_Data$StationsSKT$LatMin)
  SKT_Data$StationsSKT<-SKT_Data$StationsSKT%>%
    mutate(Latitude=SKT_Data$StationsSKT$LatDeg+SKT_Data$StationsSKT$LatMin/60+SKT_Data$StationsSKT$LatSec/3600,
           Longitude=(SKT_Data$StationsSKT$LongDec+SKT_Data$StationsSKT$LongMin/60+SKT_Data$StationsSKT$LongSec/3600)*-1)%>%
    drop_na()
}
# Sample-level data -------------------------------------------------------
# read sample data (one row per tow)
SKT_Data$Sample <- sqlFetch(conn, "tblSample")%>%
  select(SampleRowID,SampleDate,StationCode,SampleTimeStart,SurveyNumber,
         WaterTemperature,TideCode,DepthBottom,Secchi,ConductivityTop,
         TowDirectionCode,MeterStart,MeterEnd)
{
  SKT_Data$Sample$StationCode<-as.character(SKT_Data$Sample$StationCode)
  SKT_Data$Sample<-SKT_Data$Sample%>%rename(Station = StationCode, Depth = DepthBottom, Temp_surf = WaterTemperature, Survey = SurveyNumber, Date=SampleDate, Time=SampleTimeStart)%>%
    mutate(# Create a new field which is a Date-Time composite
           Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste(Date, paste(hour(Time), minute(Time), sep=":"))),
                                      "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
           # Convert tide codes to values
           Tide = recode(TideCode, `1` = "High Slack", `2` = "Ebb", `3` = "Low Slack", `4` = "Flood", .default = NA_character_),
           # Calculate flowmeter total difference
           Meter_total = MeterEnd - MeterStart,
           Meter_total = ifelse(Meter_total<0, Meter_total + 1000000, Meter_total), # Correct negative metertotals from meter resetting during trawl
           Depth = Depth*0.3048)%>% # Convert feet to meters
    # Calculate tow volume using formula provided by Trishelle Temple
    # Volume = A*K*D (A = 13.95 area of trawl mouth; K = 0.026873027 K factor of flow meter; D = difference in flow readings)
    mutate(Tow_volume = Meter_total*0.026873027*13.95,
           # Convert tow direction codes to values
           Tow_direction = recode(TowDirectionCode, `1` = "With current", `2` = "Against current",
                                  `3` = "Unknown", .default = NA_character_)) %>%
    # Remove unneeded variables
    select(-Meter_total, -TideCode, -TowDirectionCode, -MeterStart, -MeterEnd, -Time) %>%
    # Add station coordinates
    left_join(SKT_Data$StationsSKT, by = "Station")
}
# Catch data --------------------------------------------------------------
# Read Catch data (one row per species per tow)
# Fields: CatchRowID	SampleRowID	OrganismCode	Catch
SKT_Data$Catch <- sqlFetch(conn, "tblCatch")%>%select(CatchRowID,SampleRowID,OrganismCode,Catch)
{
  SKT_Data$Catch$OrganismCode<-as.character(SKT_Data$Catch$OrganismCode)
      # Add species names
  SKT_Data$Catch<-SKT_Data$Catch%>%left_join(Species %>%
                select(SKT_Code, Taxa) %>%
                dplyr::filter(!is.na(SKT_Code)),
              by = c("OrganismCode"="SKT_Code"))
}
# Length data -------------------------------------------------------------
# Read Length data (one row per measured fish per tow)
# Fields: CatchRowID, LengthRowID, ForkLength, ReleasedAlive (flag)
SKT_Data$FishInfo <- sqlFetch(conn, "tblFishInfo")#%>%select(CatchRowID,ForkLength,LengthRowID)
{
  SKT_Data$FishInfo<-SKT_Data$FishInfo%>%
    mutate(LengthFrequency = 1) %>%
    # 0 fork length means not measured, so removing those from
    # length table so those fish can be redistributed among measured lengths
    dplyr::filter(ForkLength != 0)%>%
    group_by(CatchRowID, ForkLength)%>%
    summarise(LengthFrequency=sum(LengthFrequency), .groups="drop")
}

SKT_Data$CatchLength<-SKT_Data$Catch%>%
  left_join(SKT_Data$FishInfo%>%
              group_by(CatchRowID)%>%
              # Calculate total number of fish measured for each species in each sample
              mutate(TotalMeasured = sum(LengthFrequency))%>%
              ungroup(),
            # Add catch numbers and species names
            by = "CatchRowID",multiple="all")%>%
  # Calculate adjusted count
  mutate(Count = ifelse(is.na(TotalMeasured), Catch, (LengthFrequency/TotalMeasured)*Catch))


# close connections ----
odbcCloseAll()
rm(names,conn,db_path,full_path,Path,Path_origin)

# Create final datasets ---------------------------------------------------

# Start with sample to ensure samples without any catch (empty nets) are included
SKT <- SKT_Data$Sample %>%
  # Join to catch/length data
  left_join(SKT_Data$CatchLength%>%
              dplyr::filter(!(is.na(Count) & OrganismCode!=0)), # Remove any cases other than nocatch where Count is NA
            by="SampleRowID",multiple="all") %>%
  # Convert conductivity to salinity
  mutate(Sal_surf = ec2pss(ConductivityTop/1000, t=25),
         # add identifier for survey
         Source = "SKT", Method = "Kodiak trawl",
         # Add variable for unique (across all studies) sampleID
         SampleID = paste(Source, SampleRowID),
         # Add reasoning for an NA lengths (all "No Fish Caught" for FMWT)
         Length_NA_flag = case_when(OrganismCode == 0 ~ "No fish caught",
                                    is.na(ForkLength) & Count > 0 ~ "Unknown length",
                                    TRUE ~ NA_character_),
         Count=if_else(Length_NA_flag=="No fish caught", 0, Count, missing=Count), # Setting Count to 0 for no fish caught, just like the other surveys
         # Remove life stage info from Taxa names
         Taxa = stringr::str_remove(Taxa, " \\((.*)")) %>%
  # Reorder variables for consistency
  dplyr::select(Source, Station, Latitude, Longitude, Date, Datetime, Survey,
         Depth, SampleID, CatchRowID, Method, Tide, Sal_surf, Temp_surf, Secchi,
         Tow_volume, Tow_direction, Taxa, Length = ForkLength, Count, Length_NA_flag)


# Just measured lengths

SKT_measured_lengths<-SKT_Data$FishInfo %>%
  # Join species names and sampleID
  left_join(SKT %>%
              select(CatchRowID, SampleID, Taxa) %>%
              distinct(),
            by = "CatchRowID") %>%
  # Reorder variables for consistency
  select(SampleID, Taxa, Length = ForkLength, Count = LengthFrequency)

# Remove unneeded variable
SKT<-SKT %>%
  select(-CatchRowID)%>%
  distinct()

# Clean up; remove temporary files
rm(SKT_Data)

# Save compressed data to /data
usethis::use_data(SKT, SKT_measured_lengths, overwrite=TRUE, compress="xz")
