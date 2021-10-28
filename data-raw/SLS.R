library(tidyverse)
library(lubridate)

pullSLS <- function(url = "https://filelib.wildlife.ca.gov/Public/Delta%20Smelt",
                    downloadFiles = F,
                    useAccess = F) {

  if (downloadFiles) {
    # Download the SLS.zip file from the ftp website
    SLSAccess <- paste0(url, "/SLS.zip")

    # Download and unzip the database
    download.file(SLSAccess, method = "curl",
                  destfile = "SLS.zip")
  }

  if (useAccess) {
    # In order to use this correctly, you need to have the 32-bit version of R installed

    localDbFile <- unzip(zipfile = "SLS.zip", exdir = getwd())

    dbString <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
                       "Dbq=",localDbFile)

    con <- DBI::dbConnect(drv=odbc::odbc(), .connection_string=dbString)

    tableNames <- odbc::dbListTables(conn = con) %>%
      str_subset("MSys", negate = T)

    SLSTables <- map(tableNames, ~DBI::dbReadTable(con, .)) %>%
      set_names(tableNames)

    DBI::dbDisconnect(con)
  } else {
    # If not using Access, you will need to supply the base Access files themselves
    # Table you will need are: "Catch", "Lengths", "Mater Corrections",
    # "Tow Info", "Water Info" as of 10/22/2021
    # Download these as flat files; general process I use is:
    # export to text file, delimited (default), delimiter = comma (default),
    # YES Include Field Names on First Row (NOT default), Text Qualifier = " (default)
    # NOTE: doing it this way means read_delim will read in the data slightly differently in terms
    # of format and order of the data
    # However, overall results have not changed; checked Catch table
    filesToRead <- list.files(path=file.path("data-raw", "SLS"), pattern = ".txt")
    SLSTables <- list()

    SLSTables$Catch1 <- read_delim(file.path("data-raw", "SLS", "Catch.txt"), delim = ",",
                                   col_types =
                                     cols(
                                       # Can't exactly get this to be in the same POSIXct as Access
                                       Date = col_character(),
                                       Station = col_integer(),
                                       Tow = col_integer(),
                                       FishCode = col_integer(),
                                       Catch = col_integer(),
                                       `1/4 Subsampled` = col_integer(),
                                       `1/2 Subsampled` = col_integer(),
                                       CatchID = col_integer()
                                     )
    ) %>%
      # Need to rename 1/4 Subsampled and 1/2 Subsampled to the same format as the Access method
      rename(X1.4.Subsampled = `1/4 Subsampled`,
             X1.2.Subsampled = `1/2 Subsampled`)%>%
      mutate(Date=mdy_hms(Date))
    # # In order to make sure that all.equal is the same for the Access-derived and txt file derived tables,
    # # need to make sure that: change SLSTables$Catch to Catch1 above
    # attr(SLSTables$Catch1, "spec") <- NULL
    # attr(SLSTables$Catch1, "problems") <- NULL
    # attributes(SLSTables$Catch1)$class <- "data.frame"
    # all.equal(SLSTables$Catch1 %>%
    #             arrange_all,
    #           SLSTables$Catch %>%
    #             mutate(Date = as.Date(Date)) %>%
    #             arrange_all())
    # # TRUE, good to go

    SLSTables$Lengths1 <- read_delim(file.path("data-raw", "SLS", "Lengths.txt"), delim = ",",
                                     col_types =
                                       cols(
                                         Date = col_character(),
                                         Station = col_integer(),
                                         Tow = col_integer(),
                                         FishCode = col_integer(),
                                         Length = col_integer(),
                                         entryorder = col_integer(),
                                         YolkSacorOilPresent = col_logical()
                                       )
    )%>%
      mutate(Date=mdy_hms(Date))
    # # As seen before, only the attributes should be not equal
    # attr(SLSTables$Lengths1, "spec") <- NULL
    # attr(SLSTables$Lengths1, "problems") <- NULL
    # attributes(SLSTables$Lengths1)$class <- "data.frame"
    # all.equal(SLSTables$Lengths1 %>%
    #             arrange_all,
    #           SLSTables$Lengths %>%
    #             mutate(Date = as.Date(Date)) %>%
    #             arrange_all())
    # # TRUE, good to go

    SLSTables$`Meter Corrections1` <- read_delim(file.path("data-raw", "SLS", "Meter Corrections.txt"), delim = ",",
                                                 col_types =
                                                   cols(
                                                     StudyYear = col_double(),
                                                     MeterSerial = col_integer(),
                                                     CalibrationDate = col_character(),
                                                     kfactor = col_double(),
                                                     Notes = col_character()
                                                   )
    )%>%
      mutate(CalibrationDate=mdy_hms(CalibrationDate))
    # attr(SLSTables$`Meter Corrections1`, "spec") <- NULL
    # attr(SLSTables$`Meter Corrections1`, "problems") <- NULL
    # attributes(SLSTables$`Meter Corrections1`)$class <- "data.frame"
    # all.equal(SLSTables$`Meter Corrections1` %>%
    #             arrange_all(),
    #           SLSTables$`Meter Corrections` %>%
    #             mutate(CalibrationDate = as.Date(CalibrationDate)) %>%
    #             arrange_all())
    # # TRUE, good to go

    SLSTables$`Tow Info1` <- read_delim(file.path("data-raw", "SLS", "Tow Info.txt"), delim = ",",
                                        col_types =
                                          cols(
                                            Date = col_character(),
                                            Station = col_integer(),
                                            Tow = col_integer(),
                                            Time = col_character(),
                                            Tide = col_character(),
                                            BottomDepth = col_integer(),
                                            CableOut = col_integer(),
                                            Duration = col_double(),
                                            NetMeterSerial = col_integer(),
                                            NetMeterStart = col_integer(),
                                            NetMeterEnd = col_integer(),
                                            NetMeterCheck = col_integer(),
                                            CBMeterSerial = col_integer(),
                                            CBMeterStart = col_integer(),
                                            CBMeterEnd = col_integer(),
                                            CBMeterCheck = col_integer(),
                                            Comments = col_character()
                                          )
    ) %>%
      # Just going to change this to UTC here b/c that's how the database from Access is read as
      # This gets changed later to America/Los_Angeles in creating the dataframes for the final table
      mutate(Time=mdy_hms(Time, tz="America/Los_Angeles"))
    # attr(SLSTables$`Tow Info1`, "spec") <- NULL
    # attr(SLSTables$`Tow Info1`, "problems") <- NULL
    # attributes(SLSTables$`Tow Info1`)$class <- "data.frame"
    # all.equal(SLSTables$`Tow Info1` %>%
    #             arrange_all(),
    #           SLSTables$`Tow Info` %>%
    #             mutate(Date = as.Date(Date)) %>%
    #             arrange_all())
    # # TRUE

    SLSTables$`Water Info1` <- read_delim("Water Info.txt", delim = ",",
                                          col_types =
                                            cols(
                                              Survey = col_integer(),
                                              Date = col_character(),
                                              Station = col_integer(),
                                              Temp = col_double(),
                                              TopEC = col_integer(),
                                              BottomEC = col_integer(),
                                              Secchi = col_integer(),
                                              Turbidity = col_integer(),
                                              Lat = col_character(),
                                              Long = col_character(),
                                              Comments = col_character()
                                            )
    )%>%
      mutate(Date=mdy_hms(Date))
    # attr(SLSTables$`Water Info1`, "spec") <- NULL
    # attr(SLSTables$`Water Info1`, "problems") <- NULL
    # attributes(SLSTables$`Water Info1`)$class <- "data.frame"
    # all.equal(SLSTables$`Water Info1` %>%
    #             arrange_all(),
    #           SLSTables$`Water Info` %>%
    #             mutate(Date = as.Date(Date)) %>%
    #             arrange_all())
    # # Because Temp is
    # SLSTables$`Water Info1` %>%
    #   arrange_all() %>%
    #   pull(Temp) %>%
    #   .[[1]] %>%
    #   sprintf("%.54f", .)
    #
    # SLSTables$`Water Info` %>%
    #   arrange_all() %>%
    #   pull(Temp) %>%
    #   .[[1]] %>%
    #   sprintf("%.54f", .)
    # # I'll have to just round in the waterInfo table below to make the formats uniform between both methods
    # # 7.700000000000000177635683940025046467781066894531250000 vs 7.699999809265136718750000000000000000000000000000000000
  }



  waterInfo <- SLSTables$`Water Info` %>%
    # Formatting the lat/lon into decimal degrees.
    mutate(across(c(Lat, Long), ~str_remove_all(.x, "-|\\.")),
           # After removing the "." that is occassionally in the DegSec, can now just add periods back correctly for Lat/Lon
           Lat = gsub('(?<=^.{2})', '.', Lat, perl = TRUE),
           Long = gsub('(?<=^.{2})', '.', Lat, perl = TRUE),
           # Converting secchi from cm to m
           Secchi = Secchi/100,
           # Converting EC to salinity
           # Per SOP normalized at temp = 25C; units seems to be in millisiemens (check with Adam)
           Sal_surf = wql::ec2pss(TopEC/1000, t = 25),
           Sal_bot = wql::ec2pss(BottomEC/1000, t = 25),
           # This is to take care of how the floats are read between Access and readr methods...
           Temp_surf = round(Temp, 2),
           # Changing Date to as.Date
           Date = as.Date(Date))
  # all.equal(waterInfo %>% arrange_all(), waterInfo1 %>% arrange_all())
  # # Looks fine. Date and Temp are different; Temp is taken care of by Temp_surf; date will be converted later

  towInfo <- SLSTables$`Tow Info` %>%
    # Creating a DateTime column to accomodate the Time column in the database
    rename(Datetime = Time) %>%
    # 1 parsing error because time was not recorded for that row
    # Now, joining to the MeterCorrections table to calculate tow volume later
    # This is based on the "WEB_Raw_Catch_Volum_Info" query in the SLS Access query database
    # First need to create a study year column; from the query above, the "WEB_VolumeInfo" query calculates year from the
    # towInfo table.
    mutate(StudyYear = lubridate::year(Date),
           Date = as.Date(Date)) %>%
    left_join(SLSTables$`Meter Corrections` %>%
                # FOR SOME REASON, there are duplicated entries in this table in Access itself...
                # Need to check with Adam again but will simply distinct for now
                distinct(),
              by = c("StudyYear", "NetMeterSerial" = "MeterSerial")) %>%
    # Moving on to various required calculations
    mutate(Datetime = str_remove(Datetime, "1899-12-30 ") %>%
             {parse_datetime(paste(Date, .),
                             format = "%Y-%m-%d %H:%M:%S",
                             locale = locale(tz = "America/Los_Angeles"))},
           # Now to turn tide into factors as outlined in the metadata file
           Tide = case_when(Tide == 1 ~ "High Slack",
                            Tide == 2 ~ "Ebb",
                            Tide == 3 ~ "Low Slack",
                            Tide == 4 ~ "Flood",
                            TRUE ~ NA_character_),
           # Converting bottom depth to meters
           Depth = BottomDepth * 0.3048,
           # Calculating tow volume for the fish net, 0.37 as the area of the mout of the net (per Access + SLS SOP document)
           Tow_volume = NetMeterCheck * kfactor * 0.37)
  # all.equal(towInfo %>% arrange_all(), towInfo1 %>% arrange_all())

  catch <- SLSTables$Catch %>%
    mutate(Date = as.Date(Date)) %>%
    rename(QuarterSubsample = X1.4.Subsampled,
           HalfSubsample = X1.2.Subsampled)
  # all.equal(catch %>% arrange_all(), catch1 %>% arrange_all())

  lengths <- SLSTables$Lengths %>%
    mutate(Date = as.Date(Date)) %>%
    # Calculating total number of fish measured (across all lengths) and # of fish measured per length
    group_by(Date, Station, Tow, FishCode) %>%
    add_count(name = "TotalLengthMeasured") %>%
    ungroup()
  # all.equal(lengths %>% arrange_all(), lengths1 %>% arrange_all())

  # Now to combine the datasets together, following the relationship table in Access
  # The table goes waterInfo -> towInfo -> Catch -> lengths
  dfFin <- waterInfo %>%
    full_join(towInfo,
              by = c("Date", "Station")) %>%
    full_join(catch,
              by = c("Date", "Station", "Tow")) %>%
    full_join(lengths,
              by = c("Date", "Station", "Tow", "FishCode")) %>%
    # Merging the two comment columns together; they both have data in them
    unite("Comments", c(Comments.x, Comments.y), sep = "; ", remove = T, na.rm = T) %>%
    # Changing date to as.Date only; the time zone data is captured in the DateTime column
    # Dealing with plus counts
    mutate(Date = as.Date(Date, tz = "America/Los_Angeles"),
           # Each length input has its own entry/row here so no need to have a count of the actual length measured per row
           Count = (Catch/TotalLengthMeasured),
           # Creating Length_NA_flag to parallel the other survey datasets in LTMR
           Length_NA_flag = if_else(Catch == 0, "No fish caught", NA_character_),
           # Creating Method column
           Method = "Oblique tow",
           # Creating SampleID as is asked by Sam
           Source = "SLS",
           # Filler column meant to hold position in select below
           SampleID = NA) %>%
    # Removing CatchID and entryorder as they are not relevant to the dataset
    select(Source, Station, Latitude = Lat, Longitude = Long,
           Date, Datetime, Survey, SampleID, Method, Tow, Tow_volume,
           Temp_surf, TopEC, BottomEC, Sal_surf, Sal_bot, Secchi, Turbidity,
           Depth, Tide, CableOut, Duration,
           NetMeterSerial, NetMeterStart,NetMeterEnd, NetMeterCheck,
           CBMeterSerial, CBMeterStart, CBMeterEnd, CBMeterCheck,
           FishCode, QuarterSubsample, HalfSubsample, Catch, Length, Count,
           YolkSacorOilPresent, Length_NA_flag,
           Comments, MeterNotes = Notes) %>%
    arrange(Date, Datetime, Survey, Station, Tow, FishCode, Length) %>%
    # Creating SampleID after arranging everything correctly
    mutate(SampleID = paste(Source, row_number()))

  # Checking for duplications across all columns
  # janitor::get_dupes(dfFin)
  # As of 10-22-21, no duplications

  # # How many NAs are there?
  # colSums(is.na(dfFin))
  # # Catch with no fishcode?
  # dfFin %>%
  #   filter(is.na(Catch), !is.na(FishCode)) %>%
  #   View()
  # # These are actually missing datapoints. 2/3 have lengths, all 3 have fishcode

  # all.equal(dfFin, dfFin1)
  # # The 2 Attributes differences here can be removedvia attributes(dfFin1)$class <- "data.frame"
  # # Not of concern
  # # The last difference here has to deal with Datetime...
  # dfFin %>% mutate(Datetime1 = dfFin1$Datetime) %>%
  #   filter(Datetime != Datetime1) %>%
  #   nrow()
  # # Appears to be some weird attribute with posixct formatting. Seems like the actual values are ok

  return(list(SLSTables = SLSTables,
              dfFin = dfFin))
}

# Using Access
dataAccess <- pullSLS(useAccess = T)

dataNoAccess <- pullSLS(useAccess = F)

# Are they equal?
all.equal(names(dataAccess$dfFin), names(dataNoAccess$dfFin))
# Names are the same
all.equal(sapply(dataAccess$dfFin, typeof), sapply(dataNoAccess$dfFin, typeof))
# All column types are the same
all.equal(dataAccess$dfFin, dataNoAccess$dfFin)
# Can't really figure out the 3rd dfiference but:
# Summation of all numeric columns all the same?
all.equal(dataAccess$dfFin %>% select(where(is.numeric)) %>% summarise_all(~sum(., na.rm = T)),
          dataNoAccess$dfFin %>% select(where(is.numeric)) %>% summarise_all(~sum(., na.rm = T)) %>% data.frame)
