#' Integration of SLS Access base tables into a long format data frame
#'
#' This function will read in the individual base tables from the SLS Access
#' database and integrate them into a singular long data format. The required
#' tables are "Catch", "Lengths", "Tow Info", "Water Info", and "20 mm Stations"
#' from the Access database.
#'
#' @param filePath Location of the base Access tables to be integrated. Default behavior is to simply read from the file path within data-raw/SLS folder of this package.
#'
#' @importFrom magrittr %>%
#' @return An integrated fish dataset of the CDFW SLS Survey.
#' @export SLSIntegrate
#'


SLSIntegrate <- function(filePath = NULL) {
  # Reading the data --------------------------------------------------------

  if (is.null(filePath)) {
    filePath <- file.path("data-raw", "SLS")
  }

  filesToRead <- list.files(path = filePath, pattern = ".txt")
  SLSTables <- list()

  SLSTables$Catch <- readr::read_delim(file.path("data-raw", "SLS", "Catch.txt"), delim = ",",
                                col_types =
                                  readr::cols_only(
                                    Date = readr::col_character(),
                                    Station = readr::col_character(),
                                    Tow = readr::col_integer(),
                                    FishCode = readr::col_integer(),
                                    Catch = readr::col_integer(),
                                    CatchID = readr::col_integer()
                                  )
  ) %>%
    dplyr::mutate(Date=lubridate::mdy_hms(Date))

  SLSTables$Lengths <- readr::read_delim(file.path("data-raw", "SLS", "Lengths.txt"), delim = ",",
                                  col_types =
                                    readr::cols_only(
                                      Date = readr::col_character(),
                                      Station = readr::col_character(),
                                      Tow = readr::col_integer(),
                                      FishCode = readr::col_integer(),
                                      Length = readr::col_integer(),
                                      entryorder = readr::col_integer()
                                    )
  )%>%
    dplyr::mutate(Date=lubridate::mdy_hms(Date))

  SLSTables$`Meter Corrections` <- readr::read_delim(file.path("data-raw", "SLS", "Meter Corrections.txt"), delim = ",",
                                              col_types =
                                                readr::cols_only(
                                                  StudyYear = readr::col_double(),
                                                  MeterSerial = readr::col_integer(),
                                                  CalibrationDate = readr::col_character(),
                                                  kfactor = readr::col_double(),
                                                  Notes = readr::col_character()
                                                )
  )%>%
    dplyr::mutate(CalibrationDate=lubridate::mdy_hms(CalibrationDate))

  SLSTables$`Tow Info` <- readr::read_delim(file.path("data-raw", "SLS", "Tow Info.txt"), delim = ",",
                                     col_types =
                                       readr::cols_only(
                                         Date = readr::col_character(),
                                         Station = readr::col_character(),
                                         Tow = readr::col_integer(),
                                         Time = readr::col_character(),
                                         Tide = readr::col_character(),
                                         BottomDepth = readr::col_integer(),
                                         CableOut = readr::col_integer(),
                                         Duration = readr::col_double(),
                                         NetMeterSerial = readr::col_integer(),
                                         NetMeterStart = readr::col_integer(),
                                         NetMeterEnd = readr::col_integer(),
                                         NetMeterCheck = readr::col_integer(),
                                         CBMeterSerial = readr::col_integer(),
                                         CBMeterStart = readr::col_integer(),
                                         CBMeterEnd = readr::col_integer(),
                                         CBMeterCheck = readr::col_integer(),
                                         Comments = readr::col_character()
                                       )
  ) %>%
    # Just going to change this to UTC here b/c that's how the database from Access is read as
    # This gets changed later to America/Los_Angeles in creating the dataframes for the final table
    dplyr::mutate(Time=lubridate::mdy_hms(Time, tz="America/Los_Angeles"),
           Date=lubridate::mdy_hms(Date, tz="America/Los_Angeles"),
           CableOut=CableOut * 0.3048)%>% # Convert feet to meters
    dplyr::rename(Notes_tow=Comments)

  SLSTables$`Water Info` <- readr::read_delim(file.path("data-raw", "SLS", "Water Info.txt"), delim = ",",
                                       col_types =
                                         readr::cols_only(
                                           Survey = readr::col_integer(),
                                           Date = readr::col_character(),
                                           Station = readr::col_character(),
                                           Temp = readr::col_double(),
                                           TopEC = readr::col_integer(),
                                           BottomEC = readr::col_integer(),
                                           Secchi = readr::col_integer(),
                                           Turbidity = readr::col_integer(),
                                           Comments = readr::col_character()
                                         )
  )%>%
    dplyr::mutate(Date=lubridate::mdy_hms(Date))%>%
    dplyr::rename(Notes_env=Comments)

  SLSTables$`20mm Stations`<-readr::read_delim(file.path("data-raw", "SLS", "20mm Stations.txt"), delim = ",",
                                        col_types =
                                          readr::cols_only(
                                            Station = readr::col_character(),
                                            LatD = readr::col_double(),
                                            LatM = readr::col_double(),
                                            LatS = readr::col_double(),
                                            LonD = readr::col_double(),
                                            LonM = readr::col_double(),
                                            LonS = readr::col_double()
                                          ))%>%
    dplyr::mutate(Latitude=(LatD + LatM/60 + LatS/3600),
           Longitude= -(LonD + LonM/60 + LonS/3600))

  # Manipulating the data tables --------------------------------------------

  waterInfo <- SLSTables$`Water Info` %>%
    dplyr::mutate(# Converting secchi from cm to m
      Secchi = Secchi/100,
      # Converting EC to salinity
      # Per SOP normalized at temp = 25C; units are in millisiemens
      Sal_surf = wql::ec2pss(TopEC/1000, t = 25),
      Sal_bot = wql::ec2pss(BottomEC/1000, t = 25),
      # This is to take care of how the floats are read between Access and readr methods...
      Temp_surf = round(Temp, 2),
      # Changing Date to as.Date
      Date = as.Date(Date))

  towInfo <- SLSTables$`Tow Info` %>%
    # 1 parsing error because time was not recorded for that row
    # Now, joining to the Meter Corrections table to calculate tow volume later
    # This is based on the "WEB_Raw_Catch_Volum_Info" query in the SLS Access query database
    # First need to create a study year column; from the query above, the "WEB_VolumeInfo" query calculates year from the
    # towInfo table.
    dplyr::mutate(StudyYear = lubridate::year(Date),
                  # Changing Date from POSIXct format to simply Date for easy of use
                  # Timezone data is still retained in the Datetime column
                  Date = as.Date(Date)) %>%
    dplyr::left_join(SLSTables$`Meter Corrections` %>%
                       # There are duplicated values here in this table; will simply distinct() them
                       # Confirmed via email with Adam, ES of Native Fish unit as of 10-27-2021
                       dplyr::distinct(),
                     by = c("StudyYear", "NetMeterSerial" = "MeterSerial")) %>%
    # Moving on to various required calculations
    dplyr::mutate(Datetime = lubridate::ymd_hms(dplyr::if_else(is.na(Time),
                                                               NA_character_,
                                                               paste(Date, paste(lubridate::hour(Time),
                                                                                 lubridate::minute(Time),
                                                                                 lubridate::second(Time), sep=":"))),
                                     tz = "America/Los_Angeles"),
                  # Now to turn tide into factors as outlined in the metadata file
                  Tide = dplyr::case_when(Tide == 1 ~ "High Slack",
                                          Tide == 2 ~ "Ebb",
                                          Tide == 3 ~ "Low Slack",
                                          Tide == 4 ~ "Flood",
                                          TRUE ~ NA_character_),
                  # Converting bottom depth to meters
                  Depth = BottomDepth * 0.3048,
                  # Calculating tow volume for the fish net, 0.37 as the area of the mout of the net (per Access + SLS SOP document)
                  Tow_volume = NetMeterCheck * kfactor * 0.37)

  catch <- SLSTables$Catch %>%
    dplyr::mutate(Date = as.Date(Date))

  lengths <- SLSTables$Lengths %>%
    dplyr::mutate(Date = as.Date(Date)) %>%
    # Calculating total number of fish measured (across all lengths) and # of fish measured
    # per Date, Station, Tow, and FishCode
    # This is to calculate plus counts later in dfFin
    dplyr::group_by(Date, Station, Tow, FishCode, Length)%>%
    dplyr::summarise(LengthFrequency = dplyr::n(), .groups="drop")%>%
    dplyr::group_by(Date, Station, Tow, FishCode) %>%
    dplyr::mutate(TotalLengthMeasured = sum(LengthFrequency)) %>%
    dplyr::ungroup()

  # Now to combine the datasets together, following the relationship table in Access
  # The tables go waterInfo -> towInfo -> Catch -> lengths
  SLS <- waterInfo %>%
    dplyr::full_join(towInfo,
              by = c("Date", "Station")) %>%
    dplyr::full_join(catch,
              by = c("Date", "Station", "Tow")) %>%
    dplyr::full_join(lengths,
              by = c("Date", "Station", "Tow", "FishCode")) %>%
    # Adding in taxa name based on Species Code.csv file
    dplyr::left_join(LTMRdata::Species %>%
                dplyr::select(TMM_Code,
                       Taxa) %>%
                dplyr::filter(!is.na(TMM_Code)),
              by = c("FishCode"="TMM_Code")) %>%
    dplyr::left_join(SLSTables$`20mm Stations`,
              by="Station")%>%
    # Merging the two comment columns together; they both have data in them
    dplyr::mutate(Notes_tow=paste(Notes_tow, Notes_env, sep = "; ")) %>%
    dplyr::arrange(Date, Datetime, Survey, Station, Tow, Taxa, Length) %>%
    dplyr::mutate(Source = "SLS",
           SampleID=paste(Source, Date, Station, Tow), # Creating SampleID index
           Count = dplyr::if_else(is.na(Length), as.numeric(Catch), (LengthFrequency/TotalLengthMeasured) * Catch),
           # Creating Length_NA_flag to parallel the other survey datasets in LTMR
           Length_NA_flag = dplyr::if_else(is.na(Count), "No fish caught", NA_character_),
           # Creating Method column; Adam described this as an "Olbique tow", significantly diff from WMT
           Method = "Oblique tow",
           Station=as.character(Station))%>%
    # Removing CatchID and entryorder as they are not relevant to the dataset
    # Removing TopEC, BottomEC as they have been converted over the salinity already
    # Removing CBMeterSerial, CBMeterStart, CBMeterEnd, CBMeterCheck as CB not ran on the SLS
    dplyr::select(Source, Station, Latitude, Longitude,
           Date, Datetime, Survey, Depth, SampleID, Method,
           Tide, Sal_surf, Sal_bot, Temp_surf, Secchi, Turbidity, Tow_volume,
           Cable_length=CableOut, Tow_duration=Duration,
           Taxa, Length, Count, Length_NA_flag,
           Notes_tow, Notes_flowmeter = Notes)

  # # Just to make sure that no duplications occurred; lengths should be the same
  # all.equal(lengths$Length %>% sum(na.rm = T),
  #           dfFin$Length %>% sum(na.rm = T))

  SLS
}
