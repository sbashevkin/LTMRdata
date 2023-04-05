require(dplyr)
require(tidyr)
require(LTMRdata)
require(DBI)
require(odbc)

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
# Changing some names to avoid duplicated names when joining
SalvageTables$Sample <- SalvageTables$Sample %>%
  rename(Comments_Sample = Comments)

SalvageTables$OrganismsLookUp <- SalvageTables$OrganismsLookUp %>%
  rename(Active_OrganismsLookUp = Active,
         Comments_OrganismsLookUp = Comments)

SalvageTables$StationsLookUp <- SalvageTables$StationsLookUp %>%
  rename(Active_StationsLookUp = Active,
         Comments_StationsLookUp = Comments)

# setting up the combined table to link with correct ID"s
SalvageJoined <- full_join(SalvageTables$Sample, SalvageTables$Building,
                     by = "SampleRowID", multiple = "all") %>%
  full_join(SalvageTables$Catch, by = "BuildingRowID", multiple = "all") %>%
  full_join(SalvageTables$Length, by = "CatchRowID", multiple = "all") %>%
  left_join(SalvageTables$OrganismsLookUp, by = "OrganismCode", multiple = "all") %>%
  left_join(SalvageTables$StudiesLookUp, by = "StudyRowID", multiple = "all") %>%
  left_join(SalvageTables$StationsLookUp, by = c("BuildingCode" = "FacilityCode"), multiple = "all")

# After joining, calculate and keep only relevant columns
Salvage <- SalvageJoined %>%
  group_by(CatchRowID) %>%
  mutate(TotalMeasured = sum(LengthFrequency, na.rm = T)) %>%
  ungroup() %>%
  transmute(SampleDate = as.Date(SampleDate),
            SampleTimeString = SampleTime,
            SampleTime = as.POSIXct(paste0(SampleDate, " ", SampleTime),
                                    format = "%Y-%m-%d %H:%M:%S",
                                    tz = "America/Los_Angeles"),
            StudyRowID, StudyRowDescription = Description,
            AcreFeet, MinutesPumping, SampleTimeLength,
            WaterTemperature = (WaterTemperature - 32) * 5/9,
            PrimaryDepth, PrimaryFlow, BayPump1, BayPump2, BayPump3, BayPump4,
            BayPump5, Sampler, QCed,
            BuildingCode, Building = Location, Facility = Comments_StationsLookUp,
            # PrimaryBypass, SecondaryDepth, SecondaryFlow, HoldtingTankFlow,
            OrganismCode, CommonName,
            CatchRowID, Count, TotalMeasured,
            Subsampled = ifelse(Count > TotalMeasured, T, F),
            # If Count < TotalMeasured, simply use the TotalMeasured value for Count
            MoreMeasured = ifelse(TotalMeasured > Count, T, F),
            Count = ifelse(!is.na(TotalMeasured) & (TotalMeasured > Count), TotalMeasured, Count),
            LengthFrequency = as.numeric(LengthFrequency),
            # If there is no fish measured, the pure count data is used to calculate expandedCount
            # Otherwise (for most cases), the length frequency is used to calculate expandedCount
            ExpandedCount = ifelse(!is.na(TotalMeasured) & TotalMeasured != 0 & (Count >= TotalMeasured),
                                   (LengthFrequency/TotalMeasured) * Count, Count),
            ExpandedSalvage = case_when(StudyRowID == "0000" ~ ExpandedCount * (MinutesPumping/SampleTimeLength),
                                        StudyRowID == "9999" ~ as.numeric(ExpandedCount),
                                        StudyRowID == "8888" ~ 0),
            ForkLength, AdiposeClip, Sex,
            Comments_Sample, Comments_OrganismsLookUp,
            # Some additional flags
            Length_NA_flag = case_when(is.na(LengthFrequency) & is.na(ForkLength) & is.na(OrganismCode) ~ "No fish caught",
                                       is.na(LengthFrequency) & is.na(ForkLength) & !is.na(OrganismCode) ~ "No length measured",
                                       is.na(ForkLength) | ForkLength == 0 ~ "Unknown length",
                                       TRUE ~ NA_character_),
            # Unmatched Data
            Unmatched_Data = ifelse(!is.na(SampleDate), T, F),
            TimeStart_Impossible = ifelse(is.na(SampleTime) & !is.na(SampleTimeString), T, F))

SalvageFinal <- Salvage %>%
  mutate(OrganismCode = factor(OrganismCode),
         originalData = T) %>%
  group_by(SampleDate,
           Facility = factor(Facility, levels = c("SWP", "CVP")),
           AcreFeet) %>%
  complete(OrganismCode) %>%
  ungroup() %>%
  filter(!(is.na(Facility) & is.na(originalData)))

# # For salmon loss:
# Salvage %>%
#   mutate(BayPump1 = ifelse(BayPump1, 21, 0),
#          BayPump2 = ifelse(BayPump2, 21, 0),
#          BayPump3 = ifelse(BayPump3, 43, 0),
#          BayPump4 = ifelse(BayPump4, 43, 0),
#          BayPump5 = ifelse(BayPump5, 21, 0),
#          TotalWidth = ifelse(SampleMethod == 1, BayPump1 + BayPump2 + BayPump3 + BayPump4 + BayPump5, 84),
#          # Now to calculate loss
#          Encounter = ifelse(ForkLength < 101, ExpandedSalvage/(0.630 + (0.0494 * (PrimaryFlow/(PrimaryDepth * TotalWidth)))),
#                             ExpandedSalvage/(0.568 + (0.0579 * (PrimaryFlow/(PrimaryDepth * TotalWidth))))),
#          # unique(SampleMethod) = 1, 2
#          Entrain = ifelse(SampleMethod == 1, Encounter/0.25, Encounter/0.85),
#          Release = ifelse(ForkLength < 101, ExpandedSalvage * 0.98, ExpandedSalvage),
#          Loss = case_when(StudyRowID == "0000" ~ (Entrain - Release),
#                           SampleMethod == 1 & StudyRowID == "9999" ~ (ExpandedSalvage * 4.33),
#                           SampleMethod == 2 & StudyRowID == "9999" ~ (ExpandedSalvage * 0.569)))

usethis::use_data(Salvage, overwrite=TRUE)
