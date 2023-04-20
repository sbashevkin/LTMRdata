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
  left_join(SalvageTables$OrganismsLookUp %>%
              mutate(Taxa = paste(Genus, Species)), by = "OrganismCode", multiple = "all") %>%
  left_join(SalvageTables$StudiesLookUp, by = "StudyRowID", multiple = "all") %>%
  left_join(SalvageTables$StationsLookUp, by = c("BuildingCode" = "FacilityCode"), multiple = "all")

# After joining, calculate and keep only relevant columns
Salvage <- SalvageJoined %>%
  group_by(CatchRowID) %>%
  mutate(TotalMeasured = sum(LengthFrequency, na.rm = T)) %>%
  ungroup() %>%
  transmute(Source = "Salvage",
            Station = Comments_StationsLookUp,
            Salvage_building = Location,
            Latitude = case_when(Comments_StationsLookUp == "SWP" ~ 37.825612769565474,
                               Comments_StationsLookUp == "CVP" ~ 37.81667106195238),
            Longitude = case_when(Comments_StationsLookUp == "SWP" ~ -121.59584120116043,
                                  Comments_StationsLookUp == "CVP" ~ -121.55857777929133),
            Date = as.Date(SampleDate),
            DateTime = as.POSIXct(paste0(Date, " ", SampleTime),
                                    format = "%Y-%m-%d %H:%M:%S",
                                    tz = "America/Los_Angeles"),
            SampleID = paste(Source, SampleRowID),
            # Here, 0000 = normal count, 9999 = second flush, 7777 = traveling screen count, and 8888 = special study
            Method = Description,
            # MethodSalvageDescription = Description, # I don't know a good name for this
            # Changing acre feet volume to cubic meter
            Salvage_volume = AcreFeet * 1233.48,
            MinutesPumping, SampleTimeLength,
            Temp_surf = (WaterTemperature - 32) * 5/9, # Is this really surface temperature? It's well mixed
            # PrimaryDepth, PrimaryFlow, BayPump1, BayPump2, BayPump3, BayPump4,
            # BayPump5, Sampler, QCed,
            # BuildingCode,
            # PrimaryBypass, SecondaryDepth, SecondaryFlow, HoldtingTankFlow,
            # OrganismCode, CommonName, CatchRowID
            Taxa, TotalMeasured,
            Subsampled = ifelse(Count > TotalMeasured, T, F),
            # If Count < TotalMeasured, simply use the TotalMeasured value for Count
            MoreMeasured = ifelse(TotalMeasured > Count, T, F),
            Count1 = ifelse(!is.na(TotalMeasured) & (TotalMeasured > Count), TotalMeasured, Count),
            LengthFrequency = as.numeric(LengthFrequency),
            # If there is no fish measured, the pure count data is used to calculate expandedCount
            # Otherwise (for most cases), the length frequency is used to calculate expandedCount
            ExpandedCount = ifelse(!is.na(TotalMeasured) & TotalMeasured != 0 & (Count1 >= TotalMeasured),
                                   (LengthFrequency/TotalMeasured) * Count1, Count1),
            Length = ForkLength,
            Count = case_when(StudyRowID == "0000" ~ ExpandedCount * (MinutesPumping/SampleTimeLength),
                                        StudyRowID == "9999" ~ as.numeric(ExpandedCount),
                                        StudyRowID == "8888" ~ 0),
            # AdiposeClip, Sex,
            Notes_Sample = Comments_Sample,
            # Comments_OrganismsLookUp,
            # Some additional flags
            Length_NA_flag = case_when(is.na(LengthFrequency) & is.na(ForkLength) & is.na(OrganismCode) ~ "No fish caught",
                                       is.na(LengthFrequency) & Count > 0 ~ "Unknown length",
                                       TRUE ~ NA_character_),
            # Unmatched Data
            Unmatched_Data = ifelse(!is.na(Date), T, F),
            TimeStart_Impossible = ifelse(is.na(DateTime) & !is.na(SampleTime), T, F))

Salvage_measured_lengths <- Salvage %>%
  select(SampleID, Taxa, Length, LengthFrequency) %>%
  filter(!is.na(LengthFrequency)) %>% # Remove fish that weren't measured
  rename(Count = LengthFrequency)

Salvage <- Salvage %>%
  select(-c(TotalMeasured, Subsampled, MoreMeasured, Count1, LengthFrequency, ExpandedCount))

# # This is the expansion of this dataset, checked against the CDFW website
# SalvageFinal <- Salvage %>%
#   mutate(Taxa = factor(Taxa),
#          originalData = T) %>%
#   group_by(Date,
#            Station = factor(Station, levels = c("SWP", "CVP")),
#            Salvage_volume) %>%
#   complete(Taxa) %>%
#   ungroup() %>%
#   filter(!(is.na(Station) & is.na(originalData)))

# # For salmon loss:
# Salvage %>%
#   filter(OrganismCode == 1) %>%
#   mutate(BayPump1 = ifelse(BayPump1, 21, 0),
#          BayPump2 = ifelse(BayPump2, 21, 0),
#          BayPump3 = ifelse(BayPump3, 43, 0),
#          BayPump4 = ifelse(BayPump4, 43, 0),
#          BayPump5 = ifelse(BayPump5, 21, 0),
#          TotalWidth = ifelse(Facility =="SWP", BayPump1 + BayPump2 + BayPump3 + BayPump4 + BayPump5, 84),
#          # Now to calculate loss
#          Encounter = ifelse(Length < 101, Count/(0.630 + (0.0494 * (PrimaryFlow/(PrimaryDepth * TotalWidth)))),
#                             Count/(0.568 + (0.0579 * (PrimaryFlow/(PrimaryDepth * TotalWidth))))),
#          # unique(SampleMethod) = 1, 2
#          Entrain = ifelse(Station =="SWP", Encounter/0.25, Encounter/0.85),
#          Release = ifelse(Length < 101, Count * 0.98, Count),
#          Loss = case_when(Method == "0000" ~ (Entrain - Release),
#                           Station =="SWP" & Method == "9999" ~ (Count * 4.33),
#                           Station == "CVP" & Method == "9999" ~ (Count * 0.569)))

usethis::use_data(Salvage, Salvage_measured_lengths, overwrite=TRUE)
