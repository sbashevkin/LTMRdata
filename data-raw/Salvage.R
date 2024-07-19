require(dplyr)
require(tidyr)
require(LTMRdata)
require(DBI)
require(odbc)
require(lubridate)
require(LTMRdata)

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

# Setting up the combined table to link with correct ID"s
SalvageJoined <- full_join(SalvageTables$Sample, SalvageTables$Building,
                     by = "SampleRowID", multiple = "all") %>%
  full_join(SalvageTables$Catch, by = "BuildingRowID", multiple = "all") %>%
  full_join(SalvageTables$Length, by = "CatchRowID", multiple = "all") %>%
  left_join(SalvageTables$OrganismsLookUp, by = "OrganismCode", multiple = "all") %>%
  left_join(SalvageTables$StudiesLookUp, by = "StudyRowID", multiple = "all") %>%
  left_join(SalvageTables$StationsLookUp, by = c("BuildingCode" = "FacilityCode"), multiple = "all")

# OrganismCode 98 and 99 are Total Fish Count, Total Fish Estimate, but they are generally recorded as 0.
# Will have to remove them from the dataset as there are fish caught during these sampling events.
# However, there are specific instances where these are the only occurence that represents a sampling event...
# In those cases, cannot remove them as you'll lose environmental data (e.g., export data does not align)
# Need to keep these, while removing all rows with 98/99 (if not, fails testthat)
rowsToRemove <- SalvageJoined %>%
  distinct(SampleRowID, OrganismCode) %>%
  group_by(SampleRowID) %>%
  add_tally(name = "nTotalCount") %>%
  filter((OrganismCode %in% c(98, 99) & nTotalCount != 1) |
           # Removing OrganismCode == 81, Chinese Mitten Crab Temp
           # These were collected using a different sampling methods than
           # Salvage at the CVP facility mainly in 1999. Although the number of
           # crabs is significant, these catches are not comparable to regular
           # salvage and is being removed.
           OrganismCode == 81) %>%
  pull(SampleRowID)

# Fixes to the dataset to pass package tests
# After joining, calculate and keep only relevant columns
SalvageStart <- SalvageJoined %>%
  filter(Description %in% c("Normal count", "Second flush"),
         !(OrganismCode %in% c(98, 99) & SampleRowID %in% rowsToRemove)) %>%
  mutate(Facility = case_when(SampleMethod == 1 ~ "SWP",
                              SampleMethod == 2 ~ "CVP",
                              is.na(SampleMethod) & !is.na(Comments_StationsLookUp) ~ Comments_StationsLookUp),
         Station = paste(Facility, Location),
  Station = ifelse(Station %in% "CVP NA", "CVP Federal Facility", Station)) %>%
  # Specific column to indicate that no fish has been caught
  group_by(SampleRowID, Station) %>%
  mutate(FishCaught = ifelse(sum(LengthFrequency, na.rm = T) > 0 | sum(Count, na.rm = T) > 0, T, F)) %>%
  group_by(CatchRowID) %>%
  mutate(TotalMeasured = sum(LengthFrequency, na.rm = T)) %>%
  ungroup() %>%
  # Binding in the Taxa name here; not directly the OrganismsLookUp table, see code at end for details
  left_join(Species %>%
              distinct(OrganismCode = Salvage_Code,
                       Taxa) %>%
              filter(!grepl("Age", Taxa),
                     !is.na(OrganismCode)),
            by = "OrganismCode") %>%
  # Instances in which LengthRowID is the only thing differentiating samples. Some of these
  # will have the same fork length and taxa, meaning there will be duplications once LengthRowID
  # is removed (as in the final table here). As such, will add up these values in a summary
  # group_by(across(-c(LengthRowID, AdiposeClip))) %>%
  group_by(Facility, Station, Location, Comments_StationsLookUp, SampleDate, SampleTime, SampleRowID,
           Description, AcreFeet, WaterTemperature, CatchRowID, Taxa, TotalMeasured, Count,
           ForkLength, MinutesPumping, SampleTimeLength, FishCaught, StudyRowID, OrganismCode,
           Comments_Sample) %>%
  summarise(LengthFrequency = sum(LengthFrequency), .groups = "drop") %>%
  transmute(Source = "Salvage",
            Facility,
            Station,
            Salvage_building = Location,
            Latitude = case_when(Comments_StationsLookUp == "SWP" ~ 37.825612769565474,
                                 Comments_StationsLookUp == "CVP" ~ 37.81667106195238),
            Longitude = case_when(Comments_StationsLookUp == "SWP" ~ -121.59584120116043,
                                  Comments_StationsLookUp == "CVP" ~ -121.55857777929133),
            Date = parse_date_time(SampleDate, "%Y-%m-%d", tz="America/Los_Angeles"),
            # Will produce a warning about failed parses. Due to daylight saving times;
            # There are entries in which the observer did not properly jump times entering
            # in entries that cannot exist, e.g., 2 AM when jumping forward since that is
            # automatically 3 AM.
            # 638 failed parses as of 07-01-24
            Datetime = parse_date_time(paste0(Date, " ", SampleTime),
                                       orders = "%Y-%m-%d %H:%M:%S",
                                       tz = "America/Los_Angeles"),
            # Fix nonexistent daylight savings times by setting them to 3AM
            Datetime = if_else(is.na(Datetime) & SampleTime=="02:00:00.0000000",
                               parse_date_time(paste0(Date, " 03:00:00"),
                                               orders = "%Y-%m-%d %H:%M:%S",
                                               tz = "America/Los_Angeles"),
                               Datetime),
            SampleRowID,
            # Here, 0000 = normal count, 9999 = second flush, 7777 = traveling screen count, and 8888 = special study
            Method = Description,
            # MethodSalvageDescription = Description, # I don't know a good name for this
            # Changing acre feet volume to cubic meter
            Tow_volume = AcreFeet * 1233.48,
            # MinutesPumping, SampleTimeLength,
            Temp_surf = (WaterTemperature - 32) * 5/9, # Is this really surface temperature? It's well mixed
            # PrimaryDepth, PrimaryFlow, BayPump1, BayPump2, BayPump3, BayPump4,
            # BayPump5, Sampler, QCed,
            # BuildingCode,
            # PrimaryBypass, SecondaryDepth, SecondaryFlow, HoldingTankFlow,
            # OrganismCode, CommonName,
            CatchRowID,
            # As of April 28, 2023, this filter removes 15 instances of when there was 0 fish caught but a taxa recorded
            # Not related to the 98 and 99 filter
            Taxa = ifelse(!is.na(Taxa) & FishCaught != T, NA_character_, Taxa),
            TotalMeasured,
            Subsampled = ifelse(Count > TotalMeasured, T, F),
            # If Count < TotalMeasured, simply use the TotalMeasured value for Count
            MoreMeasured = ifelse(TotalMeasured > Count, T, F),
            Count1 = ifelse(!is.na(TotalMeasured) & ((TotalMeasured > Count) |
                                                       (!is.na(TotalMeasured) & is.na(Count))),
                            TotalMeasured, Count),
            LengthFrequency = as.numeric(LengthFrequency),
            # If there is no fish measured, the pure count data is used to calculate expandedCount
            # Otherwise (for most cases), the length frequency is used to calculate expandedCount
            ExpandedCount = ifelse(!is.na(TotalMeasured) & TotalMeasured != 0 & (Count1 >= TotalMeasured),
                                   (LengthFrequency/TotalMeasured) * Count1, Count1),
            # This filter removes 12 rows (after all the filters before this). Simply change these 0s to NAs
            # to be labeled as unknown lengths
            Length = ifelse(ForkLength == 0, NA, ForkLength),
            Count = case_when(
              # In this specific sample, sample time length was not recorded. Imputing with 20 min, the median for all instances when MinutesPumping == 60
              SampleRowID == 29724 ~ ExpandedCount * (MinutesPumping/20),
              (is.na(MinutesPumping) | MinutesPumping == 0 | SampleTimeLength == 0 | is.na(SampleTimeLength)) & FishCaught != T ~ 0,
              is.na(Station) & FishCaught != T ~ 0,
              StudyRowID == "0000" ~ ExpandedCount * (MinutesPumping/SampleTimeLength),
              StudyRowID == "9999" ~ as.numeric(ExpandedCount),
              StudyRowID == "8888" ~ 0,
              # OrganismCode == 98, 99 simply leave Count as 0
              OrganismCode %in% c(98, 99) ~ 0,
              TRUE ~ 0),
            # AdiposeClip, Sex,
            Notes_tow = Comments_Sample,
            # Comments_OrganismsLookUp,
            # Some additional flags
            Length_NA_flag = case_when(FishCaught != T ~ "No fish caught",
                                       is.na(Length) & Count > 0 ~ "Unknown length",
                                       TRUE ~ NA_character_),
            FishCaught
            # Unmatched Data
            # Unmatched_Data = ifelse(is.na(Date), T, F),
            # TimeStart_Impossible = ifelse(is.na(Datetime) & !is.na(SampleTime), T, F)
  ) %>%
  # Need to remove the unmatched data for the package check to work
  filter(!is.na(Date)) %>%
  group_by(CatchRowID) %>%
  # Fixing CatchRowID 911654 and 1172485 in which length freq was NA for 1 row instance but other row instances had lengths
  # Catch will be expanded to all available lengths and the NAs will be removed
  # Also, CatchRowID 139236 and 139307 is changed as well, LengthFrequency == 0 for these rows for some reason.
  mutate(lengthNAs = ifelse(!is.na(CatchRowID) & FishCaught == T,
                            sum(ifelse(is.na(LengthFrequency) | LengthFrequency == 0, T, F) & is.na(Length_NA_flag), na.rm = T),
                            0)) %>%
  ungroup() %>%
  # 16 instances of NA counts from the CVP because
  # Removes just the four CatchRowID
  filter(!(lengthNAs == 1 & (LengthFrequency == 0 | is.na(LengthFrequency))
           # & Method == "Normal count"
           )) %>%
  # Creating a SampleID. Want to retain SampleRowID from database but also distinguish each building at the SWP
  {
    left_join(., distinct(.data = ., Source, Facility, Salvage_building, SampleRowID) %>%
                group_by(Facility, SampleRowID) %>%
                mutate(SampleID = paste(Source, SampleRowID, 1:n())),
              by = c("Source", "Facility", "Salvage_building", "SampleRowID"))
  } %>%
  relocate(SampleID, .after = SampleRowID)

Salvage_measured_lengths <- SalvageStart %>%
  select(SampleID, Taxa, Length, LengthFrequency) %>%
  filter(!is.na(LengthFrequency)) %>% # Remove fish that weren't measured
  rename(Count = LengthFrequency)

Salvage <- SalvageStart %>%
  select(-c(Facility, Salvage_building, SampleRowID,
            TotalMeasured, Subsampled, MoreMeasured, Count1, LengthFrequency, ExpandedCount,
            lengthNAs, CatchRowID, FishCaught)) %>%
  # Because rows with 0 catch but has a taxa had their taxa changed to NAs,
  # line ~124 in this script, this create some duplicates. removing here (6 rows as of 05-05-23)
  # Without CatchRowID, these become duplicates
  # SamplwRowID: 73711, 73767, 183239
  distinct()

# Final check
if (nrow(SalvageStart) - nrow(Salvage) != 6) stop("The last distinct() step removed more rows than intended. Check.")

# When are periods of no sampling? May occur when:
# Pumping minutes is NA or 0
# Count is NA or 0
# Tow volume is NA or 0
# Taxa is NA
noSamplingMaybe <- Salvage %>%
  filter((is.na(Count) | Count == 0) |
           (is.na(Tow_volume | Tow_volume == 0)) |
           is.na(Taxa))
# Unique string of all the potential comments used here

# Now, removing periods where there was no fish sampling; this is because
# water quality sampling may still occur with no fish sampling--we don't want those in this dataset
# These values appears to be comprehensive for now. Tested to see if they yield correct results.

noSamplingRegex <- c(
  "no\\s*(count|cnt|fish\\s*(count|cnt|salvage))",
  "(shut\\s*down|shutdown|facilityshutdown)",
  "down\\s*for\\s*(maintenance|repairs)",
  "no\\s*(pumping|water|flow|salvage|export|TFF.*salvage)",
  "(zero|0)\\s*(count|slvg|export|flow|pumping|salvage|units|bapp)",
  # "facility\\s*(shut\\s*down|shutdown)",
  "(power\\s*outage|equipment\\s*failure)",
  # "trash\\s*rack\\s*failure",
  # "hoist\\s*(down|failure)",
  # "screen\\s*failure",
  # "vamp",
  # "no\\s*data",
  "stopped\\s*pumping\\s*water",
  # "(bjpp|jpp)\\s*shutdown",
  # "no\\s*tracy\\s*pumps",
  # "pump\\s*not\\s*working",
  "no\\s+(fish\\s+|10\\s+(min(ute)?\\.?\\s+)?)?(count)"
  # "^0+\\s*cfs"
) # 1072
# Not all of these are required; I've commented out the redundant ones
# Not deleting them though in case they are useful in the future

# This is a conservative filter as it also requires Count == 0
Salvage <- Salvage %>%
  mutate(
    noSampling = grepl(paste(noSamplingRegex, collapse = "|"), Notes_tow, ignore.case = T)
    # noSamplingTime = grepl("[0-9]{2,4}([:][0-9]{2})?([ ]?(HRS))?", Notes_tow, ignore.case = T) & noSampling,
    # didNotShutDown = grepl("did not shut down", Notes_tow, ignore.case = T) & noSampling
  ) %>%
  # This removes 1072 records as of 7-18-2024
  filter(!(noSampling & Count == 0 & Method == "Normal count"),
         # Remove Sampling from the CVP between 2021-06-11 to 2021-06-16. No pumping at this facility
         # No comments written for these entries though. Confirmed via datasheet provided by
         # Kyle Griffiths on 2024-07-16. This should be only 66 rows for this specific filter
         !(between(Date, as.Date("2021-06-11"), as.Date("2021-06-16")) &
             Station == "CVP Federal Facility" & Tow_volume == 0 & Count == 0),
         # This singular row too specific to match with regex without false matches
         !SampleID %in% "Salvage 219623 1",
         # Kyle Griffiths confirmed that on these dates, the SWP was not sampling
         !(Date %in% as.POSIXct(c("2022-05-17", "2022-05-18", "2022-05-19", "2022-06-07",
                     "2022-06-29", "2022-06-30", "2022-07-01")) &
            Station == "SWP NA")) %>%
  select(-noSampling)

# # This is the expansion of this dataset, checked against the CDFW website
# SalvageFinal <- Salvage %>%
#   mutate(Taxa = factor(Taxa),
#          originalData = T) %>%
#   group_by(Date,
#            Station = factor(Station, levels = c("SWP", "CVP")),
#            Tow_volume) %>%
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

# # For updating the species table with the salvage codes
# SalvageTables$OrganismsLookUp %>%
#   transmute(Salvage_Code = OrganismCode,
#             ScientificName = case_when(OrganismCode == 90 ~ "UnID",
#                                        Genus == "NA" & Family != "NA" ~ Family,
#                                        Genus == "NA" & Family == "NA" ~ CommonName,
#                                        TRUE ~ paste(Genus, Species))) %>%
#   mutate(ScientificName = trimws(sub("NA", "", ScientificName)),
#          # Also, correcting names
#          ScientificName = case_when(ScientificName == "Anarrhichthys ocellantus" ~ "Anarrhichthys ocellatus",
#                                     ScientificName == "Clupea pallasi" ~ "Clupea pallasii",
#                                     ScientificName == "Hyperprosopon Argenteum" ~ "Hyperprosopon argenteum",
#                                     ScientificName == "Hysterocarpus traski" ~ "Hysterocarpus traskii",
#                                     ScientificName == "Lampetra ayresi" ~ "Lampetra ayresii",
#                                     ScientificName == "Lepidopsetta bilineatta" ~ "Lepidopsetta bilineata",
#                                     ScientificName == "Morone Chrysops" ~ "Morone chrysops",
#                                     ScientificName == "Palaemon macrodactylum" ~ "Palaemon macrodactylus",
#                                     ScientificName == "Phanerodon furactus" ~ "Phanerodon furcatus",
#                                     ScientificName == "Potamocorbula amurenis" ~ "Potamocorbula amurensis",
#                                     ScientificName == "Spirinchus starski" ~ "Spirinchus starksi",
#                                     TRUE ~ ScientificName)) %>%
#   full_join(Species, by = "ScientificName") %>%
#   relocate(Salvage_Code, .after = "TMM_Code") %>%
#   write_csv(file.path("data-raw", "Species codes.csv"))

usethis::use_data(Salvage, Salvage_measured_lengths, overwrite=TRUE, compress="xz")
