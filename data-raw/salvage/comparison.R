# Comparing salvage dataset to the website:

# The overall flow here is:
# 1. pull salvage data from the CDFW website
# 2. load the salvage dataset as manipulated for LTMRdata
# 3. compare expanded salvage between the two sources across all species across time
# 4. Find where the differences are occurring and fix


# First, website data should be downloaded
load(file.path("data-raw", "salvage", "salvageWebsite_1993.RData"))

# This is the manipulated salvage database from the Access database that will be uploaded to LTMRdata
# However, I did keep many more columns here to explore
# *****IMPORTANT: this does NOT have the filter for including only normal count and second flush, which IS
# in the LTMRdata version.
SalvageAll <- SalvageJoined %>%
  filter(!(OrganismCode %in% c(98, 99) & SampleRowID %in% rowsToRemove)
         # Description == c("Normal count", "Second flush")
  ) %>%
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
            Datetime = as.POSIXct(paste0(Date, " ", SampleTime),
                                  format = "%Y-%m-%d %H:%M:%S",
                                  tz = "America/Los_Angeles"),
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
  filter(!(lengthNAs == 1 & (LengthFrequency == 0 | is.na(LengthFrequency)) & Method == "Normal count")) %>%
  # Creating a SampleID. Want to retain SampleRowID from database but also distinguish each building at the SWP
  {
    left_join(., distinct(.data = ., Source, Facility, Salvage_building, SampleRowID) %>%
                group_by(Facility, SampleRowID) %>%
                mutate(SampleID = paste(Source, SampleRowID, 1:n())),
              by = c("Source", "Facility", "Salvage_building", "SampleRowID"))
  } %>%
  relocate(SampleID, .after = SampleRowID)

# Create a summary of each species at each facility across entire datasets to find match
websiteSummary <- websiteCompare %>%
  bind_rows() %>%
  filter(between(SampleDate, as.Date("2007-10-01"), as.Date("2023-03-21"))) %>%
  group_by(OrganismCode = Species, CommonName = OrganismName, Facility) %>%
  summarise(Acrefeet = sum(Acrefeet, na.rm = T),
            Salvage = sum(Salvage, na.rm = T), .groups = "drop") %>%
  mutate(dataset = "website")

accessSummary <- SalvageAll %>%
  mutate(Facility = regmatches(Station, regexpr("\\b(CVP|SWP)\\b", Station))) %>%
  left_join(Species %>%
              distinct(OrganismCode = Salvage_Code,
                       Taxa, CommonName) %>%
              filter(!grepl("Age", Taxa),
                     !is.na(OrganismCode)),
            by = "Taxa") %>%
  filter(between(as.Date(Date), as.Date("2007-10-01"), as.Date("2023-03-21")),
         OrganismCode %in% as.numeric(species)) %>%
  group_by(OrganismCode, CommonName, Facility) %>%
  summarise(Acrefeet = sum(Tow_volume, na.rm = T),
            Salvage = sum(Count, na.rm = T)) %>%
  mutate(dataset = "db")

# Plotting the differences
library(patchwork)
{websiteSummary %>%
    rename(salvageWeb = Salvage) %>%
    select(-c(dataset, Acrefeet)) %>%
    full_join(accessSummary %>%
                ungroup() %>%
                rename(salvageAccess = Salvage) %>%
                select(-c(dataset, Acrefeet, CommonName)) %>%
                left_join(speciesData, by = "OrganismCode") %>%
                rename(CommonName = names),
              by = c("OrganismCode", "CommonName", "Facility")) %>%
    mutate(accessWebDifference = salvageAccess - salvageWeb) %>%
    pivot_longer(c(salvageWeb, salvageAccess), names_to = "source", values_to = "expandedSalvage") %>%
    filter(!is.na(Facility)) %>%
    ggplot(aes(tidytext::reorder_within(OrganismCode, -expandedSalvage, Facility),
               expandedSalvage, fill = source)) +
    geom_col(position = position_dodge()) +
    tidytext::scale_x_reordered() +
    facet_wrap(~Facility, scales = "free_x")}/
  {
    websiteSummary %>%
      rename(salvageWeb = Salvage) %>%
      select(-c(dataset, Acrefeet)) %>%
      full_join(accessSummary %>%
                  ungroup() %>%
                  rename(salvageAccess = Salvage) %>%
                  select(-c(dataset, Acrefeet, CommonName)) %>%
                  left_join(speciesData, by = "OrganismCode") %>%
                  rename(CommonName = names),
                by = c("OrganismCode", "CommonName", "Facility")) %>%
      mutate(accessWebDifference = salvageAccess - salvageWeb) %>%
      pivot_longer(c(salvageWeb, salvageAccess), names_to = "source", values_to = "expandedSalvage") %>%
      filter(!is.na(Facility)) %>%
      ggplot(aes(tidytext::reorder_within(OrganismCode, -expandedSalvage, Facility),
                 accessWebDifference, fill = source)) +
      geom_col(position = position_dodge()) +
      tidytext::scale_x_reordered() +
      facet_wrap(~Facility, scales = "free_x")
  }

websiteSummary %>%
  rename(salvageWeb = Salvage) %>%
  select(-c(dataset, Acrefeet)) %>%
  full_join(accessSummary %>%
              ungroup() %>%
              rename(salvageAccess = Salvage) %>%
              select(-c(dataset, Acrefeet, CommonName)) %>%
              left_join(speciesData, by = "OrganismCode") %>%
              rename(CommonName = names),
            by = c("OrganismCode", "CommonName", "Facility")) %>%
  mutate(accessWebDifference = salvageAccess - salvageWeb) %>%
  View("totalDifference")

# Function to explore INDIVIDUAL species between the two datasets at a time
compare_data <- function(organismCode, salvage, startDate, endDate) {

  startDate <- as.Date(startDate)
  endDate <- as.Date(endDate)

  # AcreFeet not important here as there will be duplications
  website <- websiteCompare[[as.character(organismCode)]] %>%
    filter(between(SampleDate, startDate, endDate)) %>%
    group_by(OrganismCode = Species, CommonName = OrganismName, Facility) %>%
    summarise(Acrefeet = sum(Acrefeet, na.rm = T),
              salvageWeb = sum(Salvage, na.rm = T), .groups = "drop") %>%
    select(-c(CommonName, Acrefeet))

  db <- salvage %>%
    filter(between(as.Date(Date), startDate, endDate),
           OrganismCode %in% as.numeric(organismCode)) %>%
    group_by(OrganismCode, CommonName, Facility) %>%
    summarise(Acrefeet = sum(Tow_volume/1233.48, na.rm = T),
              salvageDatabase = sum(Count, na.rm = T), .groups = "drop") %>%
    select(-c(Acrefeet))

  website %>%
    full_join(db, by = c("OrganismCode", "Facility")) %>%
    data.frame()
}

compare_data(8, SalvageJoined %>% mutate(Date = SampleDate),
             startDate = "2007-10-01", endDate = "2023-03-21")

# sdf
compareOriginal <- function(organismCode, startDate) {

  website <- websiteCompare[[as.character(organismCode)]] %>%
    select(Date = SampleDate, OrganismCode = Species,
           Facility, salvageWebsite = Salvage, acreFeetWebsite = Acrefeet) %>%
    arrange(Date, Facility)

  db <- Salvage %>%
    mutate(Facility = regmatches(Station, regexpr("\\b(CVP|SWP)\\b", Station))) %>%
    left_join(Species %>%
                distinct(OrganismCode = Salvage_Code,
                         Taxa, CommonName) %>%
                filter(!grepl("Age", Taxa),
                       !is.na(OrganismCode)),
              by = "Taxa") %>%
    filter(Date >= as.Date(startDate),
           OrganismCode %in% as.numeric(organismCode)) %>%
    transmute(Date, OrganismCode, Facility,
              salvageDatabase = Count,
              acreFeetDatabase = Tow_volume, SampleID) %>%
    group_by(Date, Facility) %>%
    mutate(dailySalvage = sum(salvageDatabase, na.rm = T))

  website %>%
    mutate(Facility = regmatches(Station, regexpr("\\b(CVP|SWP)\\b", Station))) %>%
    left_join(Species %>%
                distinct(OrganismCode = Salvage_Code,
                         Taxa, CommonName) %>%
                filter(!grepl("Age", Taxa),
                       !is.na(OrganismCode)),
              by = "Taxa") %>%
    full_join(db, by = c("Date", "OrganismCode", "Facility")) %>%
    data.frame() %>%
    relocate(contains("salvage"), .after = last_col()) %>%
    mutate(diff = dailySalvage - salvageWebsite,
           same = ifelse(salvageWebsite == dailySalvage, T, F))
}

compareOriginal(8, "2007-10-01") %>%
  # filter(Facility == "CVP") %>%
  View("comp")

