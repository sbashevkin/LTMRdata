pull_salvage_cdfw <- function(organismCode = NULL,
                              facility = NULL,
                              adipose = NULL,
                              startDate = NULL,
                              endDate = NULL,
                              url = "https://apps.wildlife.ca.gov/Salvage/Chart/AcrefeetSalvage") {

  # Due to site set up, need to build a session to continue easily...
  # Initial link has end date BEFORE start date to allow faster loading

  if (any(is.null(organismCode), is.null(facility), is.null(adipose), is.null(startDate))) {
    startURL <- "https://apps.wildlife.ca.gov/Salvage/Chart/AcrefeetSalvage?Adipose=1&adiposeDes=Hatchery%20or%20Adipose%20Clipped&SampMethod=Both&orgCode=1&orgDes=Chinook%20Salmon%20-%20all%20races&endDate=01%2F01%2F1993%2000%3A00%3A00&StartDate=01%2F02%2F1993%2000%3A00%3A00&ShowValue=False"

    urlSession <- rvest::html_session(startURL)
    urlForm <- rvest::html_form(urlSession)[[2]]

    if (is.null(organismCode)) {
      message("The organism code options are: \n")
      return(print(urlForm$fields$OrganismCode$options))
    }

    if (is.null(facility)) {
      message("The facility options are: \n")
      return(print(urlForm$fields$SampleMethod$options))
    }

    if (organismCode %in% c("1", "Winter", "Spring", "Fall", "LateFall", "Unidentified") & is.null(adipose)) {
      message("The adipose options are: \n")
      return(print(urlForm$fields$Adipose$options))
    }

    if (is.null(startDate)) {
      stop("Provide a start date in mm/dd/YYYY format.")
    }
  }

  if (is.null(endDate)) endDate <- format(Sys.time(), format = "%m/%d/%Y")
  # The website will bog down when asking for too much data at once, generally > 2 years. Therefore, will split the data
  # request into 2 year intervals if greater than so

  startingDate <- as.Date(startDate, format = "%m/%d/%Y")
  endingDate <- as.Date(endDate, format = "%m/%d/%Y")

  if (as.numeric(endingDate - startingDate) > 365*2) {

    splitYears <- function(startDate, endDate, by = 2) {
      years <- seq(as.Date(startDate), as.Date(endDate), by = paste0(by, " years"))

      df <- data.frame(startDate = format(c(years[1], years[2:length(years)] + 1), format = "%m/%d/%Y"),
                       endDate = format(c(years[2:length(years)], as.Date(endDate)), format = "%m/%d/%Y"))
      df <- na.omit(df)

      split(df, 1:nrow(df))
    }

    yearSplit <- splitYears(startingDate, endingDate)
  } else {
    yearSplit <- list(data.frame(startDate = startDate, endDate = endDate))
  }

  # Now download the data
  df <- lapply(yearSplit, function(x) {

    finURL <- paste0(url, "?Adipose=", adipose, "&SampMethod=", facility, "&orgCode=", organismCode, "&endDate=", URLencode(x$endDate, reserved = T), "%2000%3A00%3A00&StartDate=", URLencode(x$startDate, reserved = T), "%2000%3A00%3A00&ShowValue=False")

    # The hidden dsecriptions aren't needed to get the data; it does make for a confusing URL though
    # There aren't any options for the fill form, so its part of the script that don't have to do with the table

    finWebpage <- rvest::read_html(finURL)

    df <- rvest::html_table(finWebpage, header = T)[[1]]
    names(df)[which(names(df) == "Sample Date")] <- "SampleDate"
    if (organismCode == "98") {
      names(df)[which(names(df) == "Total Fish Salvaged")] <- "Salvage"
    }

    df$SampleDate <- as.Date(df$SampleDate, format = "%m/%d/%Y")
    df$Salvage <- suppressWarnings(as.numeric(df$Salvage))

    form <- html_form(finWebpage)[[2]]
    organismName <- data.frame(name = names(form$fields$OrganismCode$options),
                               organsimCode = as.character(form$fields$OrganismCode$options))

    df$OrganismName <- organismName$name[which(organismName$organsimCode == organismCode)]

    df
  })

  df <- as.data.frame(do.call(rbind, df))

  if (is.na(sum(df$Salvage))) {
    warning("NA introduced in Salvage column represents ", dQuote("NS"), ", no sampling.", call. = F)
  }
  df
}


salvageCDFW_allWild <- pull_salvage_cdfw(organismCode = "1", adipose = "0", facility = "Both",
                                         startDate = "01/01/1993")


pull_salvage_cdfw("Winter", "Both", "All", startDate = "01/01/2022")
pull_salvage_cdfw("25", "2", startDate = "01/01/2018")
pull_salvage_cdfw("25")


salvageCompare <- Salvage %>%
  filter(SampleDate >= as.Date("2022-10-01"),
         OrganismCode == 25) %>%
  group_by(SampleDate, Facility, OrganismCode, AcreFeet) %>%
  summarise(Salvage = sum(ExpandedSalvage, na.rm = T))

# Salvage %>%
#   mutate(CommonName = factor(CommonName)) %>%
#   group_by(SampleDate, Facility) %>%
#   complete(CommonName) %>%
#   filter(SampleDate >= as.Date("2022-10-01")) %>%
#   group_by(SampleDate, Facility, OrganismCode, AcreFeet) %>%
#   summarise(Salvage = sum(ExpandedSalvage, na.rm = T)) %>%
#   View()
#
#
# Salvage %>%
#   group_by(SampleDate, Facility) %>%
#   summarise(AcreFeet = mean(AcreFeet, na.rm = T)) %>%
#   left_join(Salvage %>%
#               filter(OrganismCode == 25) %>%
#               group_by(SampleDate, Facility) %>%
#               summarise(ExpandedSalvage = sum(ExpandedSalvage, na.rm = T))) %>%
#   mutate(ExpandedSalvage = ifelse(is.na(ExpandedSalvage), 0, ExpandedSalvage)) %>%
#   View()

species <- unique(as.character(pull_salvage_cdfw()))[which(!unique(as.character(pull_salvage_cdfw())) == "98")]

websiteCompare <- lapply(species, function(x) {
  cat("Pulling", x, "\n")
  pull_salvage_cdfw(x, facility = "Both", adipose = "All", startDate = "01/01/1993", endDate = "03/21/2023")
}) %>%
  setNames(species)

# saveRDS(websiteCompare, "salvageWebsite_1993.RData")

what <- websiteCompare %>%
  bind_rows() %>%
  filter(between(SampleDate, as.Date("2007-10-01"), as.Date("2023-03-21"))) %>%
  group_by(OrganismCode = Species, CommonName = OrganismName, Facility) %>%
  summarise(Acrefeet = sum(Acrefeet, na.rm = T),
            Salvage = sum(Salvage, na.rm = T), .groups = "drop") %>%
  mutate(dataset = "website")

what2 <- SalvageCount %>%
  filter(between(SampleDate, as.Date("2007-10-01"), as.Date("2023-03-21")),
         OrganismCode %in% as.numeric(species)) %>%
  group_by(OrganismCode, CommonName, Facility) %>%
  summarise(Acrefeet = sum(AcreFeet, na.rm = T),
            Salvage = sum(ExpandedSalvage, na.rm = T)) %>%
  mutate(dataset = "db")

library(patchwork)
{what %>%
  rename(salvageWeb = Salvage) %>%
  select(-c(dataset, Acrefeet)) %>%
  full_join(what2 %>%
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
    what %>%
      rename(salvageWeb = Salvage) %>%
      select(-c(dataset, Acrefeet)) %>%
      full_join(what2 %>%
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

what %>%
  rename(salvageWeb = Salvage) %>%
  select(-c(dataset, Acrefeet)) %>%
  full_join(what2 %>%
              ungroup() %>%
              rename(salvageAccess = Salvage) %>%
              select(-c(dataset, Acrefeet, CommonName)) %>%
              left_join(speciesData, by = "OrganismCode") %>%
              rename(CommonName = names),
            by = c("OrganismCode", "CommonName", "Facility")) %>%
  mutate(accessWebDifference = salvageAccess - salvageWeb) %>%
  View("totalDifference")


compare_data <- function(organismCode, salvage, startDate, endDate) {

  startDate <- as.Date(startDate)
  endDate <- as.Date(endDate)

  website <- websiteCompare[[organismCode]] %>%
    filter(between(SampleDate, startDate, endDate)) %>%
    group_by(OrganismCode = Species, CommonName = OrganismName, Facility) %>%
    summarise(Acrefeet = sum(Acrefeet, na.rm = T),
              salvageWeb = sum(Salvage, na.rm = T), .groups = "drop") %>%
    select(-c(Acrefeet, CommonName))

  db <- salvage %>%
    filter(between(SampleDate, startDate, endDate),
           OrganismCode %in% as.numeric(organismCode)) %>%
    group_by(OrganismCode, CommonName, Facility) %>%
    summarise(Acrefeet = sum(AcreFeet, na.rm = T),
              salvageDatabase = sum(ExpandedSalvage, na.rm = T), .groups = "drop") %>%
    select(-c(Acrefeet, CommonName))

  website %>%
    full_join(db, by = c("OrganismCode", "Facility")) %>%
    data.frame()
}

# compare_data("69", "2022-10-01")

what4 <- lapply(species[c(1, 7:75)], compare_data,
                startDate = "1993-01-01", endDate = "2023-03-21",
                salvage = SalvageCount) %>%
  setNames(species[c(1, 7:75)])

speciesData <- data.frame(names = names(pull_salvage_cdfw()),
                          OrganismCode = as.integer(pull_salvage_cdfw())) %>%
  na.omit()

what4 %>%
  bind_rows() %>%
  left_join(speciesData, by = "OrganismCode") %>%
  arrange(salvageWeb) %>%
  mutate(diff = ifelse(salvageWeb != salvageDatabase, T, F),
         actualDiff = salvageWeb - salvageDatabase) %>%
  View("totalDiff")

compareOriginal <- function(organismCode, startDate) {

  website <- websiteCompare[[organismCode]] %>%
    select(SampleDate, OrganismCode = Species, Facility, salvageWebsite = Salvage, acreFeetWebsite = Acrefeet) %>%
    arrange(SampleDate, Facility)

  db <- Salvage %>%
    filter(SampleDate >= as.Date(startDate),
           OrganismCode %in% as.numeric(organismCode)) %>%
    transmute(SampleDate, OrganismCode, Facility,
              salvageDatabase = ExpandedSalvage,
              acreFeetDatabase = AcreFeet, CatchRowID) %>%
    group_by(SampleDate, Facility) %>%
    mutate(dailySalvage = sum(salvageDatabase, na.rm = T))

  website %>%
    full_join(db, by = c("SampleDate", "OrganismCode", "Facility")) %>%
    data.frame() %>%
    relocate(contains("salvage"), .after = last_col()) %>%
    mutate(diff = dailySalvage - salvageWebsite,
           same = ifelse(salvageWebsite == dailySalvage, T, F))
}

compareOriginal("3", "2007-10-01") %>%
  # filter(Facility == "CVP") %>%
  View("comp")

Salvage %>%
  filter(SampleDate %in% as.Date("2019-09-30"), OrganismCode == 6, Facility == "CVP") %>%
  View()

Salvage %>%
  filter(SampleDate %in% as.Date("2011-08-10"), OrganismCode == 10, Facility == "SWP") %>%
  group_by(CatchRowID) %>%
  summarise(total = sum(Count)) %>%
  View()

# For loss; not 100% aligned yet
loss <- SalvageCount %>%
  filter(OrganismCode == 1) %>%
  mutate(BayPump1 = ifelse(BayPump1, 21, 0),
         BayPump2 = ifelse(BayPump2, 21, 0),
         BayPump3 = ifelse(BayPump3, 43, 0),
         BayPump4 = ifelse(BayPump4, 43, 0),
         BayPump5 = ifelse(BayPump5, 21, 0),
         TotalWidth = ifelse(Facility =="SWP", BayPump1 + BayPump2 + BayPump3 + BayPump4 + BayPump5, 84),
         # Now to calculate loss
         Encounter = ifelse(ForkLength < 101, ExpandedSalvage/(0.630 + (0.0494 * (PrimaryFlow/(PrimaryDepth * TotalWidth)))),
                            ExpandedSalvage/(0.568 + (0.0579 * (PrimaryFlow/(PrimaryDepth * TotalWidth))))),
         # unique(SampleMethod) = 1, 2
         Entrain = ifelse(Facility =="SWP", Encounter/0.25, Encounter/0.85),
         Release = ifelse(ForkLength < 101, ExpandedSalvage * 0.98, ExpandedSalvage),
         Loss = case_when(StudyRowID == "0000" ~ (Entrain - Release),
                          Facility =="SWP" & StudyRowID == "9999" ~ (ExpandedSalvage * 4.33),
                          Facility == "CVP" & StudyRowID == "9999" ~ (ExpandedSalvage * 0.569)))

loss %>%
  group_by(SampleDate, Facility) %>%
  summarise(LossAccess = sum(Loss)) %>%
  full_join(websiteCompare$`1` %>%
              select(SampleDate, Facility, Loss), by = c("SampleDate", "Facility")) %>%
  mutate(diff = LossAccess - Loss) %>%
  ggplot(aes(SampleDate, diff, color = Facility)) +
  geom_point()
