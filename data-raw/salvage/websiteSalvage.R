# This script pulls data from the CDFW salvage website

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

websiteCompare <- lapply(species, function(x) {
  cat("Pulling", x, "\n")
  pull_salvage_cdfw(x, facility = "Both", adipose = "All", startDate = "01/01/1993", endDate = "03/21/2023")
}) %>%
  setNames(species)

# Pulling names from the server as they are slightly different from the db
species <- unique(as.character(pull_salvage_cdfw()))[which(!unique(as.character(pull_salvage_cdfw())) == "98")]

speciesData <- data.frame(names = names(pull_salvage_cdfw()),
                          OrganismCode = as.integer(pull_salvage_cdfw())) %>%
  na.omit()

# Saving file for the markdown document
save(pull_salvage_cdfw, species, speciesData, websiteCompare, file = "salvageWebsite_1993.RData")
