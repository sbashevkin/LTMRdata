#' FMWT dataset
#'
#' California Department of Fish and Wildlife Fall Midwater Trawl data.
#'
#' @encoding UTF-8
#' @format a tibble with 228,639 rows and 21 variables
#' \describe{
#'   \item{Source}{Name of source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude (decimal degrees).}
#'   \item{Longitude}{Longitude (decimal degrees).}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time sample was collected.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{SampleID}{Unique sample identifier.}
#'   \item{Method}{Sampling method (Otter Trawl or Midwater Trawl).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Sal_surf}{Surface salinity.}
#'   \item{Temp_surf}{Surface temperature in °C.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Secchi_estimated}{Was Secchi depth estimted?}
#'   \item{Tow_volume}{Volume towed (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^{3}}}).}
#'   \item{Tow_direction}{Tow direction relative to current.}
#'   \item{Cable_length}{Length of cable released when net deployed (feet?).}
#'   \item{Taxa}{Scientific name and lifestage (if applicable).}
#'   \item{Length}{Fork length from tip of the snout to a point at the fork of the caudal fin (mm).}
#'   \item{Count}{Estimated count for each sample, taxa, and length.}
#'   \item{Length_NA_flag}{Why is the length NA?}
#'   }
#' @details More metadata and information on methods are available \href{https://www.dfg.ca.gov/delta/projects.asp?ProjectID=FMWT}{here}.
"FMWT"

#' FMWT measured lengths
#'
#' Only measued lengths from the FMWT data
#' @format a tibble with 220,785 rows and 5 variables
#' \describe{
#'   \item{SampleID}{Unique sample identifier.}
#'   \item{Taxa}{Scientific name and lifestage (if applicable).}
#'   \item{Dead}{Was fish dead?}
#'   \item{Length}{Sampling survey (loosely corresponds to month).}
#'   \item{Count}{Number of fish measured to this length.}
#'   }
"FMWT_measured_lengths"

#' Suisun Marsh dataset
#'
#' UC Davis Suisun Marsh Fish Study data.
#'
#' @encoding UTF-8
#' @format a tibble with 180,320 rows and 21 variables
#' \describe{
#'   \item{Source}{Name of source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude (decimal degrees).}
#'   \item{Longitude}{Longitude (decimal degrees).}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time sample was collected.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{SampleID}{Unique sample identifier.}
#'   \item{Method}{Sampling method (Otter Trawl or Midwater Trawl).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Sal_surf}{Surface salinity.}
#'   \item{Temp_surf}{Surface temperature in °C.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Tow_duration}{Duration of tow (minutes).}
#'   \item{Tow_area}{Area towed (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^{2}}}).}
#'   \item{Taxa}{Scientific name and lifestage (if applicable).}
#'   \item{Length}{Standard length for all species except sturgeon, which are fork length (mm).}
#'   \item{Count}{Estimated count for each sample, taxa, and length.}
#'   \item{Length_NA_flag}{Why is the length NA?}
#'   \item{Notes_catch}{Notes or comments on the fish catch.}
#'   \item{Notes_tow}{Notes or comments on the trawl.}
#'   }
"Suisun"

#' Suisun measured lengths
#'
#' Only measued lengths from the Suisun data
#' @format a tibble with 155,447 rows and 5 variables
#' \describe{
#'   \item{SampleID}{Unique sample identifier.}
#'   \item{Taxa}{Scientific name and lifestage (if applicable).}
#'   \item{Dead}{Was fish dead?}
#'   \item{Length}{Standard length for all species except sturgeon, which are fork length (mm).}
#'   \item{Count}{Number of fish measured to this length.}
#'   }
"Suisun_measured_lengths"

#' Bay study dataset
#'
#' California Department of Fish and Wildlife Bay Study data.
#'
#' @encoding UTF-8
#' @format a tibble with 746,485 rows and 23 variables
#' \describe{
#'   \item{Source}{Name of source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{May not be accurate, see details.}
#'   \item{Longitude}{May not be accurate, see details.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time sample was collected.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{SampleID}{Unique sample identifier.}
#'   \item{Method}{Sampling method (Otter Trawl or Midwater Trawl).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Sal_surf}{Surface salinity.}
#'   \item{Temp_surf}{Surface temperature in °C.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Tow_duration}{Duration of tow (minutes).}
#'   \item{Tow_area}{Area towed (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^{2}}}).}
#'   \item{Tow_volume}{Volume towed (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^{3}}}).}
#'   \item{Tow_direction}{Tow direction relative to current.}
#'   \item{Taxa}{Scientific name and lifestage (if applicable).}
#'   \item{Length}{Fork length, total length if there's no fork or heterocercal tail (sturgeon, shark). Bat Ray wing widths starting around 1989-1992 to present. (mm).}
#'   \item{Count}{Estimated count for each sample, taxa, and length.}
#'   \item{Length_NA_flag}{Why is the length NA?}
#'   \item{Notes_tow}{Notes or comments on the trawl.}
#'   }
#'   @details Some station locations have moved over time due to shoals filling in, trees, etc.
"Baystudy"

#' Baystudy measured lengths
#'
#' Only measued lengths from the Baystudy data
#' @format a tibble with 743,295 rows and 5 variables
#' \describe{
#'   \item{SampleID}{Unique sample identifier.}
#'   \item{Taxa}{Scientific name and lifestage (if applicable).}
#'   \item{Size_group}{When two different size groups of a species are collected, Bay study may split them into size groups and subsample from each size group separately for fish to measure.}
#'   \item{Length}{Fork length, total length if there's no fork or heterocercal tail (sturgeon, shark). Bat Ray wing widths starting around 1989-1992 to present. (mm).}
#'   \item{Count}{Number of fish measured to this length.}
#'   }
"Baystudy_measured_lengths"

#' Species codes
#'
#' Crosswalk table of species codes to common and scientific names
#'
#' @format a tibble with 299 rows and 7 variables
#' \describe{
#'   \item{Baystudy_Code}{Bay Study code.}
#'   \item{CommonName}{Common name.}
#'   \item{SMF_Code}{Suisun Marsh Fish Study code.}
#'   \item{FMWT_Code}{Fall Midwater Trawl code.}
#'   \item{ScientificName}{Scientific name.}
#'   \item{Lifestage}{Lifestage if specified.}
#'   \item{Taxa}{Combination of scientific name and life stage.}
#'   }
"Species"

#' Length conversions
#'
#' Intercepts and slopes for equations to convert Standard Length to Fork Length (or Total Length if no fork).
#'
#' @format a tibble with 20 rows and 3 variables
#' \describe{
#'   \item{ScientificName}{Scientific name.}
#'   \item{Intercept}{Intercept in the equation FL = Intercept + Slope * SL.}
#'   \item{Slope}{Slope in the equation FL = Intercept + Slope * SL.}
#'   }
"Length_conversions"
