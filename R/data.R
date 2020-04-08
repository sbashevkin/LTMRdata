#' FMWT dataset
#'
#' California Department of Fish and Wildlife Fall Midwater Trawl data.
#'
#' @encoding UTF-8
#' @format a tibble with 228,639 rows and 21 variables
#' \describe{
#'   \item{Station}{Station where sample was collected.}
#'   \item{Method}{Sampling method (Otter Trawl or Midwater Trawl).}
#'   \item{Survey}{Sampling survey (loosely corresponds to month).}
#'   \item{Temp_surf}{Surface temperature in °C.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Secchi_estimated}{Was Secchi depth estimted?}
#'   \item{Cable_length}{Length of cable released when net deployed (feet?).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Date}{Date sample was collected.}
#'   \item{Tow_volume}{Volume towed (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^{3}}}).}
#'   \item{Datetime}{Date and time sample was collected.}
#'   \item{Tow_direction}{Tow direction relative to current.}
#'   \item{Latitude}{Latitude (decimal degrees).}
#'   \item{Longitude}{Longitude (decimal degrees).}
#'   \item{Length}{Fork length from tip of the snout to a point at the fork of the caudal fin (mm).}
#'   \item{Dead}{Was fish dead?}
#'   \item{Taxa}{Scientific name and lifestage (if applicable).}
#'   \item{Count}{Estimated count for each sample, taxa, and length.}
#'   \item{Sal_surf}{Surface salinity.}
#'   \item{Source}{Name of source dataset.}
#'   }
#' @details More metadata and information on methods are available \href{https://www.dfg.ca.gov/delta/projects.asp?ProjectID=FMWT}{here}.
"FMWT"

#' Suisun Marsh dataset
#'
#' UC Davis Suisun Marsh Fish Study data.
#'
#' @encoding UTF-8
#' @format a tibble with 155,690 rows and 19 variables
#' \describe{
#'   \item{Length}{Standard length for all species except sturgeon, which are fork length (mm).}
#'   \item{Dead}{Was fish dead?}
#'   \item{Count}{Estimated count for each sample, taxa, and length.}
#'   \item{Notes_catch}{Notes or comments on the fish catch.}
#'   \item{Method}{Sampling method (Otter Trawl or Midwater Trawl).}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Temp_surf}{Surface temperature in °C.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Datetime}{Date and time sample was collected.}
#'   \item{Source}{Name of source dataset.}
#'   \item{Longitude}{Longitude (decimal degrees).}
#'   \item{Latitude}{Latitude (decimal degrees).}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Tow_duration}{Duration of tow (minutes).}
#'   \item{Notes_tow}{Notes or comments on the trawl.}
#'   \item{Taxa}{Scientific name and lifestage (if applicable).}
#'   \item{Sal_surf}{Surface salinity.}
#'   }
"Suisun"

#' Bay study dataset
#'
#' California Department of Fish and Wildlife Bay Study data.
#'
#' @encoding UTF-8
#' @format a tibble with 746,611 rows and 22 variables
#' \describe{
#'   \item{Survey}{Sampling survey (loosely corresponds to month).}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Method}{Sampling method (Otter Trawl or Midwater Trawl).}
#'   \item{Tow_duration}{Duration of tow (minutes).}
#'   \item{Notes_tow}{Notes or comments on the trawl.}
#'   \item{Tow_direction}{Tow direction relative to current.}
#'   \item{Tow_volume}{Volume towed (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^{3}}}).}
#'   \item{Tow_area}{Area towed (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^{2}}}).}
#'   \item{Tow_status}{Validity of tow.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temp_surf}{Surface temperature in °C.}
#'   \item{Latitude}{May not be accurate, see details.}
#'   \item{Longitude}{May not be accurate, see details.}
#'   \item{Tide}{Tidal stage.}
#'   \item{Datetime}{Date and time sample was collected.}
#'   \item{Taxa}{Scientific name and lifestage (if applicable).}
#'   \item{Length}{Fork length, total length if there's no fork or heterocercal tail (sturgeon, shark). Bat Ray wing widths starting around 1989-1992 to present. (mm).}
#'   \item{Source}{Name of source dataset.}
#'   \item{Sal_surf}{Surface salinity.}
#'   \item{Count}{Estimated count for each sample, taxa, and length.}
#'   }
#'   @details Some station locations have moved over time due to shoals filling in, trees, etc.
"Baystudy"

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
