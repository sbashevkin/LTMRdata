#' Bay study dataset
#'
#' California Department of Fish and Wildlife Bay Study data.
#'
#' @encoding UTF-8
#' @format a tibble with 805,858 rows and 23 variables
#' \describe{
#'   \item{Source}{Name of source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{May not be accurate, see details.}
#'   \item{Longitude}{May not be accurate, see details.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time sample was collected.}
#'   \item{Survey}{Survey number, roughly corresponding to month.}
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
#'   \item{Taxa}{Scientific name.}
#'   \item{Length}{Fork length, total length if there's no fork or heterocercal tail (sturgeon, shark). Bat Ray wing widths starting around 1989-1992 to present. (mm).}
#'   \item{Count}{Estimated count for each sample, taxa, and length.}
#'   \item{Length_NA_flag}{Why is the length NA?}
#'   \item{Notes_tow}{Notes or comments on the trawl.}
#'   }
#'   @details Some station locations have moved over time due to shoals filling in, trees, etc.
"Baystudy"

#' Baystudy measured lengths
#'
#' Only measured lengths from the Baystudy data
#' @format a tibble with 802,128 rows and 5 variables
#' \describe{
#'   \item{SampleID}{Unique sample identifier.}
#'   \item{Taxa}{Scientific name.}
#'   \item{Size_group}{When two different size groups of a species are collected, Bay study may split them into size groups and subsample from each size group separately for fish to measure.}
#'   \item{Length}{Fork length, total length if there's no fork or heterocercal tail (sturgeon, shark). Bat Ray wing widths starting around 1989-1992 to present. (mm).}
#'   \item{Count}{Number of fish measured to this length.}
#'   }
"Baystudy_measured_lengths"

#' DJFMP dataset
#'
#' US Fish and Wildlife Service Delta Juvenile Fish Monitoring Program data.
#'
#' @encoding UTF-8
#' @format a tibble with 2,248,153 rows and 19 variables
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
#'   \item{Sal_surf}{Surface salinity.}
#'   \item{Temp_surf}{Surface temperature in °C.}
#'   \item{TurbidityNTU}{Turbidity (NTU).}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Tow_volume}{Volume towed (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^{3}}}).}
#'   \item{Tow_direction}{Tow direction relative to current.}
#'   \item{Taxa}{Scientific name.}
#'   \item{Length}{Fork length from tip of the snout to a point at the fork of the caudal fin (mm).}
#'   \item{Count}{Estimated count for each sample, taxa, and length.}
#'   \item{Length_NA_flag}{Why is the length NA?}
#'   }
#' @details Some station locations have moved over time due to shoals filling in, trees, etc. More metadata and information on methods are available \href{https://portal.edirepository.org/nis/mapbrowse?packageid=edi.244.7}{here}.
"DJFMP"

#' EDSM dataset
#'
#' US Fish and Wildlife Service Enhanced Delta Smelt Monitoring (EDSM) data.
#'
#' @encoding UTF-8
#' @format a tibble with 190,057 rows and 20 number of variables
#' \describe{
#'   \item{Source}{Name of source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Average of start and end Latitude.}
#'   \item{Longitude}{Average of start and end Longitude.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time sample was collected.}
#'   \item{Depth}{Bottom depth (m). Start depth as noted in survey.}
#'   \item{SampleID}{Unique sample identifier.}
#'   \item{Method}{Sampling method (Otter Trawl or Midwater Trawl).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Sal_surf}{Surface salinity.}
#'   \item{Temp_surf}{Surface temperature in °C.}
#'   \item{TurbidityNTU}{Turbidity (NTU).}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Tow_volume}{Volume towed (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^{3}}}).}
#'   \item{Tow_direction}{Tow direction relative to current.}
#'   \item{Taxa}{Scientific name.}
#'   \item{Length}{Fish fork length from point of mouth to fork of the caudal fin.}
#'   \item{Count}{Estimated count for each sample, taxa, and length.}
#'   \item{Length_NA_flag}{Why is the length NA?}
#'   }
#'   @details Stations change randomly due to random stratified sampling. More metadata and information on methods are available \href{https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=415&revision=7}{here}.
"EDSM"

#' FMWT dataset
#'
#' California Department of Fish and Wildlife Fall Midwater Trawl data.
#'
#' @encoding UTF-8
#' @format a tibble with 266,694 rows and 23 variables
#' \describe{
#'   \item{Source}{Name of source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude (decimal degrees).}
#'   \item{Longitude}{Longitude (decimal degrees).}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time sample was collected.}
#'   \item{Survey}{Survey number, roughly corresponding to month.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{SampleID}{Unique sample identifier.}
#'   \item{Method}{Sampling method (Otter Trawl or Midwater Trawl).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Sal_surf}{Surface salinity.}
#'   \item{Temp_surf}{Surface temperature in °C.}
#'   \item{TurbidityNTU}{Turbidity (NTU).}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Secchi_estimated}{Was Secchi depth estimated?}
#'   \item{Tow_volume}{Volume towed (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^{3}}}).}
#'   \item{Tow_direction}{Tow direction relative to current.}
#'   \item{Cable_length}{Length of cable released when net deployed (m).}
#'   \item{Taxa}{Scientific name.}
#'   \item{Length}{Fork length from tip of the snout to a point at the fork of the caudal fin (mm).}
#'   \item{Count}{Estimated count for each sample, taxa, and length.}
#'   \item{Length_NA_flag}{Why is the length NA?}
#'   }
#' @details More metadata and information on methods are available \href{https://www.dfg.ca.gov/delta/projects.asp?ProjectID=FMWT}{here}.
"FMWT"

#' FMWT measured lengths
#'
#' Only measured lengths from the FMWT data
#' @format a tibble with 233,902 rows and 4 variables
#' \describe{
#'   \item{SampleID}{Unique sample identifier.}
#'   \item{Taxa}{Scientific name.}
#'   \item{Length}{Sampling survey (loosely corresponds to month).}
#'   \item{Count}{Number of fish measured to this length.}
#'   }
"FMWT_measured_lengths"

#' Length conversions
#'
#' Intercepts and slopes for equations to convert Standard Length to Fork Length (or Total Length if no fork). Equations derived from the CDFW length-weight study (2005) and Jereme Gaeta, unpublished.
#'
#' @format a tibble with 20 rows and 3 variables
#' \describe{
#'   \item{Species}{Scientific name.}
#'   \item{Intercept}{Intercept in the equation FL = Intercept + Slope * SL.}
#'   \item{Slope}{Slope in the equation FL = Intercept + Slope * SL.}
#'   }
"Length_conversions"

#' SKT dataset
#'
#' California Department of Fish and Wildlife Spring Kodiac Trawl (SKT) data.
#'
#' @encoding UTF-8
#' @format a tibble with 42,909 rows and 20 variables
#' \describe{
#'   \item{Source}{Name of source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude of Station}
#'   \item{Longitude}{Longitude of Station}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time sample was collected.}
#'   \item{Survey}{Survey number, roughly corresponding to month.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{SampleID}{Unique sample identifier.}
#'   \item{Method}{Sampling method (Otter Trawl or Midwater Trawl).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Sal_surf}{Surface salinity.}
#'   \item{Temp_surf}{Surface temperature in °C.}
#'   \item{TurbidityNTU}{Turbidity (NTU).}
#'   \item{TurbidityFNU}{Turbidity (FNU).}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Tow_volume}{Volume towed (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^{3}}}).}
#'   \item{Tow_direction}{Tow direction relative to current.}
#'   \item{Taxa}{Scientific name.}
#'   \item{Length}{Fork length, total length if there's no fork or heterocercal tail (sturgeon, shark).}
#'   \item{Count}{Estimated count for each sample, taxa, and length.}
#'   \item{Length_NA_flag}{Why is the length NA?}
#'   }
#' @details More metadata and information on methods are available \href{https://wildlife.ca.gov/Conservation/Delta/Spring-Kodiak-Trawl}{here}.
"SKT"

#' SKT measured lengths
#'
#' Only measured lengths from the SKT data
#'
#' @encoding UTF-8
#' @format a tibble with 41,295 rows and 4 variables
#' \describe{
#'   \item{SampleID}{Unique sample identifier.}
#'   \item{Taxa}{Scientific name.}
#'   \item{Length}{Fork length, total length if there's no fork or heterocercal tail (sturgeon, shark).}
#'   \item{Count}{Estimated count for each sample, taxa, and length.}
#'   }
"SKT_measured_lengths"

#' SLS dataset
#'
#' California Department of Fish and Wildlife Smelt Larva Survey data.
#'
#' @encoding UTF-8
#' @format a tibble with 24,826 rows and 26 variables
#' \describe{
#'   \item{Source}{Name of source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude (decimal degrees).}
#'   \item{Longitude}{Longitude (decimal degrees).}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time sample was collected.}
#'   \item{Survey}{Survey number, roughly corresponding to month.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{SampleID}{Unique sample identifier.}
#'   \item{Method}{Sampling method.}
#'   \item{Tide}{Tidal stage.}
#'   \item{Sal_surf}{Surface salinity.}
#'   \item{Sal_bot}{Bottom salinity.}
#'   \item{Temp_surf}{Surface temperature in °C.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{TurbidityNTU}{Turbidity (NTU).}
#'   \item{TurbidityFNU}{Turbidity (FNU).}
#'   \item{Tow_volume}{Volume towed (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^{3}}}).}
#'   \item{Cable_length}{Length of cable released when net deployed (m).}
#'   \item{Tow_duration}{Duration of tow (minutes).}
#'   \item{Taxa}{Scientific name.}
#'   \item{Length}{Fork length (mm).}
#'   \item{Count}{Estimated count for each sample, taxa, and length.}
#'   \item{Length_NA_flag}{Why is the length NA?}
#'   \item{Notes_tow}{Notes or comments on the trawl.}
#'   \item{Notes_flowmeter}{Notes or comments on the flowmeter reading.}
#'   }
#' @details More metadata and information on methods are available \href{https://wildlife.ca.gov/Conservation/Delta/Smelt-Larva-Survey}{here}.
"SLS"

#' Species codes
#'
#' Crosswalk table of species codes to common and scientific names
#'
#' @format a tibble with 287 rows and 13 variables
#' \describe{
#'   \item{ScientificName}{Scientific Name.}
#'   \item{Baystudy_Code}{Bay Study code.}
#'   \item{CommonName}{Common name.}
#'   \item{USFWS_Code}{EDSM and DJFMP codes.}
#'   \item{SKT_Code}{Spring Kodiak Trawl code.}
#'   \item{STN_Code}{Summer Townet Survey code.}
#'   \item{SMF_Code}{Suisun Marsh Fish Study code.}
#'   \item{FMWT_Code}{Fall Midwater Trawl code.}
#'   \item{TMM_Code}{20-mm Survey code.}
#'   \item{SLS_Code}{SLS Survey code.}
#'   \item{Salvage_Code}{Salvage Survey code.}
#'   \item{Lifestage}{Lifestage if specified.}
#'   \item{Taxa}{Scientific name and life stage.}
#'   }
"Species"

#' STN dataset
#'
#' California Department of Fish and Wildlife Summer Townet Survey (STN) data.
#'
#' @encoding UTF-8
#' @format a tibble with 191,547 rows and 22 variables
#' \describe{
#'   \item{Source}{Name of source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude (decimal degrees).}
#'   \item{Longitude}{Longitude (decimal degrees).}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time sample was collected.}
#'   \item{Survey}{Survey number, roughly corresponding to month.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{SampleID}{Unique sample identifier.}
#'   \item{Method}{Sampling method (STN trawl).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Sal_surf}{Surface salinity.}
#'   \item{Temp_surf}{Surface temperature in °C.}
#'   \item{TurbidityNTU}{Turbidity (NTU).}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Tow_volume}{Volume towed (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^{3}}}).}
#'   \item{Tow_direction}{Tow direction relative to current.}
#'   \item{Cable_length}{Length of cable released when net deployed (feet).}
#'   \item{Taxa}{Scientific name.}
#'   \item{Length}{Fork length from the most anterior part of the fish to the median caudal fin rays (mm).}
#'   \item{Count}{Estimated count for each sample, taxa, and length.}
#'   \item{Length_NA_flag}{Why is the length NA?}
#'   }
#' @details More metadata and information on methods are available \href{https://wildlife.ca.gov/Conservation/Delta/Townet-Survey}{here}.
"STN"

#' STN measured lengths
#'
#' Only measured lengths from the STN data
#' @format a tibble with 173,981 rows and 4 variables
#' \describe{
#'   \item{SampleID}{Unique sample identifier.}
#'   \item{Taxa}{Scientific name.}
#'   \item{Length}{Fork length from the most anterior part of the fish to the median caudal fin rays (mm).}
#'   \item{Count}{Number of fish measured to this length.}
#'   }
"STN_measured_lengths"

#' Suisun Marsh dataset
#'
#' UC Davis Suisun Marsh Fish Study data.
#'
#' @encoding UTF-8
#' @format a tibble with 202,676 rows and 21 variables
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
#'   \item{Taxa}{Scientific name.}
#'   \item{Length}{Standard length for all species except sturgeon, which are fork length (mm).}
#'   \item{Count}{Estimated count for each sample, taxa, and length.}
#'   \item{Length_NA_flag}{Why is the length NA?}
#'   \item{Notes_catch}{Notes or comments on the fish catch.}
#'   \item{Notes_tow}{Notes or comments on the trawl.}
#'   }
"Suisun"

#' Suisun measured lengths
#'
#' Only measured lengths from the Suisun data
#' @format a tibble with 173,209 rows and 5 variables
#' \describe{
#'   \item{SampleID}{Unique sample identifier.}
#'   \item{Taxa}{Scientific name.}
#'   \item{Dead}{Was fish dead?}
#'   \item{Length}{Standard length for all species except sturgeon, which are fork length (mm).}
#'   \item{Count}{Number of fish measured to this length.}
#'   }
"Suisun_measured_lengths"

#' 20mm dataset
#'
#' California Department of Fish and Wildlife 20-mm Survey data.
#'
#' @encoding UTF-8
#' @format a tibble with 355,228 rows and 23 variables
#' \describe{
#'   \item{Source}{Name of source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude (decimal degrees).}
#'   \item{Longitude}{Longitude (decimal degrees).}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time sample was collected.}
#'   \item{Survey}{Survey number, roughly corresponding to month.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{SampleID}{Unique sample identifier.}
#'   \item{Method}{Sampling method (20mm Net).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Sal_surf}{Surface salinity.}
#'   \item{Temp_surf}{Surface temperature in °C.}
#'   \item{TurbidityNTU}{Turbidity (NTU).}
#'   \item{TurbidityFNU}{Turbidity (FNU).}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Tow_volume}{Volume towed (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^{3}}}).}
#'   \item{Tow_direction}{Tow direction relative to current. Not recorded but field is included here for consistency.}
#'   \item{Cable_length}{Length of cable released when net deployed (m).}
#'   \item{Taxa}{Scientific name.}
#'   \item{Length}{Fork length from tip of the snout to a point at the fork of the caudal fin (mm).}
#'   \item{Count}{Estimated count for each sample, taxa, and length.}
#'   \item{Length_NA_flag}{Why is the length NA?}
#'   }
#' @details More metadata and information on methods are available \href{https://wildlife.ca.gov/Conservation/Delta/20mm-Survey}{here}.
"TMM"

#' 20mm measured lengths
#'
#' Only measured lengths from the 20-mm Survey data
#' @format a tibble with 351,699 rows and 4 variables
#' \describe{
#'   \item{SampleID}{Unique sample identifier.}
#'   \item{Taxa}{Scientific name.}
#'   \item{Length}{Fork length from tip of the snout to a point at the fork of the caudal fin (mm).}
#'   \item{Count}{Number of fish measured to this length.}
#'   }
"TMM_measured_lengths"

#' Salvage dataset
#'
#' California Department of Fish and Wildlife Fish Salvage dataset.
#'
#' @encoding UTF-8
#' @format a tibble with 2,596,933 rows and 15 variables
#' \describe{
#'   \item{Source}{Name of source dataset.}
#'   \item{Station}{State or federal facility buildings (of the SWP or CVP).}
#'   \item{Latitude}{Latitude (decimal degrees) of the full facility.}
#'   \item{Longitude}{Longitude (decimal degrees) of the full facility.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time sample was collected.}
#'   \item{SampleID}{Unique sample identifier.}
#'   \item{Method}{Sampling period (0000 = Normal count, 9999 = Second flush)}
#'   \item{Tow_volume}{Daily export volume for the sampled facility(\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^{3}}}).}
#'   \item{Temp_surf}{Surface temperature in °C.}
#'   \item{Taxa}{Scientific name.}
#'   \item{Length}{Fork length from tip of the snout to a point at the fork of the caudal fin (mm) or total length for species without a forked tail.}
#'   \item{Count}{Estimated count for each sample, taxa, and length.}
#'   \item{Notes_tow}{Comments or notes of the sampling period.}
#'   \item{Length_NA_flag}{Why is the length NA?}
#'   }
#' @details More metadata and information on methods are available \href{https://wildlife.ca.gov/Conservation/Delta/Salvage-Monitoring}{here}.
"Salvage"

#' Salvage measured lengths
#'
#' Only measured lengths from the Salvage data
#' @format a tibble with 1,782,210 rows and 4 variables
#' \describe{
#'   \item{SampleID}{Unique sample identifier.}
#'   \item{Taxa}{Scientific name.}
#'   \item{Length}{Fork length from tip of the snout to a point at the fork of the caudal fin (mm) or total length for species without a forked tail.}
#'   \item{Count}{Number of fish measured to this length.}
#'   }
"Salvage_measured_lengths"
