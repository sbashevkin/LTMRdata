#' FMWT dataset
#'
#' California Department of Fish and Wildlife Fall Midwater Trawl data.
#'
#' @format a tibble with 220,785 rows and 35 variables
#' \describe{
#'   \item{Tow_volume}{Volume towed (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^{3}}}).}
#'   }
#' @details More metadata and information on methods are available \href{https://www.dfg.ca.gov/delta/projects.asp?ProjectID=FMWT}{here}.
"FMWT"

#' Suisun Marsh dataset
#'
#' UC Davis Suisun Marsh Fish Study data.
#'
#' @format a tibble with 181,406 rows and 26 variables
#' \describe{
#'   \item{Column}{Description.}
#'   }
"Suisun"

#' Bay study dataset
#'
#' California Department of Fish and Wildlife Bay Study data.
#'
#' @format a tibble with 743,409 rows and 35 variables
#' \describe{
#'   \item{Tow_volume}{Volume towed (\ifelse{html}{\out{m<sup>3</sup>}}{\eqn{m^{3}}}).}
#'   \item{Tow_area}{Area towed (\ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^{2}}}).}
#'   \item{Latitude}{May not be accurate, see details.}
#'   \item{Longitude}{May not be accurate, see details.}
#'   }
#'   @details Some station locations have moved over time due to shoals filling in, trees, etc.
"Baystudy"
