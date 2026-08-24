#' Run data integrity checks on a single LTMR survey dataset
#'
#' Applies the standard suite of integration tests to a dataset. This is the
#' same suite of tests as the ones in the `test` folder. Useful for catching
#' issues before binding datasets together. Salvage-specific test logic is
#' handled automatically when `source_name` is "Salvage".
#'
#' @param data A data frame — typically one of the LTMRdata survey objects
#' (e.g. LTMRdata::Baystudy).
#' @param source_name Optional character string naming the source (e.g.
#' "Baystudy"). If NULL and a `Source` column is present, the unique value(s)
#' are used. Affects test labels and Salvage-specific logic.
#' @param return_failures Logical. If FALSE (default), runs testthat assertions.
#' If TRUE, skips assertions and returns a named list containing the rows that
#' failed each check.
#'
#' @return If `return_failures = FALSE`, invisibly returns `data` and is called
#' for its side-effect of running testthat assertions. If `return_failures = TRUE`,
#' returns a named list of data frames representing failed checks (returns an empty
#' list if all checks pass).
#'
#' @examples
#' \dontrun{
#' test_dataset(LTMRdata::Baystudy)
#' fails <- test_dataset(LTMRdata::Salvage, return_failures = TRUE)
#' }

test_dataset <- function(data, source_name = NULL, return_failures = FALSE) {

  # --- Source name ---
  if (is.null(source_name) && "Source" %in% names(data)) {
    source_name <- unique(data$Source)
    if (length(source_name) > 1) {
      warning(
        "Multiple sources detected: ", paste(source_name, collapse = ", "),
        ". Salvage-specific test logic may not apply correctly."
      )
    }
  }

  is_salvage  <- isTRUE(all(source_name == "Salvage"))
  test_label  <- if (length(source_name) == 1) paste0("[", source_name, "] ") else ""

  # --- Set valid reference values ---
  valid_methods <- c(
    "Midwater trawl", "Otter trawl", "Kodiak trawl",
    "Beach seine", "20mm net", "Oblique tow",
    "STN net", "Normal count", "Second flush"
  )

  valid_tides <- c("Low Slack", "Ebb", "High Slack", "Flood")

  valid_length_flags <- c("No fish caught", "Unknown length", "Missing catch value")

  # --- Helper: prepend source label to test descriptions ---
  desc <- function(x) paste0(test_label, x)

  # --- Initialize failure collection ---
  failures <- list()

  # --- Spatial tests ---
  # Do all data points have a lat/lon? Are those lat/lon coordinates within expected area?
  fail_lat <- dplyr::filter(data, (.data$Latitude < 37 | .data$Latitude > 39.3) & !is.na(.data$Latitude))
  if (nrow(fail_lat) > 0) failures$Latitude_OutOfBounds <- fail_lat

  fail_lon <- dplyr::filter(data, (.data$Longitude < -123 | .data$Longitude > -121) & !is.na(.data$Longitude))
  if (nrow(fail_lon) > 0) failures$Longitude_OutOfBounds <- fail_lon

  if (!return_failures) {
    testthat::test_that(desc("Latitudes are between 37 and 39.3 or is NA."), {
      testthat::expect_equal(nrow(fail_lat), 0, info = "Rows failing latitude check")
    })

    testthat::test_that(desc("Longitudes are between -123 and -121 or is NA."), {
      testthat::expect_equal(nrow(fail_lon), 0, info = "Rows failing longitude check")
    })
  }

  # --- Temporal tests ---
  # Do all rows have a date and date time? Are they formatted correctly?
  pass_date_class <- all(class(data$Date) %in% c("POSIXct", "POSIXt"))
  if (!pass_date_class) failures$Date_Class_Invalid <- "Date column is not POSIXct/POSIXt"

  fail_date_fmt <- dplyr::filter(data, !grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}",
                                              as.character(.data$Date)) | is.na(.data$Date))
  if (nrow(fail_date_fmt) > 0) failures$Date_Format_Invalid <- fail_date_fmt

  pass_dt_class <- all(class(data$Datetime) %in% c("POSIXct", "POSIXt"))
  if (!pass_dt_class) failures$Datetime_Class_Invalid <- "Datetime column is not POSIXct/POSIXt"

  if (!return_failures) {
    testthat::test_that(desc("Date is POSIXct/POSIXt and matches YYYY-MM-DD format"), {
      testthat::expect_true(pass_date_class, info = "Date column is not POSIXct/POSIXt")
      testthat::expect_equal(nrow(fail_date_fmt), 0, info = "Rows failing Date format check")
    })

    testthat::test_that(desc("Datetime is POSIXct/POSIXt"), {
      testthat::expect_true(pass_dt_class, info = "Datetime column is not POSIXct/POSIXt")
    })
  }

  if (!is_salvage) {
    # Strange midnight database error; Salvage can have midnight sampling though
    fail_midnight <- dplyr::filter(data, lubridate::hour(.data$Datetime) == 0 & lubridate::minute(.data$Datetime) == 0)
    if (nrow(fail_midnight) > 0) failures$Midnight_Samples <- fail_midnight

    # --- Field categorial values ---
    # Tide data should follow pre-defined categories, if data was recorded (can be NA)
    # Salvage does not collect tide data
    fail_tide <- dplyr::filter(data, !(.data$Tide %in% valid_tides) & !is.na(.data$Tide))
    if (nrow(fail_tide) > 0) failures$Invalid_Tide <- fail_tide

    if (!return_failures) {
      testthat::test_that(desc("No sample times are exactly midnight"), {
        testthat::expect_equal(nrow(fail_midnight), 0, info = "Rows sampled exactly at midnight")
      })

      testthat::test_that(desc("Tide values are all valid"), {
        testthat::expect_equal(nrow(fail_tide), 0, info = "Rows with invalid Tide values")
      })
    }
  }

  # All rows should have a sampling method; should follow pre-defined categories
  fail_method <- dplyr::filter(data, !(.data$Method %in% valid_methods))
  if (nrow(fail_method) > 0) failures$Invalid_Method <- fail_method

  if (!return_failures) {
    testthat::test_that(desc("Method values are all valid"), {
      testthat::expect_equal(nrow(fail_method), 0, info = "Rows with invalid Method values")
    })
  }

  # --- Lengths ---
  # All lengths needs to have a taxa label; can occur if new species caught
  fail_len_taxa <- dplyr::filter(data, !is.na(.data$Length) & is.na(.data$Taxa))
  if (nrow(fail_len_taxa) > 0) failures$Length_Without_Taxa <- fail_len_taxa

  if (!return_failures) {
    testthat::test_that(desc("Lengths has a Taxa label"), {
      testthat::expect_equal(nrow(fail_len_taxa), 0, info = "Rows with Length but missing Taxa")
    })
  }

  # Length_NA_flag logic
  # All rows without a length measurement must have a flag and follow pre-defined categories
  fail_len_flag <- dplyr::filter(data, is.na(.data$Length) & !(.data$Length_NA_flag %in% valid_length_flags))
  if (nrow(fail_len_flag) > 0) failures$Invalid_Length_NA_flag <- fail_len_flag

  # Unknown length logic (ul)
  # ul flag should have a count and an NA length
  fail_ul_missing <- dplyr::filter(data, (is.na(.data$Length) & .data$Count > 0) & (is.na(.data$Length_NA_flag) | .data$Length_NA_flag != "Unknown length"))
  # If has ul flag, must have no length (but has a count)
  fail_ul_wrong <- dplyr::filter(data, .data$Length_NA_flag == "Unknown length" & !(is.na(.data$Length) & .data$Count > 0))

  if (nrow(fail_ul_missing) > 0) failures$UnknownLength_Missing <- fail_ul_missing
  if (nrow(fail_ul_wrong) > 0) failures$UnknownLength_Incorrectly_Applied <- fail_ul_wrong

  # No fish caught logic
  # nfc occurs when catch is 0 and length is NA
  fail_nfc_missing <- dplyr::filter(data, is.na(.data$Length) & .data$Count == 0 & (is.na(.data$Length_NA_flag) | .data$Length_NA_flag != "No fish caught"))
  # If there is a length, shouldn't be labeled as nfc
  fail_nfc_wrong <- dplyr::filter(data, !(is.na(.data$Length) & .data$Count == 0) & .data$Length_NA_flag == "No fish caught")
  # Taxa should be NA if there is a nfc label
  fail_nfc_taxa_not_na <- dplyr::filter(data, .data$Length_NA_flag == "No fish caught" & !is.na(.data$Taxa))
  # nfc should have an NA taxa
  fail_nfc_taxa_na <- dplyr::filter(data, is.na(.data$Taxa) & (is.na(.data$Length_NA_flag) | .data$Length_NA_flag != "No fish caught"))

  if (nrow(fail_nfc_missing) > 0) failures$NFC_Missing <- fail_nfc_missing
  if (nrow(fail_nfc_wrong) > 0) failures$NFC_Incorrectly_Applied <- fail_nfc_wrong
  if (nrow(fail_nfc_taxa_not_na) > 0) failures$NFC_With_Taxa <- fail_nfc_taxa_not_na
  if (nrow(fail_nfc_taxa_na) > 0) failures$Missing_NFC_For_NA_Taxa <- fail_nfc_taxa_na

  if (!return_failures) {
    testthat::test_that(desc("Length_NA_flag contains only valid values"), {
      testthat::expect_equal(nrow(fail_len_flag), 0, info = "Invalid Length_NA_flag values")
    })

    testthat::test_that(desc("'No fish caught' applied only when Length is NA and Count == 0"), {
      testthat::expect_equal(nrow(fail_nfc_missing), 0, info = "Missing 'No fish caught' flag")
      testthat::expect_equal(nrow(fail_nfc_wrong), 0, info = "Incorrectly applied 'No fish caught' flag")
      testthat::expect_equal(nrow(fail_nfc_taxa_not_na), 0, info = "Rows with Taxa and 'No fish caught'")
      testthat::expect_equal(nrow(fail_nfc_taxa_na), 0, info = "Rows without Taxa missing 'No fish caught'")
    })

    testthat::test_that(desc("'Unknown length' applied only when Length is NA and Count > 0"), {
      testthat::expect_equal(nrow(fail_ul_missing), 0, info = "Missing 'Unknown length' flag")
      testthat::expect_equal(nrow(fail_ul_wrong), 0, info = "Incorrectly applied 'Unknown length' flag")
    })
  }

  # --- Count / taxa coherence check ---
  # If there is no Length_NA_flag, there SHOULD be a Count
  fail_coh_no_count <- dplyr::filter(data, is.na(.data$Length_NA_flag) & is.na(.data$Count))
  if (nrow(fail_coh_no_count) > 0) failures$Coh_NoFlag_NA_Count <- fail_coh_no_count
  # If there is no Length_NA_flag, there SHOULD be a Taxa (as there should be a count)
  fail_coh_no_taxa <- dplyr::filter(data, is.na(.data$Length_NA_flag) & is.na(.data$Taxa))
  if (nrow(fail_coh_no_taxa) > 0) failures$Coh_NoFlag_NA_Taxa <- fail_coh_no_taxa

  # If Count is greater than 0, there should be a Taxa
  fail_coh_pos_no_taxa <- dplyr::filter(data, .data$Count > 0 & is.na(.data$Taxa))
  if (nrow(fail_coh_pos_no_taxa) > 0) failures$Coh_PosCount_NA_Taxa <- fail_coh_pos_no_taxa

  # If Length_NA_flag is not missing, should follow predefined categories
  fail_coh_invalid_flag <- dplyr::filter(data, !is.na(.data$Length_NA_flag) & !(.data$Length_NA_flag %in% valid_length_flags))
  if (nrow(fail_coh_invalid_flag) > 0) failures$Coh_HasCount_InvalidFlag <- fail_coh_invalid_flag

  # No zero counts exist in the dataset, except for instances of 'No fish caught'
  fail_zero_mismatch <- dplyr::filter(data,
                                      (.data$Count == 0 & !(.data$Length_NA_flag %in% "No fish caught")) |
                                        (.data$Length_NA_flag %in% "No fish caught" & !(.data$Count == 0))
  )
  if (nrow(fail_zero_mismatch) > 0) failures$ZeroCount_Mismatch <- fail_zero_mismatch

  # No NA counts can exist in the dataset
  fail_na_count <- dplyr::filter(data, is.na(.data$Count))
  if (nrow(fail_na_count) > 0) failures$NA_Count <- fail_na_count

  if (!return_failures) {
    testthat::test_that(desc("Combinations of Taxa, Count, and Length_NA_flag are coherent"), {
      testthat::expect_equal(nrow(fail_coh_no_count), 0, info = "NA Count with no Length_NA_flag")
      testthat::expect_equal(nrow(fail_coh_no_taxa), 0, info = "NA Taxa with no Length_NA_flag")
      testthat::expect_equal(nrow(fail_coh_pos_no_taxa), 0, info = "Positive Count with NA Taxa")
      testthat::expect_equal(nrow(fail_coh_invalid_flag), 0, info = "Count present but invalid Length_NA_flag")
    })

    testthat::test_that(desc("The only zero-Count rows are 'No fish caught'"), {
      testthat::expect_equal(nrow(fail_zero_mismatch), 0, info = "Mismatch between Count == 0 and 'No fish caught'")
    })

    testthat::test_that(desc("No NA counts exist"), {
      testthat::expect_equal(nrow(fail_na_count), 0, info = "Rows with NA counts")
    })
  }

  # --- Tow effort ---
  # No tow volume or area can be negative
  if("Tow_volume"%in%names(data)){
    fail_tow_vol_neg <- dplyr::filter(data, .data$Tow_volume < 0)
    if (nrow(fail_tow_vol_neg) > 0) failures$Negative_Tow_volume <- fail_tow_vol_neg
  }

  if (!is_salvage) {
    if("Tow_volume"%in%names(data)){
      # No tow volume can be 0
      # Salvage can also have 0 tow volume during secondary flushes (tow volume already accounted for)
      fail_tow_vol_zero <- dplyr::filter(data, .data$Tow_volume == 0)
      if (nrow(fail_tow_vol_zero) > 0) failures$Zero_Tow_volume <- fail_tow_vol_zero
    }

    if("Tow_area"%in%names(data)){
      # No tow area can be negative
      # Salvage does not collect tow area data
      fail_tow_area_neg <- dplyr::filter(data, .data$Tow_area < 0)
      if (nrow(fail_tow_area_neg) > 0) failures$Negative_Tow_area <- fail_tow_area_neg

      # No tow area can be 0
      fail_tow_area_zero <- dplyr::filter(data, .data$Tow_area == 0)
      if (nrow(fail_tow_area_zero) > 0) failures$Zero_Tow_area <- fail_tow_area_zero
    }
  }

  if (!return_failures) {
    testthat::test_that(desc("No Tow_volume or Tow_area values are negative"), {
      if("Tow_volume"%in%names(data)){
        testthat::expect_equal(nrow(fail_tow_vol_neg), 0, info = "Negative Tow_volume rows")
      }
      if (!is_salvage & "Tow_area"%in%names(data)) {
        testthat::expect_equal(nrow(fail_tow_area_neg), 0, info = "Negative Tow_area rows")
      }
    })

    # Salvage legitimately records zero tow volumes; skip that sub-check.
    if (!is_salvage) {
      if("Tow_area"%in%names(data)){
        testthat::test_that(desc("No Tow_area values are zero"), {
          testthat::expect_equal(nrow(fail_tow_area_zero), 0, info = "Zero Tow_area rows")
        })
      }
      if("Tow_volume"%in%names(data)){
        testthat::test_that(desc("No Tow_volume values are zero"), {
          testthat::expect_equal(nrow(fail_tow_vol_zero), 0, info = "Zero Tow_volume rows")
        })
      }
    }
  }

  # --- Water quality ---
  if (!is_salvage) {
    # Salvage does not collect salinity
    # Salinity cannot be negative
    # Salinity cannot be above 40, too high

    if("Sal_surf"%in%names(data)) {
      fail_sal_surf_neg <- dplyr::filter(data, .data$Sal_surf < 0)
      fail_sal_surf_hi <- dplyr::filter(data, .data$Sal_surf > 40)
      if (nrow(fail_sal_surf_neg) > 0) failures$Negative_Sal_surf <- fail_sal_surf_neg
      if (nrow(fail_sal_surf_hi) > 0) failures$High_Sal_surf <- fail_sal_surf_hi
    }
    if("Sal_bot"%in%names(data)) {
      fail_sal_bot_neg <- dplyr::filter(data, .data$Sal_bot < 0)
      fail_sal_bot_hi <- dplyr::filter(data, .data$Sal_bot > 40)
      if (nrow(fail_sal_bot_neg) > 0) failures$Negative_Sal_bot <- fail_sal_bot_neg
      if (nrow(fail_sal_bot_hi) > 0) failures$High_Sal_bot <- fail_sal_bot_hi
    }
  }

  # Temperature cananot be negative or too high (above 40)
  if ("Temp_surf"%in%names(data)) {
    fail_temp_surf_neg <- dplyr::filter(data, .data$Temp_surf < 0)
    fail_temp_surf_hi <- dplyr::filter(data, .data$Temp_surf > 40)
    if (nrow(fail_temp_surf_neg) > 0) failures$Negative_Temp_surf <- fail_temp_surf_neg
    if (nrow(fail_temp_surf_hi) > 0)  failures$High_Temp_surf <- fail_temp_surf_hi
  }

  if (!return_failures) {
    if (!is_salvage) {
      testthat::test_that(desc("No salinity values are negative or above 40"), {
        if ("Sal_surf"%in%names(data)) {
          testthat::expect_equal(nrow(fail_sal_surf_neg), 0, info = "Negative Sal_surf rows")
          testthat::expect_equal(nrow(fail_sal_surf_hi), 0, info = "Sal_surf > 40 rows")
        }
        if ("Sal_bot"%in%names(data)) {
          testthat::expect_equal(nrow(fail_sal_bot_neg), 0, info = "Negative Sal_bot rows")
          testthat::expect_equal(nrow(fail_sal_bot_hi), 0, info = "Sal_bot > 40 rows")
        }
      })
    }
    if ("Temp_surf"%in%names(data)) {
      testthat::test_that(desc("No surface temperature values are negative or above 40"), {
        testthat::expect_equal(nrow(fail_temp_surf_neg), 0, info = "Negative Temp_surf rows")
        testthat::expect_equal(nrow(fail_temp_surf_hi), 0, info = "Temp_surf > 40 rows")
      })
    }
  }

  # --- Return ---
  if (return_failures) {
    if (length(failures) == 0) {
      message("All checks passed for ", if (!is.null(source_name)) source_name else "dataset", ".")
    } else {
      message(length(failures), " check(s) failed: ", paste(names(failures), collapse = ", "))
    }
    return(invisible(failures))
  }

  invisible(data)
}
