require(dplyr)
data<-bind_rows(LTMRdata::Baystudy, LTMRdata::Suisun, LTMRdata::FMWT, LTMRdata::DJFMP, LTMRdata::EDSM, LTMRdata::TMM, LTMRdata::SLS, LTMRdata::STN, LTMRdata::SKT)

test_that("All Lats are between 37 and 39 and all Longs are between -123 and -121", {
  expect_true(all((data$Latitude<39.3 & data$Latitude>37) | is.na(data$Latitude)))
  expect_true(all((data$Longitude<(-121) & data$Longitude>(-123)) | is.na(data$Longitude)))
})

test_that("Sample dates are formatted correctly", {
  expect_true(all(class(data$Date) %in% c("POSIXct","POSIXt")))
  expect_true(all(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}",as.character(data$Date))))
})

test_that("Sample times are formatted correctly", {
  expect_true(all(class(data$Datetime) %in% c("POSIXct","POSIXt")))
  datetime_format <- "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}"
  expect_true(all(grepl(datetime_format,as.character(data$Datetime)) |
                    is.na(data$Datetime)))
})

test_that("Tide has the expected value", {
  expect_true(all(data$Tide %in% c("Low Slack","Ebb","High Slack","Flood") |
                    is.na(data$Tide)))
})

test_that("Length_NA_flag has correct unique values", {
  expect_setequal(unique(data$Length_NA_flag), c(NA_character_, "Unknown length", "No fish caught"))
})

test_that("Length_NA_flag 'No fish caught' is applied correctly", {
  expect_setequal(filter(data, is.na(Length) & Count==0)$Length_NA_flag, "No fish caught") # 'No fish caught' should only be applied to cases where Length and Count are both NA
  expect_equal(nrow(filter(data, !(is.na(Length) & Count==0) & Length_NA_flag=="No fish caught")), 0) # 'No fish caught' should only be applied to cases where Length and Count are both NA
  expect_equal(nrow(filter(data, !is.na(Taxa) & Length_NA_flag=="No fish caught")), 0) # Taxa should be NA for all 'No fish caught'
  expect_equal(nrow(filter(data, is.na(Taxa) & Length_NA_flag!="No fish caught")), 0) # NA taxa rows should only exist when 'No fish caught'
})

test_that("Length_NA_flag 'Unknown length' is applied correctly", {
  expect_setequal(filter(data, is.na(Length) & Count>0)$Length_NA_flag, "Unknown length") # 'Unknown length' should only be applied when length is NA and Count>0
  expect_equal(nrow(filter(data, !(is.na(Length) & Count>0) & Length_NA_flag=="Unknown length")), 0) # 'Unknown length' should only be applied when length is NA and Count>0

})

test_that("No zero counts exist in the dataset, except for instances of 'No fish caught'", {
  expect_equal(filter(data, Count==0), filter(data, Length_NA_flag=="No fish caught"))
})

test_that("No NA counts exist in the datast", {
  expect_equal(nrow(filter(data, is.na(Count))), 0)
})

test_that("No Tow volumes or Tow areas are negative", {
  expect_equal(nrow(filter(data, Tow_volume<0)), 0)
  expect_equal(nrow(filter(data, Tow_area<0)), 0)
})
