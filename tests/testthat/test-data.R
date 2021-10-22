data<-bind_rows(LTMRdata::Baystudy, LTMRdata::Suisun, LTMRdata::FMWT, LTMRdata::DJFMP, LTMRdata::EDSM, LTMRdata::TMM)

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
