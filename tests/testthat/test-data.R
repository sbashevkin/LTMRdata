data<-bind_rows(LTMRdata::Baystudy, LTMRdata::Suisun, LTMRdata::FMWT, LTMRdata::DJFMP, LTMRdata::EDSM)

test_that("All Lats are between 37 and 39 and all Longs are between -123 and -121", {
  expect_true(all((data$Latitude<39.3 & data$Latitude>37) | is.na(data$Latitude)))
  expect_true(all((data$Longitude<(-121) & data$Longitude>(-123)) | is.na(data$Longitude)))
})
