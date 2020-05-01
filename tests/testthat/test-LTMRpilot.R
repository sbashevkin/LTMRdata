library(LTMRdata)
library(dplyr)
require(stringr)

test_that("LTMRpilot produces warning messages", {
  expect_message(unconverted <<- LTMRpilot(convert_lengths=FALSE), "NOTE: Length data are not consistent across studies")
  expect_message(converted <<- LTMRpilot(convert_lengths=TRUE, remove_unconverted_lengths=FALSE, measured_lengths=FALSE), "NOTE: Length data are not entirely consistent across studies.")
  expect_message(measured <<- LTMRpilot(measured_lengths=TRUE, convert_lengths=F, remove_unconverted_lengths=TRUE), "NOTE: Length data are not consistent across studies")
  })

cutoff=40
converted_cutoff <- LTMRpilot(convert_lengths=TRUE, remove_unconverted_lengths=FALSE, size_cutoff=cutoff, measured_lengths=FALSE)

test_that("LTMRpilot simply binds together dataframes when convert_lengths=FALSE", {
  expect_equal(nrow(unconverted), nrow(LTMRdata::Baystudy)+nrow(LTMRdata::FMWT)+nrow(LTMRdata::Suisun))
  expect_setequal(names(unconverted), unique(c(names(LTMRdata::Baystudy), names(LTMRdata::FMWT), names(LTMRdata::Suisun))))
})

test_that("measured_lengths=TRUE produces an output with rows but fewer than measured_lengths=FALSE", {
  expect_gt(nrow(measured), 0)
  expect_lt(nrow(measured), nrow(unconverted))
})

test_that("No lengths are 0 or negative", {
  expect_true(all(unconverted$Length>0 | is.na(unconverted$Length)))
  expect_true(all(converted$Length>0 | is.na(converted$Length)))
  expect_true(all(measured$Length>0 | is.na(measured$Length)))
})

test_that("Some (but not all) lengths are NA and all Length_NA_flags are preserved", {
  expect_true(any(is.na(unconverted$Length)))
  expect_true(any(is.na(converted$Length)))
  expect_false(all(is.na(unconverted$Length)))
  expect_false(all(is.na(converted$Length)))
  expect_false(all(is.na(measured$Length)))
  expect_true(all(c("No fish caught", "Unknown length") %in% unique(unconverted$Length_NA_flag)))
  expect_true(all(c("No fish caught", "Unknown length") %in% unique(converted$Length_NA_flag)))
})

test_that("size_cutoff=TRUE produces lengths greater than size cutoff, including NAs", {
  expect_true(all(converted_cutoff$Length>=cutoff | is.na(converted_cutoff$Length)))
  expect_true(any(is.na(converted_cutoff$Length)))
})

test_that("measured_lengths=TRUE results in a lower overall catch", {
  expect_lt(sum(measured$Count, na.rm=T), sum(unconverted$Count, na.rm=T))
})

test_that("Converting lengths does not change the number of rows or columns or the total catch", {
  expect_equal(nrow(converted), nrow(unconverted))
  expect_setequal(names(converted), names(unconverted))
  expect_equal(sum(converted$Count, na.rm=T), sum(unconverted$Count, na.rm=T))
})

test_that("FMWT and Baystudy lengths are not altered by length conversion, but Suisun lengths are", {
  expect_equal(filter(converted, Source%in%c("FMWT", "Baystudy")), filter(unconverted, Source%in%c("FMWT", "Baystudy")))
  expect_false(isTRUE(all.equal(filter(converted, Source%in%c("Suisun")), filter(unconverted, Source%in%c("Suisun")))))
})

converted_suisun <- LTMRpilot(convert_lengths=TRUE, remove_unconverted_lengths=TRUE, measured_lengths=FALSE)%>%
  filter(Source=="Suisun" & !is.na(Length))%>%
  group_by(SampleID, Taxa)%>%
  filter(Length==min(Length))%>%
  ungroup()


unconverted_suisun <- unconverted%>%
  mutate(Species=str_remove(Taxa, " \\((.*)"))%>%
  filter(Source=="Suisun" & !is.na(Length) & Species%in%unique(LTMRdata::Length_conversions$Species))%>%
  group_by(SampleID, Taxa)%>%
  filter(Length==min(Length))%>%
  ungroup()%>%
  rename(LengthSL=Length)

combined<-left_join(converted_suisun, unconverted_suisun)

test_that("Converted Suisun fork lengths are longer than standard length", {
  expect_true(all(combined$Length>combined$LengthSL))
})
