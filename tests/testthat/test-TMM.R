library(LTMRdata)


#######################################################################################
## TMM

test_that("Source value is correct", {
  expect_true(all(TMM$Source == "20mm"))
})

test_that("Survey numbers are in the expected range", {
	expect_true(all(TMM$Survey %in% 1:12))
})

test_that("Tow numbers are in the expected range", {
	expect_true(all(TMM$TowNum %in% 1:3))
})

test_that("Depth values are in the expected range", {
	expect_true(all( (TMM$Depth > 0 & TMM$Depth < 27) | is.na(TMM$Depth) ))
})

test_that("Custom SampleID values are formatted as expected", {
	expect_true(all(grepl("20mm [0-9]+",TMM$SampleID)))
})

test_that("Data are based on 20mm Net only and not mesozooplankton (CB) net", {
  expect_true(all(TMM$Method == "20mm net"))
})

test_that("Temp_surf values are in the expected range", {
  expect_true(all( (TMM$Temp_surf > 0 & TMM$Temp_surf < 30) | is.na(TMM$Temp_surf) ))
})

test_that("Secchi values are in the expected range", {
  expect_true(all( (TMM$Secchi > 0 & TMM$Secchi < 215) | is.na(TMM$Secchi) ))
})

test_that("Tow_volume values are in the expected range", {
  expect_true(all( (TMM$Tow_volume > 0 & TMM$Tow_volume < 1550) | is.na(TMM$Tow_volume) ))
})

test_that("Cable_length values are in the expected range", {
  expect_true(all( (TMM$Cable_length > 0 & TMM$Cable_length < 230) |
										is.na(TMM$Cable_length) ))
})

test_that("Length values are in the expected range", {
  expect_true(all( (TMM$Length > 0 & TMM$Length < 900) | is.na(TMM$Length) ))
})

test_that("Count values are in the expected range", {
  expect_true(all( (TMM$Count >= 0 & TMM$Count < 4600) | is.na(TMM$Count) ))
})

test_that("Some adjusted fish counts are as expected", {
	dsm <- subset(TMM, Taxa == "Hypomesus transpacificus")
	expect_true(nrow(dsm) >= 15625)
	expect_true(sum(dsm$Count) >= 27166)

	lfs <- subset(TMM, Taxa == "Spirinchus thaleichthys")
	expect_true(nrow(lfs) >= 59116)
	expect_true(sum(lfs$Count) >= 566224)

	chn <- subset(TMM, Taxa == "Oncorhynchus tshawytscha")
	expect_true(nrow(chn) >= 382)
	expect_true(sum(chn$Count) >= 386)
})


#######################################################################################
## TMM_measured_lengths

test_that("Lengths are in the expected range", {
	expect_true(all( (TMM_measured_lengths$Length > 0 & TMM_measured_lengths$Length < 900) |
										is.na(TMM_measured_lengths$Length) ))
})

test_that("Some total measured fish counts are as expected", {
	dsm_len <- subset(TMM_measured_lengths, Taxa == "Hypomesus transpacificus")
	expect_true(sum(dsm_len$Count) >= 27144)

	lfs_len <- subset(TMM_measured_lengths, Taxa == "Spirinchus thaleichthys")
	expect_true(sum(lfs_len$Count, na.rm=TRUE) >= 188679)

	chn_len <- subset(TMM_measured_lengths, Taxa == "Oncorhynchus tshawytscha")
	expect_true(sum(chn_len$Count) >= 386)
})

