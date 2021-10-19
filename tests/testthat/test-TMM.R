library(LTMRdata)


#######################################################################################
## TMM

test_that("Data dimensions are correct", {
  expect_true(nrow(TMM) == 324218)
  expect_true(ncol(TMM) == 22)
	
	name_check <- c("Source","Station","Latitude","Longitude","Date","Datetime",
									"Survey","TowNum","Depth","SampleID","Method","Tide","Sal_surf",
									"Temp_surf","Secchi","Tow_volume","Tow_direction","Cable_length",
									"Taxa","Length","Count","Length_NA_flag")
  expect_true(all(names(TMM) == name_check))
})

test_that("Source value is correct", {
  expect_true(all(TMM$Source == "20mm"))
})

test_that("Station values are in the expected range", {
	stations <- c(323,328:330,334:336,340:349,405,411,418,501,504,508,513,519,520,
								602,606,609,610,703:707,711,716,718:720,723,724,726,794:799,801,802,
								804,809,812,815,901,902,906,910,912,914,915,918,919,997,998,999)
  expect_true(all(TMM$Station %in% stations))
})

test_that("All Lats are between 37 ad 39 and all Longs are between -123 and -121", {
	expect_true(all((TMM$Latitude < 39 & TMM$Latitude > 37) | is.na(TMM$Latitude)))
	expect_true(all((TMM$Longitude < (-121) & TMM$Latitude > (-123)) | is.na(TMM$Longitude)))
})

test_that("", {
	expect_true(all((TMM$Latitude < 39 & TMM$Latitude > 37) | is.na(TMM$Latitude)))
	expect_true(all((TMM$Longitude < (-121) & TMM$Latitude > (-123)) | is.na(TMM$Longitude)))
})

test_that("Sample dates are formatted correctly", {
	expect_true(all(class(TMM$Date) %in% c("POSIXct","POSIXt")))
	expect_true(all(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}",as.character(TMM$Date))))
})

test_that("Sample times are formatted correctly", {
	expect_true(all(class(TMM$Datetime) %in% c("POSIXct","POSIXt")))
	datetime_format <- "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}"
	expect_true(all(grepl(datetime_format,as.character(TMM$Datetime)) | 
									is.na(TMM$Datetime)))
})

test_that("Survey numbers are in the expected range", {
	expect_true(all(TMM$Survey %in% 1:12))
})

test_that("Tow numbers are in the expected range", {
	expect_true(all(TMM$TowNum %in% 1:3))
})

test_that("Depth values are in the expected range", {
	expect_true(all( (TMM$Depth > 0 & TMM$Depth < 25) | is.na(TMM$Depth) ))
})

test_that("Custom SampleID values are formatted as expected", {
	expect_true(all(grepl("20mm [0-9]+",TMM$SampleID)))
})

test_that("Data are based on 20mm Net only and not mesozooplankton (CB) net", {
  expect_true(all(TMM$Method == "20mm Net"))
})

test_that("Tide has the expected value", {
  expect_true(all(TMM$Tide %in% c("Low Slack","Ebb","High Slack","Flood") | 
							is.na(TMM$Tide)))
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
  expect_true(all( (TMM$Count > 0 & TMM$Count < 4600) | is.na(TMM$Count) ))
})

test_that("Combinations of Taxa, Count, and Length_NA_flag are as expected", {
	len_flag_values <- c("No fish caught","No length measurement","Missing catch value")
  expect_true(all(TMM$Length_NA_flag %in% len_flag_values | is.na(TMM$Length_NA_flag)))
	
	## If Length_NA_flag is NA, Count and Taxa should be present:
	sub_1 <- subset(TMM, is.na(Length_NA_flag))
  expect_true(sum(is.na(sub_1$Count)) == 0)
  expect_true(sum(is.na(sub_1$Taxa)) == 0)
	
	## If Count is present, Taxa should be present:
	sub_2 <- subset(TMM, !is.na(Count))
  expect_true(sum(is.na(sub_2$Taxa)) == 0)	
	
	## If Count is present, check Length_NA_flag:
	sub_3 <- subset(TMM, !is.na(Count))
  expect_true(all( (sub_3$Length_NA_flag %in% len_flag_values[1:2]) | 
											is.na(sub_3$Length_NA_flag) ))

	## If Length_NA_flag is not missing, it should have one of the three values:
	sub_4 <- subset(TMM, !is.na(Length_NA_flag))
	expect_true(all(sub_4$Length_NA_flag %in% len_flag_values))
	expect_true(nrow(sub_4) == 3176)
	
	TMM_missing_catch <- subset(TMM, Length_NA_flag == "Missing catch value")
	expect_true(nrow(TMM_missing_catch) == 1)
	expect_true(all(TMM_missing_catch$Taxa == "Gasterosteus aculeatus"))
})

test_that("Some adjusted fish counts are as expected", {
	dsm <- subset(TMM, Taxa == "Hypomesus transpacificus")
	expect_true(nrow(dsm) == 15625)
	expect_true(sum(dsm$Count) == 27166)

	lfs <- subset(TMM, Taxa == "Spirinchus thaleichthys")
	expect_true(nrow(lfs) == 59126)
	expect_true(sum(lfs$Count) == 566235)

	chn <- subset(TMM, Taxa == "Oncorhynchus tshawytscha")
	expect_true(nrow(chn) == 382)
	expect_true(sum(chn$Count) == 386)
})


#######################################################################################
## TMM_measured_lengths

test_that("Lengths are in the expected range", {
	expect_true(all( (TMM_measured_lengths$Length > 0 & TMM$Length < 900) | 
										is.na(TMM_measured_lengths$Length) ))
})

test_that("Some total measured fish counts are as expected", {
	dsm_len <- subset(TMM_measured_lengths, Taxa == "Hypomesus transpacificus")
	expect_true(sum(dsm_len$Count) == 27144)

	lfs_len <- subset(TMM_measured_lengths, Taxa == "Spirinchus thaleichthys")
	expect_true(sum(lfs_len$Count, na.rm=TRUE) == 188690)

	chn_len <- subset(TMM_measured_lengths, Taxa == "Oncorhynchus tshawytscha")
	expect_true(sum(chn_len$Count) == 386)
})

