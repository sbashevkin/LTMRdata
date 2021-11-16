library(LTMRdata)


#######################################################################################
## STN

test_that("Data dimensions are correct", {
  expect_true(nrow(STN) == 183718)
  expect_true(ncol(STN) == 22)

	name_check <- c('Source', 'Station', 'Latitude', 'Longitude',
	                'Date', 'Datetime', 'Survey', 'TowNum', 'Depth',
	                'SampleID', 'Method', 'Tide', 'Sal_surf', 'Temp_surf',
	                'Secchi', 'Tow_volume', 'Tow_direction', 'Cable_length',
	                'Taxa', 'Length', 'Length_NA_flag', 'Count')
  expect_true(all(names(STN) == name_check))
})

test_that("Source value is correct", {
  expect_true(all(STN$Source == "STN"))
})

test_that("Station values are in the expected range", {
	stations <- c(9,20,21,22,23,24,32,35,36,301,302,312,315,320,322,323,326,
	              328,329,334,335,336,340,342,405,411,418,501,504,508,513,519,
	              520,602,606,609,610,704,706,707,711,713,716,719,721,722,723,795,
	              796,797,801,804,809,812,815,902,906,910,912,914,915,918,919)
  expect_setequal(STN$Station, as.character(stations))
})

test_that("Survey numbers are in the expected range", {
	expect_true(all(STN$Survey %in% 1:6))
})

test_that("Tow numbers are in the expected range", {
	expect_true(all(STN$TowNum %in% 1:4))
})

test_that("Depth values are in the expected range", {
	expect_true(all( (STN$Depth > 0 & STN$Depth < 30) | is.na(STN$Depth) ))
})

test_that("Custom SampleID values are formatted as expected", {
	expect_true(all(grepl("STN [0-9]+",STN$SampleID)))
})

test_that("Data are based on STN Net", {
  expect_true(all(STN$Method == "STN Net"))
})

test_that("Temp_surf values are in the expected range", {
  expect_true(all( (STN$Temp_surf > 0 & STN$Temp_surf < 40) |
                     is.na(STN$Temp_surf) ))
})

test_that("Secchi values are in the expected range", {
  expect_true(all( (STN$Secchi > 0 & STN$Secchi < 310) | is.na(STN$Secchi) ))
})

test_that("Tow_volume values are in the expected range", {
  expect_true(all( (STN$Tow_volume > 0 & STN$Tow_volume < 20000) |
                     is.na(STN$Tow_volume) ))
})

test_that("Cable_length values are in the expected range", {
  expect_true(all( (STN$Cable_length > 0 & STN$Cable_length < 310) |
										is.na(STN$Cable_length) ))
})

test_that("Length values are in the expected range", {
  expect_true(all( (STN$Length >= 0 & STN$Length < 800) | is.na(STN$Length) ))
})

test_that("Count values are in the expected range", {
  expect_true(all( (STN$Count >= 0 & STN$Count < 35000) | is.na(STN$Count) ))
})

test_that("Combinations of Taxa, Count, and Length_NA_flag are as expected", {
	len_flag_values <- c("No fish caught","Unknown length")
  expect_true(all(STN$Length_NA_flag %in% len_flag_values | is.na(STN$Length_NA_flag)))

	## If Length_NA_flag is NA, Count and Taxa should be present:
	sub_1 <- subset(STN, is.na(Length_NA_flag))
  expect_true(sum(is.na(sub_1$Count)) == 0)
  expect_true(sum(is.na(sub_1$Taxa)) == 0)

	## If Count is present, Taxa should be present:
	sub_2 <- subset(STN, !is.na(Count))
  expect_true(sum(is.na(sub_2$Taxa)) == 0)

	## If Count is present, check Length_NA_flag:
	sub_3 <- subset(STN, !is.na(Count))
  expect_true(all( (sub_3$Length_NA_flag %in% len_flag_values[1:2]) |
											is.na(sub_3$Length_NA_flag) ))

	## If Length_NA_flag is not missing, it should have one of the three values:
	sub_4 <- subset(STN, !is.na(Length_NA_flag))
	expect_true(all(sub_4$Length_NA_flag %in% len_flag_values))
})

#######################################################################################
## STN_measured_lengths

test_that("Lengths are in the expected range", {
	expect_true(all( (STN_measured_lengths$Length > 0 & STN$Length < 900) |
										is.na(STN_measured_lengths$Length) ))
})
