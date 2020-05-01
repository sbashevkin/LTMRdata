require(LTMRdata)
require(dplyr)

Data <- LTMRpilot(convert_lengths=TRUE, remove_unconverted_lengths=TRUE)

Data_filled<- zero_fill(Data, remove_unknown_lengths=FALSE)

Data_filled_univariate <- zero_fill(Data, remove_unknown_lengths=TRUE, univariate=TRUE)

Data_filled_multivariate <- zero_fill(Data, remove_unknown_lengths=TRUE, univariate=FALSE)

test_that("zero_fill output has more rows and 0 counts than input data", {
  expect_gt(nrow(Data_filled), nrow(Data))
  expect_gt(nrow(Data_filled_univariate), nrow(Data))
  expect_gt(nrow(Data_filled_multivariate), nrow(Data))
  expect_gt(nrow(filter(Data_filled, Count==0)), nrow(filter(Data, Count==0)))
  expect_gt(nrow(filter(Data_filled_multivariate, Count==0)), nrow(filter(Data, Count==0)))
  expect_gt(nrow(filter(Data_filled_univariate, Count==0)), nrow(filter(Data, Count==0)))
})

test_that("remove_unknown_lengths=FALSE has most rows, followed by remove_unknown_lengths=TRUE & univariate=TRUE, and remove_unknown_lengths=TRUE, univariate=FALSE has the least rows", {
  expect_gt(nrow(Data_filled), nrow(Data_filled_univariate))
  expect_gt(nrow(Data_filled_univariate), nrow(Data_filled_multivariate))
})

test_that("All samples are retained in zero_fill output without removing unknown lengths", {
  expect_setequal(unique(Data_filled$SampleID), unique(Data$SampleID))
})

test_that("No unknown lengths remain after zero_fill with remove_unknown_lengths=TRUE", {
  expect_equal(nrow(filter(Data_filled_univariate, Length_NA_flag=="Unknown length")), 0)
  expect_equal(nrow(filter(Data_filled_multivariate, Length_NA_flag=="Unknown length")), 0)
})
