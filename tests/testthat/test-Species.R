require(dplyr)
species_duplicates<-LTMRdata::Species%>%
  summarise(across(contains("_Code"), ~any(duplicated(.x[!is.na(.x)]))))

test_that("Species dataset has no duplications for Baystudy", {
  expect_false(species_duplicates$Baystudy_Code)
})

test_that("Species dataset has no duplications for USFWS", {
  expect_false(species_duplicates$USFWS_Code)
})

test_that("Species dataset has no duplications for SKT", {
  expect_false(species_duplicates$SKT_Code)
})

test_that("Species dataset has no duplications for STN", {
  expect_false(species_duplicates$STN_Code)
})

test_that("Species dataset has no duplications for Suisun Marsh Fish Study", {
  expect_false(species_duplicates$SMF_Code)
})

test_that("Species dataset has no duplications for FMWT", {
  expect_false(species_duplicates$FMWT_Code)
})

test_that("Species dataset has no duplications for 20mm", {
  expect_false(species_duplicates$TMM_Code)
})

test_that("Species dataset has no duplications for SLS", {
  expect_false(species_duplicates$SLS_Code)
})

test_that("Species dataset has no duplications for Salvage", {
  expect_false(species_duplicates$Salvage_Code)
})
