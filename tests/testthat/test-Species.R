require(dplyr)
species_duplicates<-LTMRdata::Species%>%
  summarise(across(contains("_Code"), ~any(duplicated(.x[!is.na(.x)]))))
test_that("Species dataset has no duplications", {
  expect_false(all(species_duplicates))
})
