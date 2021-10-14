# Set up data -------------------------------------------------------------

sources<-c("Baystudy", "Suisun", "FMWT", "SKT", "DJFMP", "EDSM")

test_that("fish produces warning messages", {
  expect_message(unconverted <<- fish(sources=sources, convert_lengths=FALSE, zero_fill=FALSE), "NOTE: Length data are not consistent across studies")
  expect_message(converted <<- fish(sources=sources, convert_lengths=TRUE, remove_unconverted_lengths=FALSE, zero_fill=FALSE), "NOTE: Length data are not entirely consistent across studies.")
})


## Length conversions ------------------------------------------------------

converted_suisun <- fish(sources="Suisun", convert_lengths=TRUE, remove_unconverted_lengths=TRUE)%>%
  filter(!is.na(Length))%>%
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

## Zero conversion and species filtering -----------------------------------

cutoff=40
converted_cutoff <- fish(sources=sources, convert_lengths=TRUE, remove_unconverted_lengths=FALSE, size_cutoff=cutoff, zero_fill=FALSE)

species <- c("Clupea pallasii", "Morone saxatilis", "Parophrys vetulus", "Sardinops sagax")
zero_filled<-fish(sources=sources, species=species, convert_lengths=TRUE, remove_unconverted_lengths=FALSE, zero_fill=FALSE)

# Run tests ---------------------------------------------------------------

test_that("fish simply binds together dataframes when convert_lengths=FALSE", {
  expect_equal(nrow(unconverted), nrow(LTMRdata::Baystudy)+nrow(LTMRdata::FMWT)+nrow(LTMRdata::Suisun)+nrow(LTMRdata::DJFMP)+nrow(LTMRdata::EDSM)+nrow(LTMRdata::SKT))
  expect_setequal(names(unconverted), unique(c(names(LTMRdata::Baystudy), names(LTMRdata::FMWT), names(LTMRdata::Suisun), names(LTMRdata::DJFMP), names(LTMRdata::EDSM), names(LTMRdata::SKT))))
})

test_that("No lengths are 0 or negative", {
  expect_true(all(unconverted$Length>0 | is.na(unconverted$Length)))
  expect_true(all(converted$Length>0 | is.na(converted$Length)))
})


test_that("Some (but not all) lengths are NA and all Length_NA_flags are preserved", {
  expect_true(any(is.na(unconverted$Length)))
  expect_true(any(is.na(converted$Length)))
  expect_false(all(is.na(unconverted$Length)))
  expect_false(all(is.na(converted$Length)))
  expect_true(all(c("No fish caught", "Unknown length") %in% unique(unconverted$Length_NA_flag)))
  expect_true(all(c("No fish caught", "Unknown length") %in% unique(converted$Length_NA_flag)))
})

test_that("size_cutoff=TRUE produces lengths greater than size cutoff, including NAs", {
  expect_true(all(converted_cutoff$Length>=cutoff | is.na(converted_cutoff$Length)))
  expect_true(any(is.na(converted_cutoff$Length)))
})

test_that("Converting lengths does not change the number of rows or columns or the total catch", {
  expect_equal(nrow(converted), nrow(unconverted))
  expect_setequal(names(converted), names(unconverted))
  expect_equal(sum(converted$Count, na.rm=T), sum(unconverted$Count, na.rm=T))
})


test_that("FMWT, Baystudy, SKT, DJFMP, and EDSM lengths are not altered by length conversion, but Suisun lengths are", {
  expect_equal(filter(converted, Source%in%c("Baystudy", "FMWT", "SKT", "DJFMP", "EDSM")), filter(unconverted, Source%in%c("Baystudy", "FMWT", "SKT", "DJFMP", "EDSM")))
  expect_false(isTRUE(all.equal(filter(converted, Source%in%c("Suisun")), filter(unconverted, Source%in%c("Suisun")))))
})

## Length conversions ------------------------------------------------------

test_that("Converted Suisun fork lengths are longer than standard length", {
  expect_true(all(combined$Length>combined$LengthSL))
})

## Species filtering and zero fill -----------------------------------------


test_that("species filtering works correctly", {
  expect_setequal(unique(filter(zero_filled, !is.na(Taxa))$Taxa), species)
})
