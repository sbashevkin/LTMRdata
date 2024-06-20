require(dplyr)
require(LTMRdata)
require(dtplyr)
data_dir<-tempdir()
# data_dir<-file.path("data-raw", "EDI", "data_objects")
cache_dir<-"LTMRdata-test"

# First collect some stats on the raw mashed together data

data_raw<-bind_rows(LTMRdata::Baystudy, LTMRdata::Suisun, LTMRdata::FMWT, LTMRdata::DJFMP, LTMRdata::EDSM, LTMRdata::TMM, LTMRdata::SLS, LTMRdata::STN, LTMRdata::SKT, LTMRdata::Salvage)%>%
  lazy_dt()%>%
  group_by(Source)%>%
  summarise(N=n(),
            N_0=length(which(Count==0)),
            N_lengths=length(which(Length>0 & !is.na(Taxa))),
            Samples=list(unique(SampleID)),
            Fish=list(sort(unique(Taxa))),
            .groups="drop")%>%
  as_tibble()%>%
  arrange(Source)

raw_samples<-unique(unlist(data_raw$Samples))

gc()

data_integrate(data_dir)

gc()
deltafish:::create_fish_db_f(data_dir=data_dir, cache_dir, edi_pid="edi.1075.2", update=T)
gc()

fish<-deltafish:::open_fish_f(cache_dir)%>%
  select(SampleID, Length, Count, Taxa, Notes_catch)%>%
  collect()
surv<-deltafish:::open_survey_f(cache_dir)%>%
  select(SampleID, Source)%>%
  collect()

data_integrated_surveys<-surv%>%
  lazy_dt()%>%
  left_join(fish%>%
              lazy_dt(), by="SampleID")%>%
  group_by(Source)%>%
  summarise(N=n(),
            N_0=sum(as.integer(Count==0), na.rm = TRUE),
            N_lengths=length(which(Length>0)),
            N_length_NA=sum(as.integer(is.na(Length)), na.rm=TRUE),
            .groups="drop")%>%
  as_tibble()%>%
  arrange(Source)

gc()

integrated_samples<-select(surv, SampleID)%>%
  unlist()%>%
  unique()

gc()

integrated_fishlength<-fish%>%
  lazy_dt()%>%
  mutate(ID=paste(SampleID, Length, Count, Taxa, Notes_catch))%>%
  as_tibble()%>%
  pull(ID)%>%
  unique()

gc()

integrated_fish_rows<-fish%>%
  summarise(N=n())%>%
  unlist()

names(integrated_fish_rows)<-NULL

gc()

integrated_surv_rows<-surv%>%
  summarise(N=n())%>%
  collect()%>%
  unlist()

names(integrated_surv_rows)<-NULL

gc()

data_integrated_samples<-surv%>%
  lazy_dt()%>%
  distinct(SampleID, Source)%>%
  left_join(fish%>%
              lazy_dt()%>%
              distinct(SampleID, Taxa),
            by="SampleID")%>%
  group_by(Source, SampleID)%>%
  summarise(Fish=list(sort(unique(Taxa))), .groups="drop")%>%
  as_tibble()%>%
  distinct(Source, Fish)%>%
  arrange(Source)

gc()

test_that("zero_fill output has more rows and 0 counts than input data", {
  expect_true(all(data_integrated_surveys$N>data_raw$N))
  expect_true(all(data_integrated_surveys$N_0>data_raw$N_0))
})

test_that("All samples are retained when zeroes are filled", {
  expect_setequal(raw_samples, integrated_samples)
})

test_that("SampleID is not replicated in the survey table", {
  expect_equal(length(raw_samples), integrated_surv_rows)
})

test_that("No fish-length combinations are replicated in the fish table", {
  expect_equal(length(integrated_fishlength), integrated_fish_rows)
})

test_that("Length data are not lost", {
  expect_equal(data_integrated_surveys$N_lengths, data_raw$N_lengths)
})

test_that("For each survey, the zero-filled dataset has a record (whether 0 or >0) in every sample for every species ever recorded by that survey", {
  expect_equal(data_integrated_samples, select(data_raw, Source, Fish))
})

test_that("No lengths are <= 0", {
  expect_equal(data_integrated_surveys$N_lengths + data_integrated_surveys$N_length_NA, data_integrated_surveys$N)
})

test_that("Some (but not all) lengths are NA ", {
  expect_true(all(data_integrated_surveys$N_length_NA > 0))
  expect_true(all(data_integrated_surveys$N > data_integrated_surveys$N_length_NA))
})

# Remove the cache at the end
rm(fish, surv)
gc()
deltafish:::clear_cache_f(cache_dir)
