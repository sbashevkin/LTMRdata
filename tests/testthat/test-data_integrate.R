require(dplyr)
require(LTMRdata)

integrate<-TRUE

if(integrate){
  data_dir<-tempdir()

}else{
  data_dir<-file.path("publication", "data_objects")
}

cache_dir<-"LTMRdata-test"

# First collect some stats on the raw mashed together data

data_raw<-bind_rows(LTMRdata::Baystudy, LTMRdata::Suisun, LTMRdata::FMWT, LTMRdata::DJFMP, LTMRdata::EDSM, LTMRdata::TMM, LTMRdata::SLS, LTMRdata::STN, LTMRdata::SKT, LTMRdata::Salvage)%>%
  group_by(Source)%>%
  summarise(N=n(),
            N_0=length(which(Count==0)),
            N_lengths=length(which(Length>0 & !is.na(Taxa))),
            Samples=list(unique(SampleID)),
            Fish=list(sort(unique(Taxa[which(Taxa!="UnID")]))),
            .groups="drop")%>%
  arrange(Source)

raw_samples<-unique(unlist(data_raw$Samples))

gc()

if(integrate){
  data_integrate(data_dir)
  gc()
}

deltafish:::create_fish_db_f(data_dir=data_dir, cache_dir, edi_pid="edi.1075.2", update=T)

con <- deltafish:::open_database_f(cache_dir)

fish<-deltafish::open_fish(con)%>%
  select(SampleID, Length, Count, Taxa, Notes_catch)

surv<-deltafish::open_survey(con)%>%
  select(SampleID, Source)

data_integrated_surveys<-fish%>%
  left_join(surv, by="SampleID")%>%
  group_by(Source)%>%
  summarise(N=n(),
            N_0=sum(as.integer(Count==0), na.rm = TRUE),
            N_lengths=sum(as.integer(Length>0), na.rm = TRUE),
            N_length_NA=sum(as.integer(is.na(Length)), na.rm=TRUE),
            .groups="drop")%>%
  collect()%>%
  arrange(Source)

integrated_samples<-pull(surv, SampleID)%>%
  unique()

integrated_fishlength<-fish%>%
  mutate(ID=paste(SampleID, Length, Count, Taxa, Notes_catch))%>%
  pull(ID)

integrated_fish_rows<-fish%>%
  summarise(N=n())%>%
  pull(N)

integrated_surv_rows<-surv%>%
  summarise(N=n())%>%
  pull(N)

data_integrated_samples<-fish%>%
  filter(Taxa!="UnID")%>%
  left_join(surv, by="SampleID")%>%
  distinct(SampleID, Source, Taxa)%>%
  collect()%>%
  group_by(Source, SampleID)%>%
  summarise(Fish=list(sort(unique(Taxa))), .groups="drop")%>%
  distinct(Source, Fish)%>%
  arrange(Source)

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
deltafish::close_database(con)
gc()
deltafish:::clear_cache_f(cache_dir)
