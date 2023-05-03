## code to prepare `Suisun` dataset goes here

require(readr)
require(dplyr)
require(wql)
require(lubridate)
require(LTMRdata)
require(readxl)
require(tidyr)

# Must still reach out to Teejay (taorear@ucdavis.edu) to get the Access db
unzip(file.path("data-raw", "Suisun", "SuisunMarshFish2020_1_14_23.zip"), exdir = tempdir())
db_path <- file.path(tempdir(), "SuisunMarshFish2020_1_14_23.accdb")

source(file.path("data-raw", "bridgeAccess.R"))

keepTables <- c("AgesBySizeMo", "Catch", "Depth",
                "Sample", "StationsLookUp", "TrawlEffort")

suisunMarshTables <- bridgeAccess(db_path,
                          tables = keepTables,
                          script = file.path("data-raw", "connectAccess.R"))

# # If you've chosen to read csv --------------------------------------------
# suisunMarshTables <- list()
#
# suisunMarshTables$Depth <- read_csv(file.path("data-raw", "Suisun", "Depth.csv"),
#                                     col_types=cols_only(SampleRowID="c", Depth="d"))
#
# suisunMarshTables$StationsLookUp <- read_csv(file.path("data-raw", "Suisun", "StationsLookUp.csv"),
#                                              col_types=cols_only(StationCode="c", x_WGS84="d", y_WGS84="d"))
#
# suisunMarshTables$TrawlEffort <- read_csv(file.path("data-raw", "Suisun", "TrawlEffort.csv"),
#                                           col_types = cols_only(SampleRowID="c", TowDuration="d", TrawlComments="c"))
#
# suisunMarshTables$AgesBySizeMo <- read_csv(file.path("data-raw", "Suisun", "AgesBySizeMo.csv"),
#                                            col_types = cols_only(Class="c", Month="d", Min="d", Max="d", OrganismCode="c"),
#                                            na = "N/A")
#
# suisunMarshTables$Sample <- read_csv(file.path("data-raw", "Suisun", "Sample.csv"),
#                                      col_types = cols_only(SampleRowID="c", MethodCode="c", StationCode="c",
#                                                            SampleDate="c", SampleTime="c",
#                                                            QADone="l", WaterTemperature="d", DO="d", PctSaturation="d",
#                                                            Secchi="d", SpecificConductance="d", TideCode="c"))
#
# suisunMarshTables$Catch <- read_csv(file.path("data-raw", "Suisun", "Catch.csv"), na=c("NA", "n/p"),
#                                     col_types = cols_only(SampleRowID="c", OrganismCode="c", StandardLength="d",
#                                                           Dead="c", Count="d", CatchComments="c"))

# Depth data --------------------------------------------------------------


depth_suisun <- suisunMarshTables$Depth%>%
  group_by(SampleRowID)%>%
  summarise(Depth=mean(Depth, na.rm=T), .groups="drop") # Sometimes there were multiple depths per sample


# Station locations -------------------------------------------------------


stations_suisun <- suisunMarshTables$StationsLookUp %>%
  transmute(StationCode = as.character(StationCode),
            across(c(x_WGS84, y_WGS84), as.double)) %>%
  rename(Longitude=x_WGS84, Latitude=y_WGS84, Station=StationCode)%>%
  drop_na()

# Trawl effort ------------------------------------------------------------

effort_suisun <- suisunMarshTables$TrawlEffort %>%
  transmute(SampleRowID = as.character(SampleRowID),
            TowDuration = as.double(TowDuration),
            TrawlComments = as.character(TrawlComments)) %>%
  mutate(Tow_area = (TowDuration/60)*4*1000*4.3*0.7) # ((TowDuration minutes) / (60 minutes/hour)) * 4km/hour towing speed * 1000 m/km * 4.3 m net width * 0.7 for assumption of 70% open

# Age by size info to help with decoding lengths (see below) --------------


age_size_suisun <- suisunMarshTables$AgesBySizeMo %>%
  transmute(Class = ifelse(Class == "N/A", NA, as.character(Class)),
            across(c(Month, Min, Max), as.double),
            OrganismCode = as.character(OrganismCode))

# Sample-level data -------------------------------------------------------


#Removing salinity because data do not correspond well with conductivity
sample_suisun <- suisunMarshTables$Sample %>%
  transmute(across(c(SampleRowID, MethodCode, StationCode, SampleTime), as.character),
            SampleDate = as.Date(SampleDate),
            QADone = as.logical(QADone),
            across(c(WaterTemperature, DO, PctSaturation, Secchi, SpecificConductance), as.double),
            TideCode = as.character(TideCode)) %>%
  rename(Station=StationCode, Date=SampleDate, Time=SampleTime,
         Temperature=WaterTemperature, Conductivity=SpecificConductance,
         Tide=TideCode, Method=MethodCode)%>%
  mutate(Method=recode(Method, MWTR="Midwater trawl", OTR="Otter trawl"))%>% # Convert method codes to values
  dplyr::filter(Method=="Otter trawl")%>% #Only including otter trawl data because midwater trawl only used rarely and not currently
  #mutate(Date=parse_date_time(Date, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"),
  # Time=parse_date_time(Time, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
  mutate(Datetime=lubridate::parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"))%>%
  dplyr::select(-Time)%>% # Remove unneeded variable
  mutate(Tide=recode(Tide, flood="Flood", ebb="Ebb", low="Low Slack", high="High Slack", outgoing="Ebb", incoming="Flood"), # Rename tide codes for consistency
         Source="Suisun",
         SampleID=ifelse(grepl("\\{|\\}", SampleRowID), paste(Source, SampleRowID),
                         paste0(Source, " {", SampleRowID, "}")))%>% # Create identifier for each sample
  left_join(stations_suisun, # Add station locations
            by="Station")%>%
  left_join(depth_suisun, # Add bottom depths
            by="SampleRowID")%>%
  left_join(effort_suisun, # Add sampling effort
            by="SampleRowID")

# Catch data --------------------------------------------------------------

catch_suisun <- suisunMarshTables$Catch %>%
  transmute(across(c(SampleRowID, OrganismCode), as.character),
            StandardLength = as.double(StandardLength),
            Dead = as.character(Dead),
            Count = as.double(Count),
            CatchComments = as.character(CatchComments)) %>%
  mutate(across(everything(), function(x) replace(x, x %in% c("NA", "n/p", ""), NA)),
         Count=if_else(OrganismCode=="NOCATCH", 0, Count)) %>% #Make sure "no catch" actually has a count of 0
  right_join(sample_suisun, # Add sample-level data
             by="SampleRowID")%>%
  dplyr::filter(Method=="Otter trawl" & OrganismCode!="NOTRAWL")%>% # Only include otter trawl and exclude samples with no trawl.
  left_join(Species%>% # Add species names
              dplyr::select(OrganismCode=SMF_Code, Taxa)%>%
              dplyr::filter(!is.na(OrganismCode)),
            by="OrganismCode")%>%
  dplyr::select(-OrganismCode)%>% # Remove unneeded variable
  mutate(Count = if_else(SampleRowID=="{8327B645-BC36-4405-ADB3-C6561718A17B}" & StandardLength==87, Count+1, Count))%>% # Correcting for misstyped data point per email from Teejay that
  dplyr::filter(!(!QADone & Taxa=="Pogonichthys macrolepidotus" & StandardLength==8))%>% # all QADone==FALSE data from January 2007 are correct EXCEPT for that lone splittail measuring 8 mm (was actually 87 mm).
  mutate(StandardLength=if_else(is.na(Taxa), NA_real_, StandardLength))%>% #COnverting lengths to NA for samples in which no fish were caught (i.e. Taxa is NA).
  mutate(CatchComments=if_else(SampleID=="Suisun {490F4873-8947-4352-81C9-3AA475D5FEEE}" & Taxa=="Gobiidae" & StandardLength==0, "larval", CatchComments)) # Removing weird symbol in this comment that messes up code


# Fixing problems encountered non-random measuring of fish --------

## Sometimes the measured and unmeasured fish did not represent random samples from the same pool
## This would often occur when there were multiple clear size classes of a species
## In the data, this manifests as StandardLength==0 AND !is.na(CatchComments)
## StandardLength==0 represents unmeasured fish and the comments would say something like "<30mm"


# This approach assumes that if a record of an unmeasured fish has a comment, there are no other unmeasured fish in that sample of the same species without a comment.
# Currently, as of 4/15/20, this is true.
# First have to manually translate the catch comments from the below exported csv into the format of Suisun comments.xlsx, in which comments are translated
# into a minimum length, maximum length, or measured length. Some comments (like "YOY") could not be translated into lengths and will remain NAs.
# Other comments had nothing to do with length (e.g., "with eggs") and those comments are marked "ignore" to indicate we will assume they DO represent
# random samples of all catch of that species.

# dplyr::filter(catch_suisun, StandardLength==0 & !is.na(CatchComments))%>%
# dplyr::select(SampleRowID, Station, Date, Datetime, SampleID, TrawlComments, Taxa, Count, CatchComments)%>%
#   write_csv("~/Suisun comments.csv")

## For future updates, create the csv files as follows
 #old<-read_excel(file.path("data-raw", "Suisun", "Suisun comments.xlsx"))%>%mutate(ID=paste(SampleID, Taxa, CatchComments))
 #new<-dplyr::filter(catch_suisun, StandardLength==0 & !is.na(CatchComments))%>%
 #mutate(SampleRowID=paste("{",SampleRowID,"}",sep=""),
  #      SampleID=paste("Suisun",SampleRowID,sep=" "),
  #      Min_length=ifelse(grepl(">",CatchComments),1,as.numeric(NA)),
  #      Length=ifelse(grepl("~",CatchComments)|grepl("about",CatchComments),1,as.numeric(NA)),
  #      Max_length=ifelse(grepl("<",CatchComments),1,as.numeric(NA)),
  #      Lifestage=ifelse(grepl("YOY",CatchComments),"YOY",
  #                       ifelse(grepl("larva",CatchComments),"Larval",
  #                              ifelse(grepl("uvenile",CatchComments),"Juvenile",
  #                                     ifelse(grepl("dult",CatchComments),"Adult",as.character(NA))))),
  #      Lifestage=ifelse(grepl("age 0",CatchComments),"Age-0",
  #                       ifelse(grepl("age 1",CatchComments),"Age-1",
  #                              ifelse(grepl("age 2",CatchComments),"Age-2+",Lifestage))),
  #      Ignore=ifelse(grepl("egg",CatchComments)|grepl("ovi",CatchComments)|grepl("OVI",CatchComments),1,as.numeric(NA)),
  #      ID=paste(SampleID, Taxa, CatchComments)
  #     )%>%
 #dplyr::filter(!ID%in%old$ID)%>%
 #dplyr::select(SampleRowID, Station, Date, Datetime, SampleID, TrawlComments, Taxa, Count, CatchComments,Min_length,Length,Max_length,Lifestage,Ignore)%>%
  # write_csv("~/Suisun comments.csv")



# The overall approach is to, where possible, convert these comments into Size Groups as used by Baystudy
# If we can identify the range of lengths sampled for these unmeasured lengths, we can then find all fish actually
# Measured within the same range and assign those to the same group for later calculations of adjusted size frequencies.

# Create dataset of just measured lengths for use below
catch_fix<-catch_suisun%>%
  dplyr::select(Taxa, StandardLength, Count, SampleID)%>%
  dplyr::filter(StandardLength>0)

catch_comments_suisun <- read_excel(file.path("data-raw", "Suisun", "Suisun comments.xlsx"))%>% # Read in translated excel comments
  dplyr::filter(is.na(Ignore))%>% #Remove "ignored" comments that have nothing to do with length.
  mutate(Lifestage=recode(Lifestage, YOY="Age-0", Larval="Age-0", Yearling="Age-1"),
         Lifestage=case_when(
           Lifestage=="Adult" & Taxa%in%c("Ameiurus melas","Morone saxatilis") ~ "Age-2+",
           Lifestage=="Adult" & Taxa%in%c("Tridentiger bifasciatus", "Acanthogobius flavimanus") ~ "Age-1+",
           TRUE ~ Lifestage
         ),
         Month=month(as.Date(Date)))%>%
  left_join(Species%>% # Add species names
              dplyr::select(OrganismCode=SMF_Code, Taxa)%>%
              dplyr::filter(!is.na(OrganismCode) & !is.na(Taxa)),
            by="Taxa")%>%
  left_join(age_size_suisun%>%
              dplyr::filter(!is.na(Class)),
            by=c("Lifestage"="Class", "Month", "OrganismCode"))%>%
  mutate(Min_length=if_else(is.na(Min_length), Min, Min_length),
         Max_length=if_else(is.na(Max_length), Max, Max_length))%>%
  dplyr::select(SampleID, Taxa, Count, CatchComments, Min_length, Length, Max_length, Lifestage, Notes)%>%
  mutate(NA_length = if_else(is.na(Min_length) & is.na(Max_length) & is.na(Length), TRUE, FALSE)) # Identify comments with no translatable length information (like "YOY")

sizegroups_suisun <- catch_comments_suisun%>%
  rename(Unmeasured=Count)%>% # Each of these counts is the number of unmeasured fish
  dplyr::filter(!is.na(Min_length) | !is.na(Max_length))%>% # Only start with those with a value for min or max length
  mutate(Min_length=if_else(is.na(Min_length), -Inf, Min_length), # If there is no min length, the min is -Inf. This will help dplyr::select values between min and max.
         Max_length=if_else(is.na(Max_length), Inf, Max_length), # If there is no max length, the max is Inf
         RowNum=2:(n()+1))%>% # Give each comment a unique number excluding 1. THis numbers will be converted to "Size Groups" as used in the Baystudy data.
  nest_join(catch_fix, # For each commented set of unmeasured fish, this will join a mini dataframe with all measured fish of the same species and sample
            by=c("Taxa", "SampleID"))%>%
  rowwise()%>% # This ensures all operations below are performed row-by-row, necessary for these nested dataframes within a dataframe
  mutate(catch_fix = list(mutate(catch_fix, SizeGroup=if_else(StandardLength<Max_length & StandardLength>Min_length, RowNum, as.integer(1)))))%>% # This assigns size groups to each
  # row in the mini dataframes of measured fish. If the measured fish are within the same size range as the unmeasured fish, they are assigned the same
  # size group (corresponding to the row number for now). If measure fish are outside the size range of the measured fish, they are assigned a size-group of 1.
  mutate(NA_length=if_else(nrow(catch_fix)==0, TRUE, FALSE))%>% # If there are no measured fish corresponding to a set of unmeasured fish, they will get a length of NA.
  ungroup()%>%
  unnest(cols="catch_fix", keep_empty = TRUE) # This will expand out the dataframe by each row of the nested mini dataframes of measured fish.

catch_comments_suisun2<-sizegroups_suisun%>% # Start with the parsed comments that contain min or max lengths
  dplyr::select(-SizeGroup, -Count, -StandardLength, -Notes, -Lifestage, -Max_length, -Min_length)%>%
  rename(Count=Unmeasured)%>%
  distinct()%>% # By calling 'distinct' after removing all length-associated data, this will remove any measured lengths added to the dataset by the "unnest" above.
  bind_rows(catch_comments_suisun%>% # Now bind to the rest of the comments that do not have a min nor max length
              dplyr::filter(is.na(Min_length) & is.na(Max_length))%>%
              dplyr::select(-Notes, -Lifestage, -Max_length, -Min_length))%>%
  mutate(Length=case_when(
    !is.na(Length) ~ Length, # If the comment was parsed to an actual length, assign that as a length
    NA_length ~ NA_real_, # If we had marked earlier that length should be NA, make length NA
    TRUE ~ 0), # Otherwise, lengths are unmeasured and can be parsed into adjusted frequencies later, so mark as 0
    RowNum=replace_na(RowNum, 1))%>% # If RowNum (i.e. size group) is NA, replace with 1, which is the default size group
  mutate(Length_NA_flag = if_else(is.na(Length), "Unknown length", NA_character_))%>% # All these NA lengths correspond to an "unknown" length, as opposed to
  # other NA lengths in the data which correspond to empty nets (Taxa would be NA in this latter case as well).
  rename(SizeGroup=RowNum, StandardLength=Length)%>% # Convert RowNum to size group
  left_join(catch_suisun%>% # Join to the catch data to add all sample-level data to these unmeasured length data
              dplyr::select(-StandardLength, -Count, -Dead, -CatchComments)%>%
              distinct(),
            by=c("SampleID", "Taxa"))%>%
  mutate(ID=paste(SampleID, Taxa))%>% # Create an ID for all species by sample combinations that ended up in this corrected portion of the dataset, to avoid data duplication later on.
  dplyr::select(-NA_length)

catch_suisun2<-catch_suisun%>%
  left_join(sizegroups_suisun%>% # Add size groups to measured fish
              dplyr::filter(RowNum==SizeGroup)%>% # There were a few cases where we had multiple size groups per taxa per sample, this ensures we're only them once
              dplyr::select(SampleID, Taxa, StandardLength, Count, SizeGroup),
            by=c("SampleID", "Taxa", "StandardLength", "Count"))%>%
  mutate(SizeGroup=replace_na(SizeGroup, 1))%>% # Unless we've just defined a size group, give it the default value of 1
  mutate(ID=paste(SampleID, Taxa))%>% # Create an ID to correspond the one created above
  dplyr::filter(!(ID%in%unique(catch_comments_suisun2$ID) & StandardLength==0))%>% # Remove all rows from the catch data that correspond to rows in the corrected catch comments to prevent data duplication
  bind_rows(catch_comments_suisun2)%>% # Bind to parsed comments
  mutate(Length_NA_flag=if_else(is.na(Length_NA_flag) & is.na(StandardLength), "No fish caught", Length_NA_flag)) # If length is NA and a flag hasn't been defined yet, no fish were caught

Suisun1 <- catch_suisun2%>%
  dplyr::filter(StandardLength!=0 | is.na(StandardLength))%>% #Remove unmeasured fish to calculate total number of fish measured. The is.na part is Trying to retain samples in which no fish were caught
  group_by(SampleID, Taxa, SizeGroup)%>% # This is where size group comes in, now all calculations are performed separately for each size group
  mutate(TotalMeasured=sum(Count, na.rm=T))%>% # Calculate the total number of fish measured
  ungroup()%>%
  left_join(catch_suisun2%>% # Join to data with total catch of fish. Using a full join
              dplyr::select(SampleID, Taxa, Count, SizeGroup)%>%
              group_by(SampleID, Taxa, SizeGroup)%>%
              summarise(TotalCatch=sum(Count, na.rm=T), .groups="drop"), # Calculate total catch
            by=c("SampleID", "Taxa", "SizeGroup"))%>%
  mutate(ID=paste(SampleID, Taxa, SizeGroup))

Suisun <- Suisun1%>%
  bind_rows(catch_suisun2%>% # Now joining to any records in the catch data that do not corresponding to any measured lengths.
              mutate(ID=paste(SampleID, Taxa, SizeGroup))%>%
              dplyr::filter(!ID%in%unique(Suisun1$ID))%>% # Avoiding data duplication, this is the reason Suisun1 had to be created in a prior step
              mutate(StandardLength=NA_real_, # Converting these 0 lengths to NAs
                     Length_NA_flag = "Unknown length"))%>% # Flagging these as unknown lengths
  mutate(Count = if_else(is.na(TotalMeasured) | TotalMeasured==0, Count, (Count/TotalMeasured)*TotalCatch), # Calculate adjusted frequency, if no fish were measured, leave as Count
         Count = if_else(Count==0 | is.na(Taxa), NA_real_, Count), # Transform all 0 counts, or counts with NA taxa to NA
         Sal_surf=ec2pss(Conductivity/1000, t=25), # Calculate salinity from conductivity
         Taxa=stringr::str_remove(Taxa, " \\((.*)"), # Remove life stage from Taxa
         Taxa=if_else(is.na(Count),  NA_character_, Taxa), # Set Taxa to NA if Length and Count are NA
         StandardLength=if_else(is.na(Count), NA_real_, StandardLength),
         CatchComments=if_else(is.na(Count), NA_character_, CatchComments))%>%
  dplyr::select(Source, Station, Latitude, Longitude, Date, Datetime, Depth, SampleID, Method, Tide, # Re-order variables
         Sal_surf, Temp_surf=Temperature, Secchi,
         Tow_duration=TowDuration, Tow_area, Taxa,
         Length=StandardLength, Count, Length_NA_flag, Notes_catch=CatchComments, Notes_tow=TrawlComments)%>%
  group_by(across(-Count))%>% # Add up any new multiples after removing lifestages
  summarise(Count=sum(Count), .groups="drop")%>%
  group_by(SampleID)%>%
  mutate(Valid=sum(Count, na.rm=T))%>% # Identify cases where there are actually fish catches
  ungroup()%>%
  mutate(Length_NA_flag=if_else(Valid>0, Length_NA_flag, "No fish caught"))%>% # Where there are no fish catches in the whole sample, set to "No fish caught"
  distinct()%>%
  dplyr::filter(!(Valid>0 & is.na(Count)))%>% # Remove rows corresponding to NA catch in samples where there are other fish catches
  dplyr::select(-Valid)%>%
  mutate(Count=if_else(Length_NA_flag=="No fish caught", 0, Count, missing=Count)) # Transform all counts for 'No fish caught' to 0.

# Just measured lengths
Suisun_measured_lengths <- catch_suisun2%>%
  dplyr::filter(StandardLength!=0)%>%
  mutate(Taxa=stringr::str_remove(Taxa, " \\((.*)"))%>% # Remove life stage from Taxa
  dplyr::select(SampleID, Taxa, Dead, Length=StandardLength, Count)

# Differences in comments and presence of unicode errors between bridgeAccess and reading the csvs. TI can't spot the
# specific differences. Deeming that they are equal.

usethis::use_data(Suisun, Suisun_measured_lengths, overwrite=TRUE, compress="xz")
