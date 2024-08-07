# This script executes an EMLassemblyline workflow.

# Initialize workspace --------------------------------------------------------

# Update EMLassemblyline and load

library(EMLassemblyline)
library(LTMRdata)
library(EML)
library(readr)
library(dplyr)
# Define paths for your metadata templates, data, and EML

path <- "publication"
path_templates <- file.path(path, "metadata_templates")
path_data <- file.path(path, "data_objects")
path_eml <- file.path(path, "eml")


# Create data -------------------------------------------------------------

data_integrate(path_data, format="both")

# Create metadata templates ---------------------------------------------------

# Below is a list of boiler plate function calls for creating metadata templates.
# They are meant to be a reminder and save you a little time. Remove the
# functions and arguments you don't need AND ... don't forget to read the docs!
# E.g. ?template_core_metadata

# Create core templates (required for all data packages)

EMLassemblyline::template_core_metadata(
  path = path_templates,
  license = "CCBY",
  file.type = ".docx")

# Create table attributes template (required when data tables are present)

EMLassemblyline::template_table_attributes(
  path = path_templates,
  data.path = path_data,
  data.table = c('survey.csv', 'fish.csv', 'Length_conversions.csv'))

# Create categorical variables template (required when attributes templates
# contains variables with a "categorical" class)

EMLassemblyline::template_categorical_variables(
  path = path_templates,
  data.path = path_data)

# Create taxonomic coverage template (Not-required. Use this to report
# taxonomic entities in the metadata)

#remotes::install_github("EDIorg/taxonomyCleanr")
tax<-FALSE

if(tax){
  library(taxonomyCleanr)

  EMLassemblyline::template_taxonomic_coverage(
    path = path_templates,
    data.path = path_data,
    taxa.table = "fish.csv",
    taxa.col = "Taxa",
    taxa.name.type = "scientific",
    taxa.authority = c(9,3,11))
}

# Create geographic coverage (required when more than one geographic location
# is to be reported in the metadata).

coords<-read_csv(file.path(path_data, 'survey.csv'),
                 col_types=cols_only(Latitude="d", Longitude="d"))%>%
  summarise(South=min(Latitude, na.rm = T),
            North=max(Latitude, na.rm = T),
            West=min(Longitude, na.rm = T),
            East=max(Longitude, na.rm = T))

date_range<-read_csv(file.path(path_data, 'survey.csv'),
                 col_types=cols_only(Date="D"))%>%
  pull(Date)%>%
  range()
# Make EML from metadata templates --------------------------------------------

# Once all your metadata templates are complete call this function to create
# the EML.

#ID<-"edi.1118.2" # Sandbox EDI
ID<-'edi.1075.2' # Real EDI

eml <- make_eml(
  path = path_templates,
  data.path = path_data,
  eml.path = path_eml,
  dataset.title = 'Fish abundance in the San Francisco Estuary (1959-2024), an integration of 10 monitoring surveys.',
  temporal.coverage = date_range,
  maintenance.description = 'ongoing',
  geographic.description = "Data were collected in the San Francisco Estuary, including San Francisco Bay, Suisun Bay and Marsh, and the Sacramento San Joaquin Delta.",
  geographic.coordinates = c(coords$North, coords$East, coords$South, coords$West),
  data.table = c('survey.csv', 'fish.csv', 'Length_conversions.csv'),
  data.table.name = c('Sample-level table', 'Fish-level data', 'Length conversion equations'),
  data.table.description = c('Sample-level environmental and effort data. Can be joined to the fish table with the SampleID column.', 'Fish-level length and abundance data. Can be joined to the survey table with the SampleID column.', 'Length conversion equations for 20 fishes, of the form fork or total length = intercept + slope * standard length.'),
  data.table.quote.character = c('"', '"', '"'),
  other.entity = 'fishsurvey_compressed.rda',
  other.entity.name = 'Compressed fish and survey data.',
  other.entity.description = 'The fish and survey tables compressed into an rda file (compressed file format for the R programming language).
                               These data are exactly identical to their csv analogs. They are provided to reduce download time for R-users.
                               This file can be read into R using the "load" function.',
  user.id = 'sbashevkin',
  user.domain = 'EDI',
  package.id = ID,
  return.obj = TRUE
)

changelog<-list(list(changeScope="Metadata and data",
                     oldValue="See previous version (1)",
                     changeDate="2024-07-30",
                     comment="1) Updated all datasets to what was available as of at least July 19, 2024 except Suisun, which was updated earlier.
                              2) Added Turbidity in NTU or FNU.
                              3) Added Salvage dataset")
                )
class(changelog)<-c("emld", "list")

eml$dataset$maintenance$changeHistory<-changelog
write_eml(eml, file.path(path_eml, paste0(ID, ".xml")))
eml_validate(file.path(path_eml, paste0(ID, ".xml")))
