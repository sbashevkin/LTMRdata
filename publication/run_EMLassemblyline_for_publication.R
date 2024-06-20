# This script executes an EMLassemblyline workflow.

# Initialize workspace --------------------------------------------------------

# Update EMLassemblyline and load

library(EMLassemblyline)
library(LTMRdata)

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

# Create geographic coverage (required when more than one geographic location
# is to be reported in the metadata).

EMLassemblyline::template_geographic_coverage(
  path = path_templates,
  data.path = path_data,
  data.table = 'survey.csv',
  lat.col = "Latitude",
  lon.col = "Longitude",
  site.col = "Station")

# Create taxonomic coverage template (Not-required. Use this to report
# taxonomic entities in the metadata)

remotes::install_github("EDIorg/taxonomyCleanr")
library(taxonomyCleanr)

EMLassemblyline::template_taxonomic_coverage(
  path = path_templates,
  data.path = path_data,
  taxa.table = "fish.csv",
  taxa.col = "Taxa",
  taxa.name.type = "scientific",
  taxa.authority = c(9,3,11))

# Make EML from metadata templates --------------------------------------------

# Once all your metadata templates are complete call this function to create
# the EML.

ID<-"" # Sandbox EDI
#ID<-'edi.1075.2' # Real EDI

eml <- make_eml(
  path = path_templates,
  dataset.title = 'Fish abundance in the San Francisco Estuary (1959-2024), an integration of 9 monitoring surveys.',
  temporal.coverage = c('1959-06-14', '2024-02-06'),
  maintenance.description = 'ongoing',
  data.table = c('survey.csv', 'fish.csv', 'Length_conversions.csv'),
  data.table.url = c("https://app.box.com/s/9hbc4unysh09nz7kwp6gk7rd1hqmwyoh", "", "https://app.box.com/s/chvsh7rdm6rhl8iz3nnbug97pl441yr1"),
  data.table.name = c('Sample-level table', 'Fish-level data', 'Length conversion equations'),
  data.table.description = c('Sample-level environmental and effort data. Can be joined to the fish table with the SampleID column.', 'Fish-level length and abundance data. Can be joined to the survey table with the SampleID column.', 'Length conversion equations for 20 fishes, of the form fork or total length = intercept + slope * standard length.'),
  data.table.quote.character = c('"', '"', '"'),
  other.entity = 'fishsurvey_compressed.rda',
  other.entity.url = 'https://app.box.com/s/zxf0tr1yqudi2ml57jlhwa3vpkb5w1j0',
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
                     changeDate="2024-07-01",
                     comment="1) Updated all datasets to what was available as of at least February 2024.
                              2) Added Turbidity in NTU or FNU.
                              3) Added Salvage dataset"),
                )
class(changelog)<-c("emld", "list")

eml$dataset$maintenance$changeHistory<-changelog
write_eml(eml, file.path(path_eml, paste0(ID, ".xml")))
eml_validate(file.path(path_eml, paste0(ID, ".xml")))
