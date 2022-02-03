# TODO:
# 1) Add variables to attributes templates that were recently added to the dataset







# This script executes an EMLassemblyline workflow.

# Initialize workspace --------------------------------------------------------

# Update EMLassemblyline and load

#remotes::install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)
source("data-raw/data_divide.R")

# Define paths for your metadata templates, data, and EML
root<-file.path("data-raw", "EDI")
path_templates <- file.path(root, "metadata_templates")
path_data <- file.path(root, "data_objects")
path_eml <- file.path(root, "eml")

data_tables<-c("survey.csv", "fish.csv", "Length_conversions.csv")


# Create data -------------------------------------------------------------

data_divide(path_data)

# Create metadata templates ---------------------------------------------------

# Below is a list of boiler plate function calls for creating metadata templates.
# They are meant to be a reminder and save you a little time. Remove the
# functions and arguments you don't need AND ... don't forget to read the docs!
# E.g. ?template_core_metadata

# Create core templates (required for all data packages)

template_core_metadata(
  path = path_templates,
  license = "CCBY",
  file.type = ".docx")

template_provenance(
  path = path_templates
)

# Create table attributes template (required when data tables are present)

template_table_attributes(
  path = path_templates,
  data.path = path_data,
  data.table = data_tables)

# Create categorical variables template (required when attributes templates
# contains variables with a "categorical" class)

template_categorical_variables(
  path = path_templates,
  data.path = path_data)

# Create geographic coverage (required when more than one geographic location
# is to be reported in the metadata).

template_geographic_coverage(
  path = path_templates,
  data.path = path_data,
  data.table = "survey.csv",
  lat.col = "Latitude",
  lon.col = "Longitude",
  site.col = "Station")

# Create taxonomic coverage template (Not-required. Use this to report
# taxonomic entities in the metadata)

#remotes::install_github("EDIorg/taxonomyCleanr")
library(taxonomyCleanr)

taxonomyCleanr::view_taxa_authorities()

template_taxonomic_coverage(
  path = path_templates,
  data.path = path_data,
  taxa.table = "fish.csv",
  taxa.col = "Taxa",
  taxa.name.type = "scientific",
  taxa.authority = c(3,9,11))

# Make EML from metadata templates --------------------------------------------

# Once all your metadata templates are complete call this function to create
# the EML.

#Sandbox
ID<-"edi.746.1"

#EDI
#ID<-"edi.1075.1"

make_eml(
  path = path_templates,
  data.path = path_data,
  eml.path = path_eml,
  dataset.title = "Fish abundance in the San Francisco Estuary (1959-2021), an integration of 9 monitoring surveys.",
  temporal.coverage = c("1959-06-13", "2021-09-23"),
  maintenance.description = "ongoing",
  data.table = data_tables,
  data.table.url = c("https://deltacouncil.box.com/shared/static/hgnmfhhlceg1r8qmw6n51dx9nhk4rhwp.csv",
                     "https://deltacouncil.box.com/shared/static/wzn5xixwl7fyoudbu4fn2k1d94hytjva.csv",
                     "https://deltacouncil.box.com/shared/static/11ip1suyvp2z6byuhomhldfifralyegd.csv"),
  data.table.name = c("Sample-level table", "Fish-level data", "Length conversion equations"),
  data.table.description = c("Sample-level environmental and effort data. Can be joined to the fish table with the SampleID column.",
                             "Fish-level length and abundance data. Can be joined to the survey table with the SampleID column.",
                             "Length conversion equations for 20 fishes, of the form fork or total length = intercept + slope * standard length."),
  data.table.quote.character=c('"','"','"'),
  other.entity = c("fishsurvey_compressed.rds"),
  other.entity.url = "https://deltacouncil.box.com/shared/static/bmmel4xzjg59mqmjzci8chsxmv869on8.rds",
  other.entity.name = c("Compressed fish and survey data."),
  other.entity.description = c("The fish and survey tables compressed into an rda file (compressed file format for the R programming language).
                               These data are exactly identical to their csv analogs. They are provided to reduce download time for R-users."),
  user.id = "sbashevkin",
  user.domain = "EDI",
  package.id = ID)
