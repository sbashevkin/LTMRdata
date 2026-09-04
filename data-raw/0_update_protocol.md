# Purpose

The following outlines the protocol for updating datasets in the LTMRdata package. It is applicable across all datasets but may have specific clauses for certain surveys. Overall workflow:

1.  Download the new dataset

2.  Wrangle the data

3.  Compare new to old data

4.  Ensure all tests pass

5.  Update `rda` file and finish

# Protocol

## 1.  GitHub Preparation

a.  [GitHub - sbashevkin/LTMRdata: Data for the IEP long term monitoring survey review](https://github.com/sbashevkin/LTMRdata)
    
b.  Clone repository
    
c.  Create new branch, name it `survey_updateYear`, e.g., “Salvage_2025”

## 2.  Open data wrangling script: ALL subsequent steps happen in this code

a. In `data-raw` folder, named per survey

b. Applicable surveys (implementing organization) are:

   1. 20mm.R (CDFW)
        
   2. Baystudy.R (CDFW)
        
   3. DJFMP.R (USFWS)
        
   4. EDSM.R (USFWS)
        
   5. FMWT.R (CDFW)
      
   6. Salvage.R (CDFW)
      
   7. SKT.R (CDFW)
      
   8. STN.R (CDFW)
        
   9. SLS.R (CDFW)
        
   10. Suisun.R (UCDAVIS)

## 3. Download the newest data

a.  CDFW surveys are downloaded from their FTP website. These are generally Access databases
    
b.  DJFMP surveys are downloaded from their EDI publications. These are generally csv tables
    
c.  The Suisun Marsh (UCDAVIS) dataset is currently imported from the Access database, which is available upon request. The source may be updated to EDI in the future.
    
d. Potential complications and remedies:

   1. Dead link: Update link
          
   2. Changed internet protocol, e.g., SSL error: Manually download the file(s) via your browser and move to appropriate location to read into R

## 4.  Read in the data

a. Data from Access databases are pulled directly via the relational tables: two approaches
       
   1. `bridgeAccess` function. There is an internal version of this function or use the `deltadata` version (which may be more updated) that connects R to Access
      
       - Huge limitation: this function REQUIRES a 32-bit R OR 64-bit Access, read more: <https://trinhxuann.github.io/deltadata/articles/qaqcDemonstration20mm.html#prerequisite-to-enable-bridgeaccess>
      
   2. Manually export the relevant relational tables as `csv` or `txt` by opening Access itself
   
b. Potential complications and remedies:
    
   1. Relational table changed or deleted: Remove or change relational tables by  checking “Relationships” table within Access 
        
      - (Ribbon -\> Database Tools -\> Relationships)
        
      - Also can check table names within R with:
     
            ```
            bridgeAccess(db_path,
                         script = file.path("data-raw", "connectAccess.R"))
            ```
            
## 5.  Run the wrangling code

a. Potential complications and remedies:
   
   1. Column names changed or deleted
    
      -  Change names accordingly
   
      -  Reach out to PIs to understand changes
   
   2. Joining steps produces `many-to-many` relationships
   
      -  Missing column keys
   
      -  Check “Relationships” table within Access to understand join types and keys
   
      -  Reach out to PIs to understand how to best join relational tables
   
## 6.  Run the comparison code

a. `create_comparison_code()`
    
   1.  Located in `data-raw\comparison.R`. Use `source(file.path("data-raw", "comparison.R"))` if not in your script
    
   2. The function takes 3 arguments:
    
      -  `new_data`: fully wrangled data table, e.g. “Salvage”
    
      -  `old_data`: snapshotted data table from current LTMRdata publication, e.g., `LTMRdata::Salvage`
    
      -  `id_cols`: vector of column names that together describe each row uniquely. MUST be unique per row.
    
b. Logic:
    
   1.  This function compares historical data that we have accepted (previous publication of `LTMRdata`) to the SAME data that has just been downloaded during this current update.
    
   2. The historical data should NOT change. If it did, potentially structural changes occurred to the dataset, or there may have been corrections to old data
    
c. Application: use on data tables to be updated
    
   1.  Some surveys have two data tables to be updated, the joined dataset and a measured length dataset
    
d. If changes are found: MUST investigate

   1. Understand what “SampeID” means for your survey. Explore database for those IDs

   2. Reach out to survey PI to understand changes

   3. Document changes:

      - Directly in the code via comments

      - In the commit message

e. Potential complications and remedies
    
   1. `many-to-many` relationships

      - This could be a sign that data duplication has occured, so carefully investigate duplications in the values of the `id_cols` pasted together to determine the cause
        
      - If the duplications are expected, you may need to add additional column(s) to the `id_cols` since they must contain all column names necessary to make each row unique

## 7.  Run the test code

a. `test_dataset()`
 
   1.  Located in `R\helper-data_checks.R`. Load the data package to load this function, i.e., `devtools::load_all(".")` in the console or `Ctrl + Shift + L` shortcut in RStudio

b. Logic:

   1. Various logistical tests that the dataset must pass to allow clean integration with other survey datasets for `deltafish`

   2. Most of these same tests are run automatically during the testing phase of the package but on the fully integrated dataset (all surveys together). Running this function on an individual dataset allows for easier diagnosis

c. If test fails: MUST investigate

   1. The returned product is a list of the failing data rows

   2. Use the sample ID to trace the complications. Reach out to the PIs as necessary

d. Potential complications and remedies

   1. Failing length and catch flags: Ensure that new species caught are entered into `Species.csv`:

      - Found in `data-raw\Species Code.csv`

      - If the species already exists in this look up table, add the `OrganismCode` to the table for your survey (column)

      - If the species does not already exist, add a new row and fill out the `OrganismCode` for your survey (column)

      - Update: `Species.rda` file by running `data-raw\Species.R` after changes to `data-raw\Species Code.csv` are completed

## 8. Update the passing dataset

a. `use_data()` step

## 9.  Update package metadata

a. Update table metadata:
  
   1. Open `R\data.R`
  
   2. Find your dataset (potentially two tables):
  
      - Survey data table, e.g., “Salvage”
  
      - Measured length, e.g., “Salvage measured lengths”
  
   3. Update table description as needed
  
   4. Update column names: add, remove, or update (as needed)
  
      -  Format: `\item{column name}{column description}`
  
b. Document your changes: `devtools::document()` or `Ctrl + Shift + D` in RStudio 
  
   1.  This command updates help files and the namespace file

## 10. Final testing

a. `Build` tab in R Studio
 
b. Click `Check`: takes quite a while

    1.  This runs through various built-in tests. All tests must pass, with no errors, warnings, or notes

## 11. GitHub publication

a. Document any changes to the dataset
    
   1. Any changes to the structure of the dataset, other than routine updates to add new data, as well as any corrections to prior errors, should be described in `NEWS.md`

b. Commit your changes
 
   1. Document any changes to the code in the commit message
 
c. Push commit(s)
 
d. Trigger pull request for inclusion into main data package
