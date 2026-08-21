# Purpose

The following outlines the protocol for updating datasets in the LTMRdata package. It is applicable across all datasets but may have specific clauses for certain surveys. Overall workflow:

1.  Download the new dataset
2.  Wrangle the data
3.  Compare new to old data
4.  Ensure all tests pass
5.  Update `rda` file and finish

# Protocol

1.  GitHub Preparation

    a.  GitHub - sbashevkin/LTMRdata: Data for the IEP long term monitoring survey review · GitHub
    b.  Clone repository
    c.  Create new branch, name it `survey_updateYear`, e.g., “Salvage_2025”

2.  Open data wrangling script: ALL subsequent steps happen in this code

    a.  In `data-raw` folder, named per survey

    b.  Applicable surveys are: (organization)

        i.  20mm.R (CDFW)
        ii. Baystudy.R (CDFW)
        iii. DJFMP.R (USFWS)
        iv. EDSM.R (USFWS)
        v.  FMWT.R (CDFW)
        vi. Salvage.R (CDFW)
        vii. SKT.R (CDFW)
        viii. STN.R (CDFW)
        ix. SLS.R (CDFW)
        x.  Suisun.R (UCDAVIS)

3.  Download the newest data

    a.  CDFW surveys are downloaded from their FTP website. These are generally Access databases
    b.  DJFMP surveys are downloaded from their EDI publications. These are generally csv tables
    c.  The Suisun Marsh (UCDAVIS) dataset is downloaded from its EDI publication
    d.  Potential complications and remedies:
        i.  Dead link
            1.  Update link
        ii. Changed internet protocol, e.g., SSL error
            1.  Manually download the file(s) via your browser and move to appropriate location to read into R

4.  Read in the data

    a.  Data from Access databases are pulled directly via the relational tables: two approaches
        i.  `bridgeAccess` function. There is an internal version of this function or use the `deltadata` version (which may be more updated) that connects R to Access
            1.  Huge limitation: this function REQUIRES a 32-bit R OR 64-bit Access, read more: <https://trinhxuann.github.io/deltadata/articles/qaqcDemonstration20mm.html#prerequisite-to-enable-bridgeaccess>
        ii. Manually export the relevant relational tables as `csv` or `txt` by opening Access itself
    b.  Potential complications and remedies:
        i.  Relational table changed or deleted
            1.  Remove or change relational tables
            2.  Check “Relationships” table within Access (Ribbon -\> Database Tools -\> Relationships)

5.  Run the wrangling code

    a.  Potential complications and remedies:
        i.  Column names changed or deleted
            1.  Change names accordingly
            2.  Reach out to PIs to understand changes
        ii. Joining steps produces `many-to-many` relationships
            1.  Missing column keys
            2.  Check “Relationships” table within Access to understand join types and keys
            3.  Reach out to PIs to understand how to best join relational tables
    b.  Product is a singular joined dataset, ready to update

6.  Run the comparison code

    a.  `create_comparison_code()`
        i.  Located in `data-raw\comparison.R`. Use `source(file.path("data-raw", "comparison.R"))` if not in your script
        ii. The function takes 3 arguments:
            1.  `new_data`: fully wrangled data table, e.g. “Salvage”
            2.  `old_data`: snapshotted data table from current LTMRdata publication, e.g., `LTMRdata::Salvage`
            3.  `id_cols`: vector of column names that together describe each row uniquely. MUST be unique per row.
    b.  Logic:
        i.  This function compares historical data that we have accepted (previous publication of `LTMRdata`) to the SAME data that has just been downloaded during this current update.
        ii. The historical data should NOT change. If it did, potentially structural changes occurred to the dataset
    c.  Application: use on data tables to be updated
        i.  Some surveys have two data tables to be updated, the joined dataset and a measured length dataset
    d.  If changes are found: MUST investigate
        i.  Understand what “SampeID” means for your survey. Explore database for those IDs
        ii. Reach out to survey PI to understand changes
        iii. Document changes:
             1.  Directly in the code via comments
             2.  In the commit message
    e.  Potential complications and remedies
        i.  `many-to-many` relationships
            1.  `id_cols` must contain all column names to make each row unique
        ii. False flags
            1.  `id_cols` must contain all column names to make each row unique

7.  Run the test code

    a.  `test_dataset()`
        i.  Located in `tests\testthat\helper-data_checks.R`. Load the data package to load this function, i.e., `devtools::load_all(".")` in the console or `Ctrl + Shift + L` shortcut in RStudio
    b.  Logic:
        i.  Various logistical tests that the dataset must pass to allow clean integration with other survey datasets for `deltafish`
        ii. These same tests are ran automatically during the testing phase of the package but on the fully integrated dataset (all surveys together). Running this function on an individual dataset allows for easier diagnosis
    c.  If test fails: MUST investigate
        i.  The returned product is a list of the failing data rows
        ii. Use the sample ID to trace the complications. Reach out to the PIs as necessary
    d.  Potential complications and remedies
        i.  Failing length and catch flags
            1.  Ensure that new species caught are entered into `Species.csv`:
                a.  Found in `data-raw\Species Code.csv`
                b.  If the species already exists in this look up table, add the `OrganismCode` to the table for your survey (column)
                c.  If the species does not already exist, add a new row and fill out the `OrganismCode` for your survey (column)
                d.  If `Species.csv` is changed, you must update:
                    i.  `Species.rda` file by running `data-raw\Species.R` afterwards
                    ii. Species table metadata in `R\data.R`

8.  Update the passing dataset

    a.  `use_data()` step

9.  Update package metadata

    a.  Update table metadata:
        i.  Open `R\data.R`
        ii. Find your dataset. Update:
            1.  Potential TWO tables per survey:
                a.  Survey data table, e.g., “Salvage”
                b.  Measured length, e.g., “Salvage measured lengths”
            2.  Table description (as needed)
            3.  Number of rows and columns
            4.  Column names: add, remove, or update (as needed)
                a.  Format: `\item{column name}{column description}`
    b.  Document your changes: `Ctrl + Shift + D` in RStudio
        i.  This command updates help files and the namespace file

10. Final testing

    a.  `Build` tab in R Studio
    b.  Click `Check`: takes quite a while
        i.  This runs through various built-in tests. All tests must pass

11. GitHub publication

    a.  Commit your changes
        i.  Document any changes from step 6 above in the commit message
    b.  Push commit(s)
    c.  Trigger pull request for inclusion into main data package
