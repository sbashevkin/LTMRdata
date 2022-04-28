# LTMRdata (development version)

* Add exported function to wrangle and save the integrated dataset for EDI publication: `data_integrate`

# LTMRdata 2.0.0

* Set `Count` values to 0 for all cases of "No fish caught"
* Standardized usage of `Length_NA_flag`
* Added SKT, SLS, STN, 20mm, DJFMP, and EDSM datasets.
* Fixed issues with incorrect incorporation of zeroes in the `zero_fill` function.
* Updated FMWT, Bay Study, and Suisun data.

# LTMRdata 1.0.0

* Fixed incorrect Bay Study Tow_area values by converting Nautical miles to meters.
* Updated FMWT coordinates to latest version from CDFW. Prior version may have had some 700 stations far east from their actual locations. 
* Fixed issue with Suisun data by forcing all counts corresponding to `OrganismCode=="NOCATCH"` to be 0.
* Tweaked `zero_fill` to remove rows with `Taxa==NA`.
* Re-added `Survey` variable to `FMWT` and `Baystudy` datasets. 

# LTMRdata 0.6.0

* Removed life stage from the "Taxa" column.
* Added option to `zero_fill` to filter to specified species, along with more tests to ensure there are zeros to fill. 

# LTMRdata 0.5.0

* In Suisun data, added counts with no corresponding length measurements, e.g., fish within a certain size group were counted and not measured and no fish within that size group were measured. Despite the use of the word "fish" the vast majority of these instances are for invertebrates. Only 334 rows out of 24,289 corresponded to fish.
* Documented data processing code.
* Fixed some incorrect station locations

# LTMRdata 0.4.0

* Added new function `zero_fill` to fill in 0s for species not recorded in a sample and deal with the unknown lengths from the `Suisun` data. 
* Added new argument `size_cutoff` to remove fish below a certain length.

# LTMRdata 0.3.0

* Added a `NEWS.md` file to track changes to the package
* Added length conversions to convert Suisun measurements to Fork length or Total length for species with no fork
* Added tests
* Re-ordered columns of internal datasets to a logical order
* Removed invalid tows from Baystudy data
