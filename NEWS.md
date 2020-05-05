* In Suisun data, added conts with no corresponding length measurements, e.g., fish within a certain size group were counted and not measured and no fish within that size group were measured. Despite the use of the word "fish" the vast majority of these instances are for invertebrates. Only 334 rows out of 24,289 corresponded to fish.
* Documented data processing code.
* Fixed some incorrect station locations

# LTMRdata 0.4.0

* Added new function `zero_fill` to fill in 0s for species not recorded in a sample and deal with the unknown lengths from the `Suisun` data. 
* Added new argument `size_cutoff` to remove fish below a certain length.

# LTMRdata 0.3.0

* Added a `NEWS.md` file to track changes to the package
* Added length conversions to convert Suisun measurements to Fork length or Total length for species with no fork
* Added tests
* Re-orded columns of internal datasets to a logical order
* Removed invalid tows from Baystudy data