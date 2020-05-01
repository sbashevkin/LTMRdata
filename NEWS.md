* Added new function `zero_fill` to fill in 0s for species not recorded in a sample and deal with the unknown lengths from the `Suisun` data. 
* Added new argument `size_cutoff` to remove fish below a certain length.

# LTMRdata 0.3.0

* Added a `NEWS.md` file to track changes to the package
* Added length conversions to convert Suisun measurements to Fork length or Total length for species with no fork
* Added tests
* Re-orded columns of internal datasets to a logical order
* Removed invalid tows from Baystudy data
