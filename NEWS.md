## BayLum 0.1.2 (Release date: 2018-XX-XX)

### Bugfixes
* The functions `AgeC14_Compution()` and `Age_OSL14()` now returns age results with two digits
* The function `AgeS_Computation()` now returns the MCMC information from all samples of the PDF output is set to `TRUE`
* The function `Age_OSLC14()` never returned numerical data; fixed
* Fix `Age_OSLC14()` age format, now in ka for C-14 instead of years

### Enhancements
* Allow different BIN/BINX-file names as input. The functions `Generate_DataFile()` and `Generate_DataFile_MG()` do no longer expecting the name sheme bin.BIN
* The functions `Generate_DataFile()` and `Generate_DataFile_MG()` now supports the `...` argument to pass further
arguments to the function `Luminescene::read_BIN2R()`
* The functions `Generate_DataFile()` and `Generate_DataFile_MG()` gained a new argument `verbose` to control the terminal output
* The function `Age_Computation()`, `AgeC14_Computation()` now support the argument `quiet` and pipes it to `rjags` to suppress verbose terminal output
* The function `AgeC14_Computation()` now returns a warning if outliers are detected. 
* The function `LT_RegenDose()` gained two new arguments `nrow` and `ncol` became a little bit more user friendly

### Internals
* Set patch level to 0 (for 3.3.0) to account for CRAN note


## BayLum 0.1.1 (Release date: 2017-09-01)

* Reduce required R version to 3.3.2


## BayLum 0.1.0 (Release date: 2017-08-22)

* Initial version on CRAN
