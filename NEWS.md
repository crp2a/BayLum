## BayLum 0.1.3 (Release date: 2018-08-31)

### New functions
* `plot_Ages()` this function replaces code used internally by the functions `AgeC14_Compution()` and `AgeS_Computation()`
 to create an age overview plot. Before the user had no control over the appearance of age plot output. This is still the 
 case if the functions `AgeC14_Compution()` and `AgeS_Computation()` are used (although internally only the function `plot_Ages()` is called), 
 however, both functions now return a new list of class `BayLum.list` which is recognised by the function `plot_Ages()` and can be used to plot the typical age plot while providing several arguments for adjusting the plot output. 

### Bugfixes
* The newly introduced function combine_DataFiles() did not work on R < 3.5.0, which caused an 
error on CRAN; fixed.
* Replace function `ScatterSamples()` by `plot_Scatterplots()`
* All computation functions left text connections open after they have been exited; fixed. 
* The output of `AgeC14_Compution()` and `AgeS_Computation()` gain a new class `BayLum.list`
* Remove redundant code in `AgeC14_Compution()` and `AgeS_Computation()` 


## BayLum 0.1.2 (Release date: 2018-06-22)

### New functions
* The function `MCMC_plot()` was replaced by the newly written function `plot_MCMC()`
* The function `Concat_DataFile()` is deprecated and is basically replaced by the function `combine_DataFiles()`

### Bugfixes
* The functions `AgeC14_Compution()` and `Age_OSL14()` now returns age results with two digits
* The function `AgeS_Computation()` now returns the MCMC information from all samples of the PDF output is set to `TRUE`
* The function `Age_OSLC14()` never returned numerical data; fixed
* Fix `Age_OSLC14()` age format, now in ka for C-14 instead of years
* The last example in `AgeS_Computation()` could not be run; fixed
* C-14 calibration datasets now show data in 'ka' and not 'a'
* Various typo corrections.

### Enhancements
* Allow different BIN/BINX-file names as input. The functions `Generate_DataFile()` and `Generate_DataFile_MG()` do no longer expecting the name sheme bin.BIN
* The functions `Generate_DataFile()` and `Generate_DataFile_MG()` now supports the `...` argument to pass further
arguments to the function `Luminescene::read_BIN2R()`
* The functions `Generate_DataFile()` and `Generate_DataFile_MG()` gained a new argument `verbose` to control the terminal output
* The function `Age_Computation()`,`AgeS_Computation()`,`AgeC14_Computation()`, `Age_OSLC14()` now support the argument 
`quiet` and pipe it to `rjags` to suppress the terminal output
* The function `AgeC14_Computation()` now returns a warning if outliers are detected. 
* The function `LT_RegenDose()` gained two new arguments `nrow` and `ncol` became a little bit more user friendly

### Internals
* Set patch level to 0 (for 3.3.0) to account for CRAN note
* Vignette was updated and corrected


## BayLum 0.1.1 (Release date: 2017-09-01)

* Reduce required R version to 3.3.2

## BayLum 0.1.0 (Release date: 2017-08-22)

* Initial version on CRAN
