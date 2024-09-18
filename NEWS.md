# BayLum 0.3.3.9000-13

**This version of BayLum requires R >= 4.3!**

## Miscellaneous

- Fix various anchors in the manual with broken target (CRAN request).

## Internals

- The vignette manual code takes very long to execute and must be pre-computed.
  To pre-compute the vignette: make changes in `BayLum.Rmd.orig`, then run 
  `vignettes/precompile.R` locally.

# BayLum 0.3.2 (2024-04-14)

**This version of BayLum requires R >= 4.1.x!**

## New functions

- `create_DataFile()`: this function attempts to ease the very pain-stacking 
  process of input data processing (BIN/BINX files) so that it can be used in 
  the **BayLum** modelling. The function is an entirely new development and
  will replace the functions `Generate_DataFile_MG()` and
  `Generate_DataFile()` in future versions of **BayLum**. Key features:
    - Only one function for single-grain and multiple-grain data.
    - Support for BIN/BINX and XSYG files and `RLum.Analysis-class` objects,
      i.e. the data can be imported independently of the data file creation.
    - A new single-file configuration based on the YAML format. This makes
      the old error-prone folder structure obsolete.
    - Various internal consistency checks to avoid input data are
      erroneousness; which may lead to very hard to-track-down error
      messages.
    - Carries information about the number of samples and the sample names.
- `plot_RegDosePoints()`: the function will replace the deprecated function 
  `LT_RegenDose()`, which was too error-prone and complicated to maintain.
- `write_YAMLConfigFile()`: This function supplements `create_DataFile()` 
  supporting auto-generated YAML config template files.

## Bugfixes and changes

- `Age_OSLC14()`: determines the sample name automatically by default.
- `Age_Computation()`: determines the sample name automatically by default.
- `AgeS_Computation()`: determines the sample name automatically by default.
- `combine_DataFiles()`: now preserves the originator attribute.
- `SCMatrix()`:
    - Interactive mode is now more beautiful and consistently.
    - Can now extract information from an object created by
      `create_DataFile()`.
- `SC_Ordered()`: can handle objects created by `create_DataFile()` for easier 
  usage.

## Miscellaneous

- Drop dependency to deprecated package `'ArchaeoPhases'` and make
  corresponding code an internal function (PR #33, @nfrerebeau).
- Add dependencies to the packages `'yaml'` and `'cli'`.
- Bump version requirement for `'Luminescence'` to >= v0.9.22.
- Add new external files `example.yml` and `yaml_config_reference.yml`.
- Internal datasets `DATA1`, `DATA2`, `DATA3` were updated to be
  consistent with the new function `create_DataFile()`.

## Deprecated functions

The following functions are deprecated and will be removed in future
versions of **BayLum**:

- `Generate_DataFile()`
- `Generate_DataFile_MG()`
- `LT_RegenDose()`

# BayLum 0.3.1 (2023-03-28)

**This version of BayLum requires R >= 4.1.x!**

## Bugfixes and changes

- `AgeS_Computation()` & `Age_OSLC14()` had two instances of the
  MCMC-list. One has been removed. This essentially halves the amount of
  memory needed to store the output of these functions. (Thanks to @guiguerin 
  for reporting, #27).
- `AgeS_Computation()` crashed reproducibly, if the THETA matrix (as one
  possibility) was provided indeed as a file name instead of a `matrix`
  object; fixed (thanks to M. Heydari for reporting).
- `AgeS_Computation()` the function handles wrong user input for the
  Theta matrix more friendly by converting it automatically to a matrix
  if needed.
- `create_ThetaMatrix()` the function did not throw an error in all
  cases where `sigma_s` was obviously wrong; fixed (thanks to M. Heydari
  for reporting).
- `Generate_DataFile_MG()` the function now checks that the number of
  specified BIN/BINX-files matches the actual number of BIN/BINX-files
  present in the folders. If not, a warning is prompted. This prevents
  hard-to-track-down error messages and odd modelling results that
  erroneously encourage searching for problems with **JAGS** or the
  **BayLum** models.
- `plot_Ages()` gained a couple of new arguments. First, `plot_mode`
  allows to switch between the standard plot output `"ages"` (the
  default) and the new option `"density"`. The latter draws probability
  density curves instead of lines. Further arguments specific to the
  `plot_mode = "density"` are `d_scale` to scale the density plots in
  height and `show_ages` to show the Bayes estimator. More generally,
  `legend.cex` was added to enable a convenient scaling of the legend
  fonts; this works for both plot modes.
- `plot_MCMC()` gained additional support for `BayLum.list` returned by
  `Age_OSLC14()`.
- `plot_Scatterplots()` The functions can now handle `BayLum.list`
  objects if returned by certain functions. Currently:
  `AgesS_Computation()`, `Age_OSLC14()`.

# BayLum 0.3.0 (2022-10-28)

**This version of BayLum requires R >= 4.1.x!**

## New functions

- `write_BayLumFiles()`: @imgoej contributed an excellent new function
  that creates all folders and files needed for **BayLum**, making it a
  lot easier and less error-prone to work with **BayLum**.

## Bugfixes and changes

- `AgeS_Computation()`: the function now supports parallel processing using the 
  package `'rungjags'` (thanks to @imgoej)
- `Age_OSLC14()`:
    - Regression error, the function tried to observe more variables than
      were actually in the model; fixed. (thanks to @imgoej, #21).
    - The function now supports parallel processing using the package
      `'rungjags'` (thanks to @imgoej).

## Internals

- A few fixes regarding string comparisons in `if()` conditions.
- Fix CRAN warning regarding `Escaped LaTeX specials: \&`.

# BayLum 0.2.1 (2022-02-20)

*Note: This package version needs at least R >= 4.0.*

## Bugfixes & changes

- `AgeS_Computation()`:
    - The function crashed if `jags_method = "rjparallel"` with a runtime
      error due to missing information about the RNG type; fixed (thanks
      to @imgoej for spotting).
    - If `jags_method = "rjparallel"` the inits were preset to a specific
      value for the random number generator. While this setting led to a
      crash of the function and probably caused no harm, the setting did
      not make any sense. Now, the random number generator settings (seed,
      type etc.) can be provided via the argument `inits`, which is passed
      to `runjags::autorun.jags`. The default is `NA` to maintain the
      expected behaviour if `jags_method = "rjparallel"` is used.
    - The function gained a new argument `model` to inject custom models
      to run with the parameters as already defined for the model. This
      option allows to adjust priors and likelihoods to test different
      scenarios. Please not that this option works without a safety net,
      mean the function does not check whether the parameters in the model
      are actually available.
    - The function now returns the used model as a character string, in
      combination with the new parameter `model` it can be used to modify
      the model on the fly.

- `Age_OSLC14()`: add support for `"rjparallel"` via new argument
  `jags_method = "rjparallel"`, like in `AgeS_Computation()` (thanks
  @imgoej, #20).

- `Generate_DataFile_MG()`:
    - The function gained a new argument `force_run1_at_a_time`. If set to
      `TRUE` it automatically orders the curves in the BIN/BINX-file
      following the "Run 1 at a time" option. Regardless of whether this
      option was used for the measurement or not. Please note: this cannot
      work if you have assigned a position number more than one time to
      different aliquots.
    - `verbose = FALSE` was not respected under all circumstances; fixed.

# BayLum 0.2.0 (2020-12-06)

*Note: This package version needs at least R >= 3.5.0.*

## Bugfixes & changes

- `AgeS_Computation()`:
    - Now stops if a mismatch was encountered between the number of
      samples and the matrix provided via `THETA` to prevent a **JAGS**
      runtime error.
    - The function experimentally gained a new argument `jags_method`
      to support full automated processing in parallel using the R
      package `'runjags'`; including a `...` argument for a more
      detailed control. By default the calculation is done simply via
      `'rjags'` as usual.
- `Age_OSLC14()`:
    - Add additional output to enhance output table by additional
      information (#11).
    - The CSV-file option now returns the age and the HPD intervals
    - The bugs model for the `gaussian` distribution did not work due
      to a wrongly set index (#13); fixed.
    - If a small number of samples was used and the first was a C-14
      sample **JAGS** crashed; now it throws are more comprehensible error
      message (#14).
- `Generate_DataFile_MG()`: now works only on preselected records; the 
    limitation of OSL only was removed.
- `plot_Ages()`:
    - The sorting of the samples in the graph did not work as
      expected; along with this a new argument `sample_order` was
      added.
    - The function accepts a data.frame as alternative input, if the
      `data.frame` is similar the data frames created by the
      computation functions (e.g., `AgeS_Computation()`). This option
      enables the manual creation of age plots from self-compiled
      data.
- `plot_MCMC()`:
    - Accidentally the function could not display the chains from more
      than 9 samples; fixed (thanks to Maryam Heydari).
    - Now the function understand the output of `AgeS_Computation()`
      for an automated treatment.
    - Wrong variable names now lead to a `warning` instead of a
      `stop`. This is less aggressive and it makes the function easier
      to use by users who only use the plot functionality of
      **BayLum**.
- `plot_Scatterplots()`:
    - Line breaks and leading and trailing white space is now removed
      from the character (vector) provided via `sample_names`
    - If the number of selected samples via `sample_selection` was
      shorter than the number of names provided by `sample_names` the
      function returned an `subscript out of bounds` error; fixed
      (reported by Maryam Heydari).

## Internals

- All models now come with an example showing how they can be
  inspected (before the examples not working).
- Fix a couple of documentation glitches (e.g., part of the
  documentation was missing).
- Smooth the creation of `BayLum.list` objects using a central
  internal function.
- Add new C-14 calibration data and unify the names of those datasets.
  Allowed inputs are `"IntCal13"`,`"IntCal20"`,`"Marine13"`,`"Marine20"`, 
  `"SHCal13"`, `"SHCal20"` or an own dataset which must be stored in a CSV 
  file.

## New functions

- `create_ThetaMatrix()` this function is a helper to create the theta
  matrix that can be provided as input for the functions
  `AgeS_Computation()` and `Age_OSLC14()`.
- `create_FolderTemplates()` creates the folder and file structures
  required by `Generate_DataFile()` and `Generate_DataFile_MG()` on
  the user's hard drive.

# BayLum 0.1.3

# BayLum 0.1.2

# BayLum 0.1.1

# BayLum 0.1.0

- Initial CRAN submission.
