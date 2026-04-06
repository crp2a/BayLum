# Prepare input data for subsequent BayLum Analysis

The function pre-processes input data from BIN/BINX file, XSYG files or
[Luminescence::RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.html)
objects for `'BayLum'`. The parameters for the modelling are controlled
by a to be supplied YAML configuration file (please read package
vignette).

## Usage

``` r
create_DataFile(config_file, verbose = TRUE)
```

## Arguments

- config_file:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  path to YAML configuration file; alternatively the config_file can be
  a [list](https://rdrr.io/r/base/list.html) similar to the R
  representation of the imported YAML file. This enables on-the fly
  modifications

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable terminal feedback

## Value

Returns a [list](https://rdrr.io/r/base/list.html) that can be processed
by the modelling functions of 'BayLum'

- **LT** (one list per sample); each list contains all L/T values for
  the corresponding sample;

- **sLT** (one list per sample); each list contains all uncertainties on
  L/T values for the corresponding sample;

- **ITimes** (one list per sample); each list contains irradiation time
  values for the corresponding sample;

- **dLab**, a matrix containing in line `i`, the laboratory dose rate
  and its variance for sample `i`;

- **ddot_env**, a matrix containing in line `i`, the environmental dose
  rate and its variance (excluding the common error terms) for sample
  `i`;

- **regDose** (one list per sample); each list contains all regenerated
  doses;

- **J**, a vector giving, for each BIN file, the number of aliquots
  selected for the analysis;

- **K**, a vector giving, for each BIN file, the number of regenerative
  doses in the SAR protocol;

- **Nb_measurement**, a vector giving, for each BIN file, the number of
  measurements;

- **SampleNames**, a character vector with the sample names;

- **Nb_sample**, the number of samples in the dataset

## Details

The function uses a single configuration file based on the YAML format
and operates in two modes:

\(1\) The YAML file contains the path to the files and the function
attempts to import them. In such a case, all files must be thoroughly
prepared (e.g., strictly follow the SAR protocol etc.).

\(2\) Alternatively, the YAML file contains no file paths but the data
were imported and processed before `create_DataFile()` was called
(recommended). Then the function is searching for objects with the
sample name in the global environment. Example:
`samp1 <- Luminescence::read_BIN2R(...)` with `samp1` the sample name as
specified in the YAML file.

For more details, please see the package vignette.

## Function version

0.1.0

## See also

[write_YAMLConfigFile](https://crp2a.github.io/BayLum/dev/reference/write_YAMLConfigFile.md),
[yaml::read_yaml](https://yaml.r-lib.org/reference/read_yaml.html),
[Luminescence::read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.html),
[Luminescence::read_XSYG2R](https://r-lum.github.io/Luminescence/reference/read_XSYG2R.html),
[Luminescence::subset_SingleGrainData](https://r-lum.github.io/Luminescence/reference/subset_SingleGrainData.html)

## Author

Sebastian Kreutzer, Institute of Geography, Ruprecht-Karl University of
Heidelberg (Germany), in parts based on code by Claire Christophe

## Examples

``` r
##set path to YAML file
yaml_file <- system.file("extdata/example.yml", package = "BayLum")

samp1_file <- system.file("extdata/samp1/bin.bin", package = "BayLum")
samp2_file <- system.file("extdata/samp2/bin.bin", package = "BayLum")

## import BIN files
samp1 <- Luminescence::read_BIN2R(samp1_file, verbose = FALSE) |>
 subset(POSITION == 2 & GRAIN == 32)
samp2 <- Luminescence::read_BIN2R(samp2_file, verbose = FALSE) |>
 subset(POSITION == 2 & GRAIN == 32)

## create file
create_DataFile(yaml_file)
#> ── BayLum - [create_DataFile()] ────────────────────────────────────────────────
#> ⠙ Load YAML configuration file ... 
#> ✔ Load YAML configuration file ...  [7ms]
#> 
#> ⠙ Sanitize sample names ... 
#> ✔ Sanitize sample names ...  [12ms]
#> 
#> ⠙ Generate object list ... 
#> Error: [create_DataFile()] <samp1> is not a valid object in the working environment!
#> ✖ Generate object list ...  [19ms]
#> 
```
