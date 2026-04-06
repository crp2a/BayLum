# Generates, from one (or several) BIN file(s) of Multi-grain OSL measurements a list of luminescence data and information before statistical analysis (DEPRECATED)

This function is used to generate, from the BIN file(s), a list of
values of:

**Multi-grain** OSL intensities and associated uncertainties,
regenerative doses, etc., which will be the input of the Bayesian
models. To be easy-to-use, this function requires a rigorous
organisation - all needed files should be arranged in one folder - of
informations concerning each BIN file.  
It is possible to process data for various samples simultaneously and to
consider more than one BIN-file per sample.

## Usage

``` r
Generate_DataFile_MG(
  Path,
  FolderNames,
  Nb_sample,
  Nb_binfile = length(FolderNames),
  BinPerSample = rep(1, Nb_sample),
  sepD = c(","),
  sepDE = c(","),
  sepDS = c(","),
  sepR = c("="),
  verbose = TRUE,
  force_run1_at_a_time = FALSE,
  ...
)
```

## Arguments

- Path:

  [character](https://rdrr.io/r/base/character.html) (**required**): the
  path to the project folder, containing one or more sub folders in
  which the BIN files are located. If it is not equal to `""`, it must
  end with `"/"`.

- FolderNames:

  [character](https://rdrr.io/r/base/character.html) (**required**)
  vector: list of names of the sub-folders containing the BIN files

  - each sub folder must contain a BIN file and associated csv files.
    See details for more informations on associated csv files required
    in the sub folders. If there is more than one BIN file per sample,
    see the details section for instructions regarding how to correctly
    fill the `FolderNames` vector.

- Nb_sample:

  [integer](https://rdrr.io/r/base/integer.html) (**required**): number
  of samples

- Nb_binfile:

  [integer](https://rdrr.io/r/base/integer.html) (with default): number
  of BIN files. It must be equal to, or greater than `Nb_sample`.

- BinPerSample:

  [integer](https://rdrr.io/r/base/integer.html) vector (with default):
  vector with the number of BIN files per sample. The length of this
  vector must be equal to `Nb_sample` and the sum of entries of this
  vector must be equal to `Nb_binfile`. If there is more than one BIN
  file per sample, see the details section for instructions regarding
  how to correctly fill `BinPerSample` vector. Otherwise, this vector
  must contain a list of 1 values.

- sepD:

  [character](https://rdrr.io/r/base/character.html) (with default):
  column separator in the `DiscPose.csv` files.

- sepDE:

  [character](https://rdrr.io/r/base/character.html) (with default):
  column separator in the `DoseEnv.csv` files.

- sepDS:

  [character](https://rdrr.io/r/base/character.html) (with default):
  column separator in the `DoseLab.csv` files.

- sepR:

  [character](https://rdrr.io/r/base/character.html) (with default):
  column separator in the `Rule.csv` files.

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (with default):
  enable/disable verbose mode

- force_run1_at_a_time:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*): if
  set to `TRUE`, the order of the records is pushed to follow the one
  "Run 1 at a time" order (this is, all sequence steps were performed on
  one aliquot before moving to the next aliquot), regardless of their
  original sequence. The default is `FALSE` because `'BayLum'` assumes
  that the sample was measured with the "Run 1 at a time" option (only
  Risø readers, lexsyg readers do not have another option). In other
  words, the argument allows you to automatically correct your input
  data to follow the order `'BayLum'` expects. Why isn't the default
  value `TRUE`?. Because this re-ordering must fail if a measurement
  position was used more than once for different samples! This typically
  happens when different BIN/BINX files are merged.

- ...:

  further arguments that can be passed to
  [Luminescence::read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.html).

## Value

A list containing the following objects:

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

\*\* How to save this list \*\*

You can save this list in a `.RData` object. To do this, you can use the
function [`save`](https://rdrr.io/r/base/save.html). Then, to load this
list you can use the function [`load`](https://rdrr.io/r/base/load.html)
(see example section fore more details).

## Details

With `Path` and `FolderNames`, this function goes to the sub folders
containing the BIN files and associated information to compute the
luminescence data.  

\*\* What are the required files in each subfolder? \*\*  

Each subfolder can be named, for example, as the sample name followed by
a number; it must contain:

- **bin.bin**, the bin file renamed as bin.BIN (note: the name of all
  files matters);

- **Disc.csv**, a one columns csv file containing the list of disc
  number of the previously selected grains (typically this list will
  include the position of grains based on their sensitivity, recycling
  or other properties);

- **DoseEnv.csv**, a two columns file containing the observation of the
  natural (or environmental), dose rate, and its non-shared variance
  (i.e. after removing all shared errors), both in Gy. Note: the user
  shall provide the squared value of the error associated with the dose
  rate experienced by the sample grains in nature;

- **DoseSourve.csv**, a two columns file containing the observation of
  the laboratory dose rate, and its variance (squared error), both in
  Gy;

- **rule.csv**, a csv file containing information on

  - `beginSignal=` the first channel for summing the natural or
    regenerative OSL signal (typically 1 or 6);

  - `endSignal=` the last channel for summing the natural or
    regenerative OSL signal (typically 5 or 10);

  - `beginBackground=` the first channel for background estimation of
    the natural or regenerative OSL signal (typically 76 or 81);

  - `endBackground=` the last channel for background estimation of the
    natural or regenerative OSL signal (typically 95 or 100);

  - `beginTest`,

  - `endTest`,

  - `beginTestBackground`,

  - `endTestBackground=` same values as above, for the test dose
    response (typically the same values should be used);

  - `inflatePercent=` uncertainty arising from the instrument
    reproducibility (typically 0.02, i.e. 2\\

  - `nbOfLastCycleToRemove=` number of cycles at the end of the SAR
    protocol which should not be included in the dose response curve
    fitting (typically 1 if only a recycling test is performed, or 2 if
    both recycling and IR depletion are tested).  

\*\* How to fill the `FolderNames` vector? \*\*  

`FolderNames` is a vector of length `Nb_binfile`. `FolderNames[i]` is
the name (e.g., Sample1-File1, or successive names separated by "/"
signs, if BIN files are in subfolders, e.g. Sample1/File1) of the
subfolder containing all informations on the BIN file of ID number `i`.
The names in `FolderNames` are ordered following two rules:

- The names in the `FolderNames` vector must be ordered following the
  sample order (the names of subfolders containing BIN files for the
  same sample should follow each other in the `FolderNames` vector, e.g.
  Sample1, Sample2-File1, Sample2-File2, etc.).

- If stratigraphic constraints apply to samples, and so a **Bayesian
  model with stratigraphic constraints** is implemented, then the names
  in the `FolderNames` vector must be ordered by order of increasing
  ages.  
  For example, `FolderNames=c(noun1,noun2)`, in which case `noun1`
  (respectively, `noun2`) corresponds to the subfolder name containing
  the BIN file of sample 1 (respectively of sample 2). In addition, if
  we know that sample 1 is younger than sample 2, then `FolderNames`
  vector is correctly filled.  
  If conversely, `FolderNames=c(noun2,noun1)`, the analysis performed by
  [`AgeS_Computation`](https://crp2a.github.io/BayLum/dev/reference/AgeS_Computation.md)
  would not be consistent.  

\*\* How to fill the `BinPerSample` vector? \*\*  

`BinPerSample[i]` corresponds to the number of BIN files for the sample
whose number ID is equal to `i`.  
For example, let us consider a case with two samples (Sample1 and
Sample2), with 2 BIN files for Sample1 and 1 for Sample2. In this case,
`Nb_binfile`=3 and `Nb_sample`=2. The user may then set
`FolderNames=c("Sample1-File1", "Sample1-File2", "Sample2-File1")`, in
which case `"Sample1-1"` is the name of the subfolder containing the
first BIN file for Sample1, `"Sample1-File2"` the name of the subfolder
for the second BIN file of Sample1; eventually, `"Sample2-1"` is the
name of the subfolder containing the BIN file for the second sample. In
this case, `BinPerSample=c(2,1)`.

For the general BIN-file structure, the reader is referred to the
following website: `http://www.nutech.dtu.dk/`

The function
[Luminescence::read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.html)
is used to read the BIN files.

## Note

The function imports only BIN/BINX-file records which have been
previously selected.

## See also

[Luminescence::read_BIN2R](https://r-lum.github.io/Luminescence/reference/read_BIN2R.html),
[`combine_DataFiles`](https://crp2a.github.io/BayLum/dev/reference/combine_DataFiles.md),
[`LT_RegenDose`](https://crp2a.github.io/BayLum/dev/reference/LT_RegenDose-deprecated.md)
[`Age_Computation`](https://crp2a.github.io/BayLum/dev/reference/Age_Computation.md),
[`AgeS_Computation`](https://crp2a.github.io/BayLum/dev/reference/AgeS_Computation.md),
[`Palaeodose_Computation`](https://crp2a.github.io/BayLum/dev/reference/Palaeodose_Computation.md)

## Author

Claire Christophe, Sebastian Kreutzer, Anne Philippe, Guillaume Guérin

## Examples

``` r
if (FALSE) { # \dontrun{
path <- system.file("extdata/FER1", "", package="BayLum")
folder <- ""
# give the number of sample
nbsample <- 1
DATA <- Generate_DataFile_MG(
 Path = path,
 FolderNames = folder,
 Nb_sample = nbsample)
str(DATA)

# to save information in RData object in folder containing bin file
#save(DATA,file=c(paste(path,folder,'DATA.RData',sep="")))
# to load information containing DATA.RData object
#load(file=c(paste(path,folder,"DATA.RData",sep="")))
} # }
```
