# Write BayLum .csv-files

This function allows the user to write all .csv files expected by
[Generate_DataFile](https://crp2a.github.io/BayLum/dev/reference/Generate_DataFile-deprecated.md)
and
[Generate_DataFile_MG](https://crp2a.github.io/BayLum/dev/reference/Generate_DataFile_MG-deprecated.md).
Unlike
[create_FolderTemplates](https://crp2a.github.io/BayLum/dev/reference/create_FolderTemplates.md),
this function makes it possible to write .csv files with all information
directly from R. No further modification of .csv files are required. The
purpose of this function is (i) to reduce tedious manual editing of
.csv-files and the errors that result (ii) to introduce an easy way to
review information inside .csv-files (by revisiting code rather than
opening individual .csv-files) and (iii) to streamline folder and file
creation when preparing data to run BayLum's modelling functions. Note:
the user will still need to move the appropriate .bin-files into all the
sample folders.

## Usage

``` r
write_BayLumFiles(
  folder,
  SampleNames = "Sample_1",
  BinPerSample = rep(1, length(SampleNames)),
  SubSampleNames = NULL,
  DiscPos = NULL,
  DRenv = 1,
  DRenv.error = 0.04,
  DRsource = 0.1,
  DRsource.error = 0.002,
  signal.integral.min = 6,
  signal.integral.max = 10,
  background.integral.min = 346,
  background.integral.max = 395,
  inflatePercent = 0.025,
  nbOfLastCycleToRemove = 2
)
```

## Arguments

- folder:

  [character](https://rdrr.io/r/base/character.html) (*required*\*): The
  name of the main folder in which all subsequent BayLum files and
  folders will be located. This could be a path to an already existing
  folder, or the path/name of a folder to be created.

- SampleNames:

  [character](https://rdrr.io/r/base/character.html) (*required*):
  Vector of sample names.

- BinPerSample:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  Vector of numbers indicating the number of .bin-files per sample.

- SubSampleNames:

  [character](https://rdrr.io/r/base/character.html) (*optional*):
  Vector of names to give each subfolder within a sample when the number
  of .bin-files in a sample counts more than one. If omitted or NULL,
  the subfolders are named by the subfolder count number.

- DiscPos:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*): List
  of data frames with each data frame having one or two columns to
  identify aliquots/grains to be included in the analysis. The first
  column corresponds to the position number, and the second column
  corresponds to the grain number. If the data frame has only one
  column, a Disc.csv will be written. If the data.frame has two columns,
  a DiscPos.csv will be written. The length of the list should be the
  number of .bin-files included.

- DRenv:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  Vector where `DRenv[i]` corresponds to environmental dose rate for
  `.bin-file[i]`. Length should be one or the number of .bin-files
  included in the analysis.

- DRenv.error:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  Vector where `DRenv.error[i]` corresponds to environmental dose rate
  error for `.bin-file[i]`. Length should be one or the number of
  .bin-files included in the analysis.

- DRsource:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  Vector where `DRsource[i]` corresponds to source dose rate for
  `.bin-file[i]`. Length should be one or the number of .bin-files
  included in the analysis.

- DRsource.error:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  Vector where `DRsource.error[i]` corresponds to source dose rate error
  for `.bin-file[i]`. Length should be one or the number of .bin-files
  included in the analysis.

- signal.integral.min:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  Vector where `signal.integral.min[i]` corresponds to the channel
  number where the OSL signal should be summed from for`bin-file[i]`
  Length should be one or the number of .bin-files included in the
  analysis.

- signal.integral.max:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  Vector where `signal.integral.max[i]` corresponds to the channel
  number where the OSL signal should be summed from to for `bin-file[i]`
  Length should be one or the number of .bin-files included in the
  analysis.

- background.integral.min:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  Vector where `background.integral.min[i]` corresponds to the channel
  number where the OSL background signal should be summed from
  for`bin-file[i]` Length should be one or the number of .bin-files
  included in the analysis.

- background.integral.max:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  Vector where `background.integral.max[i]` corresponds to the channel
  number where the OSL background signal should be summed to for
  `.bin-file[i]`. Length should be one or the number of .bin-files
  included in the analysis.

- inflatePercent:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  Vector where `inflatePercent[i]` corresponds to uncertainty due to
  instrumental reproducibility to`bin-file[i]` Length should be one or
  the number of .bin-files included in the analysis.

- nbOfLastCycleToRemove:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  Vector where `nbOfLastCycleToRemove[i]` corresponds to the number of
  regeneration points to remove in analysis for `bin-file[i]` Length
  should be one or the number of .bin-files included in the analysis.

## Value

The function returns nothing, but writes the folder structure.

## Function version

0.1.0

## See also

[Generate_DataFile](https://crp2a.github.io/BayLum/dev/reference/Generate_DataFile-deprecated.md),
[Generate_DataFile_MG](https://crp2a.github.io/BayLum/dev/reference/Generate_DataFile_MG-deprecated.md)

## Author

Frederik Baumgarten, RadPhys, DTU Physics, Technical University of
Denmark (Denmark)

## Examples

``` r
# example samples
SampleNames <- c("OSL-1-MG","OSL-2-SG")

# number of .bin-files for each sample
BinPerSample <- c(1,3)

# List of data.frames of accepted aliquot/grain to be included
# in the analysis for each .bin-file.
DiscPos <- list(
data.frame("position" = 1:48),
data.frame("position" = c(1,1,1,1), "grain" = c(4,67,92,99)),
data.frame("position" = c(2,2,2,2), "grain" = c(7,13,41,72)),
data.frame("position" = c(3,3,3,3), "grain" = c(7,52,67,88)))

# example 1: write to disk (all together)
write_BayLumFiles(
folder = paste(tempdir(),"new_BayLum_folder",sep = "/"),
SampleNames = SampleNames,
BinPerSample = BinPerSample,
DiscPos = DiscPos,
DRenv = c(1.75, 1.52, 1.52, 1.52),
DRenv.error = c(0.04, 0.03, 0.03, 0.03),
DRsource = c(0.2075, 0.1501, 0.1501, 0.1501),
DRsource.error = c(0.0010, 0.0008, 0.0008, 0.0008))

# example 2: write to disk (by sample)
write_BayLumFiles(
folder = paste(tempdir(),"new_BayLum_folder",sep = "/"),
SampleNames = "OSL-1-MG",
BinPerSample = 1,
DiscPos = DiscPos[[1]],
DRenv = 1.75,
DRenv.error = 0.04,
DRsource = 0.2075,
DRsource.error = 0.0010)

write_BayLumFiles(
folder = paste(tempdir(),"new_BayLum_folder",sep = "/"),
SampleNames = "OSL-2-SG",
BinPerSample = 3,
DiscPos = DiscPos[2:4],
DRenv = 1.75,
DRenv.error = 0.04,
DRsource = 0.2075,
DRsource.error = 0.0010)
```
