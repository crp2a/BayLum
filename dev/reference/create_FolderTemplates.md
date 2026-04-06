# Create Folder Templates

Create file and folder structure templates on the user hard drive as
expected by
[Generate_DataFile](https://crp2a.github.io/BayLum/dev/reference/Generate_DataFile-deprecated.md)
and
[Generate_DataFile_MG](https://crp2a.github.io/BayLum/dev/reference/Generate_DataFile_MG-deprecated.md).
Files and data in the folders must then be overwritten manually with
user data. The function intends to minimise the errors going along with
the creation of these folder structures. The function uses the example
data of `BayLum` to create the templates.

## Usage

``` r
create_FolderTemplates(
  path,
  mode = "SG",
  n_folders = 1,
  names = paste("Sample_", 1:n_folders),
  verbose = TRUE
)
```

## Arguments

- path:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  path to the folder where the templates should be created

- mode:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  depending on the dataset you can create templates or single grain
  (`SG`) or multi-grain (`MG`) data

- n_folders:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  number of template folders to be created

- names:

  [character](https://rdrr.io/r/base/character.html) (*optional*):
  allows give own names to the subfolders.

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enables/disables verbose mode

## Value

If the templates were created successfully on the hard drive, the
function returns nothing.

## Function version

0.1.0

## See also

[Generate_DataFile](https://crp2a.github.io/BayLum/dev/reference/Generate_DataFile-deprecated.md),
[Generate_DataFile_MG](https://crp2a.github.io/BayLum/dev/reference/Generate_DataFile_MG-deprecated.md)

## Author

Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University
(United Kingdom)

## Examples

``` r
create_FolderTemplates(tempdir())
#> 
#> [create_FolderTemplates()]
#> -|
#>  |__(dir created:) /tmp/RtmpuRrOip 
#> All templates created. Please modify the parameters according to your data!

```
