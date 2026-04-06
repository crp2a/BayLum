# Construct the Stratigraphic Constrain Matrix Interactively

This function helps to define the stratigraphic relation between samples
using questions. The output of this function can be used in the function
[AgeS_Computation](https://crp2a.github.io/BayLum/dev/reference/AgeS_Computation.md).

## Usage

``` r
SCMatrix(DATA = NULL, Nb_sample, SampleNames)
```

## Arguments

- DATA:

  `BayLum.list` (*with default*): Object of class `BayLum.list`, if
  provided the other parameters are not any longer mandatory.

- Nb_sample:

  [integer](https://rdrr.io/r/base/integer.html) (**required**): the
  sample number, if `DATA` is provided, the input is not required

- SampleNames:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  sample names, if `DATA` is provided, the input is not required

## Value

Returns a [matrix](https://rdrr.io/r/base/matrix.html) that summarise
the ordered relation between samples. This matrix can be integrate in
[AgeS_Computation](https://crp2a.github.io/BayLum/dev/reference/AgeS_Computation.md)
function. We refer to detail on
[AgeS_Computation](https://crp2a.github.io/BayLum/dev/reference/AgeS_Computation.md)
for more information concerning this matrix.

## Details

The function will ask if sample `i` is younger than sample `j` to
construct the stratigraphic constrain matrix.

## See also

[AgeS_Computation](https://crp2a.github.io/BayLum/dev/reference/AgeS_Computation.md)

## Author

Claire Christophe, Anne Philippe, Guillaume Guérin, Sebastian Kreutzer

## Examples

``` r
if (FALSE) { # \dontrun{
SCMatrix(
 Nb_sample = 2,
 SampleNames = c("sample1","sample2"))
} # }
```
