# Create Stratigraphically Ordered Sample Matrix

Construct the stratigraphic matrix used in the functions
[AgeS_Computation](https://crp2a.github.io/BayLum/dev/reference/AgeS_Computation.md)
and
[AgeC14_Computation](https://crp2a.github.io/BayLum/dev/reference/AgeC14_Computation.md)
for samples that are all ordered by increasing age.

## Usage

``` r
SC_Ordered(Nb_sample)
```

## Arguments

- Nb_sample:

  [integer](https://rdrr.io/r/base/integer.html) (**required**): the
  number of samples; alternatively an object of class `BayLum.list` can
  be provided as input (such as produced by
  [create_DataFile](https://crp2a.github.io/BayLum/dev/reference/create_DataFile.md))

## Value

Stratigraphic matrix where each sample are ordered by increasing order.
This matrix can be integrated in the function
[AgeS_Computation](https://crp2a.github.io/BayLum/dev/reference/AgeS_Computation.md).
Please see
[AgeS_Computation](https://crp2a.github.io/BayLum/dev/reference/AgeS_Computation.md)
for more information on this matrix.

## See also

[AgeS_Computation](https://crp2a.github.io/BayLum/dev/reference/AgeS_Computation.md),
[SCMatrix](https://crp2a.github.io/BayLum/dev/reference/SCMatrix.md)

## Author

Claire Christophe, Anne Philippe, Sebastian Kreutzer, Guillaume Guérin

## Examples

``` r
SC <- SC_Ordered(Nb_sample = 3)
```
