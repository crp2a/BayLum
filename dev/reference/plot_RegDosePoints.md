# Plot Regeneration Dose Points

Simple plot functionality to visualise \$L_x/T_x\$ values against the
dose extracted from data created by
[create_DataFile](https://crp2a.github.io/BayLum/dev/reference/create_DataFile.md)

## Usage

``` r
plot_RegDosePoints(object, nrow = 3L, ncol = nrow, ...)
```

## Arguments

- object:

  [list](https://rdrr.io/r/base/list.html) (**required**): input object
  created by
  [create_DataFile](https://crp2a.github.io/BayLum/dev/reference/create_DataFile.md)

- nrow:

  [integer](https://rdrr.io/r/base/integer.html) (*with default*):
  number of rows used for the plot panel

- ncol:

  [integer](https://rdrr.io/r/base/integer.html) (*with default*):
  number of columns in the plot panel

- ...:

  further plot arguments passed down to modify the plot output.
  Supported arguments are `xlab`, `ylab`, `type`, `pch`, `col`, `cex`

## Value

The function returns a plot

## See also

[create_DataFile](https://crp2a.github.io/BayLum/dev/reference/create_DataFile.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany)

## Examples

``` r
data(DATA3,envir = environment())
plot_RegDosePoints(DATA3)


```
