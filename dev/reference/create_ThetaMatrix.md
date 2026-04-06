# Create Theta Matrix

Create the \\\Theta\\ matrix with the shared uncertainties that can used
as input in, e.g.,
[AgeS_Computation](https://crp2a.github.io/BayLum/dev/reference/AgeS_Computation.md)
and
[Age_OSLC14](https://crp2a.github.io/BayLum/dev/reference/Age_OSLC14.md)
which is used for the covariance matrix \\\Sigma\\ (Combès & Philippe,
2017)

## Usage

``` r
create_ThetaMatrix(input, output_file = NULL, sigma_s, ...)
```

## Arguments

- input:

  [character](https://rdrr.io/r/base/character.html) or
  [data.frame](https://rdrr.io/r/base/data.frame.html) (*optional*):
  input data frame or file connection to import a CSV-file with the
  needed information. If nothing is provided the function returns an
  input template. The argument `output_file` can be used to write this
  input template to the file system

- output_file:

  [character](https://rdrr.io/r/base/character.html) (optional): file
  path for the output CSV-file, the field separator is hard set to
  `","`. Please use
  [utils::write.table](https://rdrr.io/r/utils/write.table.html) for
  more flexibility.

- sigma_s:

  [numeric](https://rdrr.io/r/base/numeric.html) (**required**): named
  character with values for systematic uncertainties. Those values are
  lab-specific. Can be set to `NULL` to remove systematic uncertainties.
  The order of the *named* vector is not important, but the naming!
  **Note**: some of the uncertainties have a unit, please check details.

- ...:

  further arguments that can be passed to
  [utils::read.table](https://rdrr.io/r/utils/read.table.html) (for the
  CSV-file import)

## Value

A symmetric \\Theta\\ matrix or if `input` is missing, a
[data.frame](https://rdrr.io/r/base/data.frame.html) with an input
template

## Details

The function intends to ease the creation of the \\Theta\\ matrix, which
cannot be created straight forward, e.g., base R functions such as
[`stats::cov`](https://rdrr.io/r/stats/cor.html). The relationship
between the covariance matrix \\Sigma\\ and \\Theta\\ is given with

\$\$\Sigma_ij = A_i \* A_j \* \Theta_ij\$\$

For details see Combès & Philippe, 2017 and Guérin et al. (2021).

**Input modes**

The function supports two different operation modes:

1.  `input` is left empty: the function returns a
    [data.frame](https://rdrr.io/r/base/data.frame.html) template that
    can be used as input (the option `output_file` works as well)

2.  `input` is fed with a
    [data.frame](https://rdrr.io/r/base/data.frame.html) or a
    `character` (file path), the \\\Theta\\ matrix is returned

**Input format**

The function expects either a CSV-file or a
[data.frame](https://rdrr.io/r/base/data.frame.html) as input. To create
template you can run the function leaving the argument `input` empty
(see example). Please note the format of the input table
([data.frame](https://rdrr.io/r/base/data.frame.html)) needs to kept as
specified in the template.

The following table lists the meaning of the columns:

|                  |                                      |       |
|------------------|--------------------------------------|-------|
| COLUMN           | DESCRIPTION                          | UNIT  |
| `SAMPLE_ID`      | sample name                          | \-    |
| `DR_BETA_K`      | standard error beta-dose rate K      | Gy/ka |
| `DR_BETA_U`      | standard error beta-dose rate U      | Gy/ka |
| `DR_BETA_Th`     | standard error beta-dose rate Th     | Gy/ka |
| `DR_GAMMA_K`     | standard error gamma-dose rate K     | Gy/ka |
| `DR_GAMMA_U`     | standard error gamma-dose rate U     | Gy/ka |
| `DR_GAMMA_Th`    | standard error gamma-dose rate Th    | Gy/ka |
| `DR_GAMMA_TOTAL` | standard error total gamma-dose rate | Gy/ka |
| `DR_TOTAL`       | total dose rate                      | Gy/ka |
| `DR_TOTAL_X`     | standard error total dose rate       | Gy/ka |

*Note: All columns can be set to 0 or `NA` but no column must be left
empty! If a value \> 0 is provided for `DR_GAMMA_TOTAL` this value is
taken and values in, e.g., `DR_GAMMA_K` are discarded (set to 0)!*

**Systematic uncertainties**

The following table provides information on the named argument that can
be provided via the argument `sigma_s`. Missing values are not allowed,
all values must be set.

|             |                                              |       |
|-------------|----------------------------------------------|-------|
| ARGUMENT    | DESCRIPTION                                  | UNIT  |
| `s_betaK`   | relative uncertainty K concentration         | \-    |
| `s_betaU`   | relative uncertainty U concentration         | \-    |
| `s_betaTh`  | relative uncertainty Th concentration        | \-    |
| `s_gammaK`  | relative uncertainty K concentration         | \-    |
| `s_gammaU`  | relative uncertainty U concentration         | \-    |
| `s_gammaTh` | relative uncertainty Th concentration        | \-    |
| `s_gammaDR` | relative uncertainty gamma-dose rate         | \-    |
| `s_CAL`     | relative uncertainty beta-source calibration | \-    |
| `s_intDR`   | absolute uncertainty internal dose rate      | Gy/ka |

## Function version

0.1.0

## References

Combès, B., Philippe, A., 2017. Bayesian analysis of individual and
systematic multiplicative errors for estimating ages with stratigraphic
constraints in optically stimulated luminescence dating. Quaternary
Geochronology 39, 24–34.
[doi:10.1016/j.quageo.2017.02.003](https://doi.org/10.1016/j.quageo.2017.02.003)

Guérin, G., Lahaye, C., Heydari, M., Autzen, M., Buylaert, J.-P.,
Guibert, P., Jain, M., Kreutzer, S., Lebrun, B., Murray, A.S., Thomsen,
K.J., Urbanova, P., Philippe, A., 2021. Towards an improvement of
optically stimulated luminescence (OSL) age uncertainties: modelling OSL
ages with systematic errors, stratigraphic constraints and radiocarbon
ages using the R package BayLum. Geochronology 3, 229—245.
[doi:10.5194/gchron-3-229-2021](https://doi.org/10.5194/gchron-3-229-2021)

## See also

[AgeS_Computation](https://crp2a.github.io/BayLum/dev/reference/AgeS_Computation.md),
[Age_OSLC14](https://crp2a.github.io/BayLum/dev/reference/Age_OSLC14.md),
[utils::read.table](https://rdrr.io/r/utils/read.table.html),
[utils::write.table](https://rdrr.io/r/utils/write.table.html)

## Author

Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS-Université Bordeaux
Montaigne (France), based on an 'MS Excel' sheet by Guillaume Guérin,
IRAMAT-CRP2A, UMR 5060, CNRS-Université Bordeaux Montaigne (France)

## Examples

``` r
##(1) return template data.frame (no file output)
create_ThetaMatrix()
#> [create_ThetaMatrix()] 'input' missing, input template returned!
#>   SAMPLE_ID DR_BETA_K DR_BETA_U DR_BETA_TH DR_GAMMA_K DR_GAMMA_U DR_GAMMA_TH
#> 1        NA        NA        NA         NA         NA         NA          NA
#>   DR_GAMMA_TOTAL DR_TOTAL DR_TOTAL_X
#> 1             NA       NA         NA

if (FALSE) { # \dontrun{
##(2) return template as data.frame + file
file_path <- tempfile(fileext = ".csv")
create_ThetaMatrix(output_file = file_path )

##NOT RUNNING EXAMPLE for sigma_s
calc_ThetaMatrix(...,
sigma_s =  c(
 s_betaK = 0.010,
 s_betaU = 0.007,
 s_betaTh = 0.006,
 s_gammaK = 0.010,
 s_gammaU = 0.007,
 s_gammaTh = 0.006,
 s_gammaDR = 0.05,
 s_CAL = 0.020,
 s_intDR = 0.030))

} # }


```
