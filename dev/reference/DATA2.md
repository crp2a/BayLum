# DATA on sample named GDB5

list of objects: LT, sLT, ITimes, dLab, ddot_env, regDose,
J,K,Nb_measurement obtained using
[`Generate_DataFile`](https://crp2a.github.io/BayLum/dev/reference/Generate_DataFile-deprecated.md)
function with single-grain OSL measurementsl of the sample GDB5.

## Usage

``` r
data("DATA2")
```

## Format

A data frame containing:

- `LT:`:

  (one list per sample): each list contains all L/T values for the
  corresponding sample;

- `sLT:`:

  (one list per sample): each list contains all uncertainties on L/T
  values for the corresponding sample;

- `ITimes:`:

  (one list per sample): each list contains irradiation time values for
  the corresponding sample;

- `dLab:`:

  a matrix containing in line `i`, the laboratory dose rate and its
  variance for sample `i`;

- `ddot_env:`:

  a matrix containing in line `i`, the environmental dose rate and its
  variance (excluding the common error terms) for sample `i`;

- `regDose:`:

  (one list per sample): each list contains all regenerated doses;

- `J:`:

  a vector giving, for each BIN file, the number of aliquots selected
  for the analysis;

- `K:`:

  a vector giving, for each BIN file, the number of regenerative doses
  in the SAR protocol;

- `Nb_measurement:`:

  , a vector giving, for each BIN file, the number of measurements;

## References

For more informations on this sample we refer to the following
publication:

Tribolo, C., Asrat, A., Bahain, J. J., Chapon, C., Douville, E.,
Fragnol, C., Hernandez, M., Hovers, E., Leplongeon, A., Martin, L,
Pleurdeau, D, Pearson, O , Puaud, S, Assefa, Z. (2017). Across the Gap:
Geochronological and Sedimentological Analyses from the Late
Pleistocene-Holocene Sequence of Goda Buticha, Southeastern Ethiopia.
PloS one, 12(1), e0169418.

## Examples

``` r
data(DATA2)
str(DATA2)
#> List of 11
#>  $ LT            :List of 1
#>   ..$ : num [1:188, 1:6] 4.54 2.73 2.54 2.27 1.48 ...
#>  $ sLT           :List of 1
#>   ..$ : num [1:188, 1:6] 0.333 0.386 0.128 0.171 0.145 ...
#>  $ ITimes        :List of 1
#>   ..$ : num [1:188, 1:5] 40 40 40 40 40 40 40 40 40 40 ...
#>  $ dLab          : num [1:2, 1] 1.53e-01 5.89e-05
#>  $ ddot_env      : num [1:2, 1] 2.512 0.0563
#>  $ regDose       :List of 1
#>   ..$ : num [1:188, 1:5] 6.14 6.14 6.14 6.14 6.14 6.14 6.14 6.14 6.14 6.14 ...
#>  $ J             : num 188
#>  $ K             : num 5
#>  $ Nb_measurement: num 14
#>  $ SampleNames   : chr "samp 1"
#>  $ Nb_sample     : num 1
#>  - attr(*, "originator")= chr "create_DataFile"
```
