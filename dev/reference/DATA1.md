# DATA of sample named GDB3

list of objects: LT, sLT, ITimes, dLab, ddot_env, regDose,
J,K,Nb_measurement obtained using
[`Generate_DataFile`](https://crp2a.github.io/BayLum/dev/reference/Generate_DataFile-deprecated.md)
function with single-grain OSL measurementsl of the sample GDB3.

## Usage

``` r
data("DATA1")
```

## Format

A list containing:

- `LT:`:

  (one list per sample): each list contains all L/T values for the
  corresponding sample;

- `sLT:`:

  (one list per sample): each list contains all uncertainties on L/T
  values for the corresponding sample;

- `ITimes:`:

  (one list per sample): each list contains irradiation time values for
  the corresponding sample;

- `dLab=`:

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

  a vector giving, for each BIN file, the number of measurements;

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
data(DATA1)
str(DATA1)
#> List of 11
#>  $ LT            :List of 1
#>   ..$ : num [1:101, 1:6] 5.66 6.9 4.05 3.43 4.97 ...
#>  $ sLT           :List of 1
#>   ..$ : num [1:101, 1:6] 0.373 0.315 0.245 0.181 0.246 ...
#>  $ ITimes        :List of 1
#>   ..$ : num [1:101, 1:5] 160 160 160 160 160 160 160 160 160 160 ...
#>  $ dLab          : num [1:2, 1] 1.53e-01 5.89e-05
#>  $ ddot_env      : num [1:2, 1] 2.26 0.0617
#>  $ regDose       :List of 1
#>   ..$ : num [1:101, 1:5] 24.6 24.6 24.6 24.6 24.6 ...
#>  $ J             : num 101
#>  $ K             : num 5
#>  $ Nb_measurement: num 14
#>  $ SampleNames   : chr "samp 1"
#>  $ Nb_sample     : num 1
#>  - attr(*, "originator")= chr "create_DataFile"
```
