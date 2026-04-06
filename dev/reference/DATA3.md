# DATA of sample named FER1

list of objects: LT, sLT, ITimes, dLab, ddot_env, regDose,
J,K,Nb_measurement obtained using
[`Generate_DataFile`](https://crp2a.github.io/BayLum/dev/reference/Generate_DataFile-deprecated.md)
function with multi-grain OSL measurementsl of the sample FER1.

## Usage

``` r
data("DATA3")
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

Guerin, G., Frouin, M., Talamo, S., Aldeias, V., Bruxelles, L., Chiotti,
L., Goldberg, P., Hublin, J.J., Jain, M., Lahaye, C., Madelaine, S.,
Maureille, B., McPherron, S., Mercier, N., Murray, A., Sandgathe, D.,
Steele, T., Thomsen, K., Turq, A. (2015). A multi-method luminescence
dating of the Palaeolithic sequence of La Ferrassie based on new
excavations adjacent to the La Ferrassie 1 and 2 skeletons. Journal of
Archaeological Science, 58, 147-166.

## Examples

``` r
data(DATA3)
str(DATA3)
#> List of 13
#>  $ LT            :List of 1
#>   ..$ : num [1:10, 1:6] 6.91 5.97 7.26 4.63 10.29 ...
#>  $ sLT           :List of 1
#>   ..$ : num [1:10, 1:6] 0.0744 0.035 0.0474 0.1428 0.0451 ...
#>  $ ITimes        :List of 1
#>   ..$ : num [1:10, 1:5] 400 400 400 400 400 400 400 400 400 400 ...
#>  $ dLab          : num [1:2, 1] 0.081 0.0004
#>  $ ddot_env      : num [1:2, 1] 1.93 0.00548
#>  $ regDose       :List of 1
#>   ..$ : num [1:10, 1:5] 32.4 32.4 32.4 32.4 32.4 32.4 32.4 32.4 32.4 32.4 ...
#>  $ J             : num 10
#>  $ K             : num 5
#>  $ Nb_measurement: num 16
#>  $ SampleNames   : chr "samp 1"
#>  $ Nb_sample     : num 1
#>  $ SampleNames   : chr "samp 1"
#>  $ Nb_sample     : num 1
#>  - attr(*, "originator")= chr "create_DataFile"
```
