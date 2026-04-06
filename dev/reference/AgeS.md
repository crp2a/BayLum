# Output of [`AgeS_Computation`](https://crp2a.github.io/BayLum/dev/reference/AgeS_Computation.md) function for the samples: "GDB5" and "GDB3"

Output of
[`AgeS_Computation`](https://crp2a.github.io/BayLum/dev/reference/AgeS_Computation.md)
function for the samples: "GDB5" and "GDB3", there is no stratigraphic
relation neither systematic errors.

## Usage

``` r
data("AgeS")
```

## Format

A list containing

- `Sampling`:

  MCMC.list that corresponds to a sample of the posterior distributions
  of the ages (in ka), palaeodoses (in Gy) and equivalent dose
  dispersions (in Gy) parameters of samples "GDB5" and "GDB3";

- `Model_GrowthCurve`:

  stating which dose response fitting option was chosen to run the
  function

- `Distribution`:

  stating which distribution was chosen to model the dispersion of
  individual equivalent dose values around the palaeodose of the sample;

- `PriorAge`:

  stating the priors used for the age parameter (in ka);

- `StratiConstraints`:

  stating the matrix of stratigraphic relations between samples
  considered in the model;

- `CovarianceMatrix`:

  stating the covariance matrix of error used in the model, highlighting
  not common errors between samples in our cases (diagonal matrix).

## References

Tribolo, C., Asrat, A., Bahain, J. J., Chapon, C., Douville, E.,
Fragnol, C., Hernandez, M., Hovers, E., Leplongeon, A., Martin, L,
Pleurdeau, D, Pearson, O , Puaud, S, Assefa, Z. (2017). Across the Gap:
Geochronological and Sedimentological Analyses from the Late
Pleistocene-Holocene Sequence of Goda Buticha, Southeastern Ethiopia.
PloS one, 12(1), e0169418.

## Examples

``` r
data(AgeS)
str(AgeS)
#> List of 6
#>  $ Sampling         :List of 3
#>   ..$ : 'mcmc' num [1:2000, 1:6] 6.39 6.17 8.38 7.91 7.47 ...
#>   .. ..- attr(*, "dimnames")=List of 2
#>   .. .. ..$ : NULL
#>   .. .. ..$ : chr [1:6] "A[1]" "A[2]" "D[1]" "D[2]" ...
#>   .. ..- attr(*, "mcpar")= num [1:3] 20005 30000 5
#>   ..$ : 'mcmc' num [1:2000, 1:6] 6.54 6.84 6.81 6.53 6.84 ...
#>   .. ..- attr(*, "dimnames")=List of 2
#>   .. .. ..$ : NULL
#>   .. .. ..$ : chr [1:6] "A[1]" "A[2]" "D[1]" "D[2]" ...
#>   .. ..- attr(*, "mcpar")= num [1:3] 20005 30000 5
#>   ..$ : 'mcmc' num [1:2000, 1:6] 7.31 8.47 6.95 7.37 6.26 ...
#>   .. ..- attr(*, "dimnames")=List of 2
#>   .. .. ..$ : NULL
#>   .. .. ..$ : chr [1:6] "A[1]" "A[2]" "D[1]" "D[2]" ...
#>   .. ..- attr(*, "mcpar")= num [1:3] 20005 30000 5
#>   ..- attr(*, "class")= chr "mcmc.list"
#>  $ Model_GrowthCurve: chr "AgesMultiCS2_EXPLIN"
#>  $ Distribution     : chr "cauchy"
#>  $ PriorAge         : num [1:4] 1 100 1 100
#>  $ StratiConstraints: num [1:3, 1:2] 1 0 0 1 0 0
#>  $ CovarianceMatrix : num [1:2, 1:2] 0.0566 0 0 0.062
```
