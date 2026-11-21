# C14 cal age estiamte and its error

C14 cal age estiamtes and theirs error of samples S-EVA-26510,
S-EVA-26506, S-EVA-26507, S-EVA-26508.

## Usage

``` r
data("DATA_C14")
```

## Format

A list containing:

- `Names:`:

  character vector of the sample names;

- `C14:`:

  numeric matrix, in the first column the 14C Cal age of the samples,
  and in the second column theirs errors.

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
data(DATA_C14)
(DATA_C14)
#> $Names
#> [1] "S-EVA-26510" "S-EVA-26506" "S-EVA-26507" "S-EVA-26508"
#> 
#> $C14
#>       [,1] [,2]
#> [1,] 37379  382
#> [2,] 43369  291
#> [3,] 42153  652
#> [4,] 42367  678
#> 
```
