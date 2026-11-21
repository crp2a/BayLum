# MCMC sample from the posterior distribution of the dataset GDB5

MCMC samples from the posterior distribution of "A" for age, "D" for
palaeodose and "sD" for dispersion of equivalent doses around "D", of
the data set GDB5.

## Usage

``` r
data("MCMCsample")
```

## Format

It is a matric with 6000 row and tree column.

- `A`:

  The first column of the matrice are sampled from the posterior
  distribution of the paramete `A`

- `D`:

  The first column of the matrice are sampled from the posterior
  distribution of the paramete `D`

- `sD`:

  The first column of the matrice are sampled from the posterior
  distribution of the paramete `sD`

## References

Tribolo, C., Asrat, A., Bahain, J. J., Chapon, C., Douville, E.,
Fragnol, C., Hernandez, M., Hovers, E., Leplongeon, A., Martin, L,
Pleurdeau, D, Pearson, O , Puaud, S, Assefa, Z. (2017). Across the Gap:
Geochronological and Sedimentological Analyses from the Late
Pleistocene-Holocene Sequence of Goda Buticha, Southeastern Ethiopia.
PloS one, 12(1), e0169418.

## Examples

``` r
data(MCMCsample)
## maybe str(MCMCsample) ; plot(MCMCsample[,1],type="l") ...
```
