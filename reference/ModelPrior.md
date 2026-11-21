# Prior for JAGS models use in `Age_OSLC14`

A list to define prior in JAGS models, taking acount OSL data and C14
data in stratigraphic constraint. The difficulty is in the fact that
each cases is different. The youngest sample can be a C14 as well as a
OSL sample. To resolve this problem we consider diferent cases thanks to
this list.

## Usage

``` r
data("ModelPrior")
```

## Format

This list contains:

- `Sample1_C14`:

  model considering that the youngest sample is a C14 sample

- `Sample1_OSL`:

  model considering that the youngest sample is a OSL sample

- `C14_OSL`:

  model considering that the second sample is a C14 sample

- `OSL_C14`:

  model considering that the second sample is a OSL sample

- `C14`:

  model considering that the last sample is a C14 sample

- `OSL`:

  model considering that the last sample is a OSL sample

## References

Plummer, M. (2003). JAGS: A program for analysis of Bayesian graphical
models using Gibbs sampling. In Proceedings of the 3rd international
workshop on distributed statistical computing, volume 124, page 125.
Technische Universit at Wien, Austria.

Plummer, M. (2015). JAGS Version 4.0. 0 user manual.

## Examples

``` r
data(ModelPrior)
## ModelPrior[[OSL]]
writeLines(ModelPrior$OSL)
#> 
#>       # donnee OSL
#>       kk<-k+1
#>       for(i in (ind_change[2*q+1]+1):ind_change[2*(q+1)]){
#>       u[CS_OSL[i]]~dunif(0,1)
#>       CS[i]<-max(StratiConstraints[(1:i),i]*c(xbound[(2*i-1)],A[1:(i-1)]))
#>       A[i]<-exp(u[CS_OSL[i]]*log(xbound[2*(i-1)]/CS[i])+log(CS[i]))
#>       }
#>       
```
