# JAGS models use in `Palaeodose_Computation`

A list of JAGS models use to a Bayesian analysis of OSL palaeodose of
one or various samples. There are models for various growth curves and
various distrubution to describe equivalent dose distribution around the
palaeodose.

## Usage

``` r
data("Model_Palaeodose")
```

## Format

This list contains:

- `PalaeodosesMultiBF_EXPLIN`:

  a list of 4 models that all consider a saturating exponential plus
  linear growth. These 4 models have different distribution to describe
  equivalent dose values around the palaeodose.

- `PalaeodosesMultiBF_EXP`:

  a list of 4 models that all consider a saturating exponential growth.
  These 4 models have different distribution to describe equivalent dose
  values around the palaeodose.

- `PalaeodosesMultiBF_EXPZO`:

  a list of 4 models that all consider a saturating exponential plus
  linear growth and fitting through the origin. These 4 models have
  different distribution to describe equivalent dose values around the
  palaeodose.

- `PalaeodosesMultiBF_EXPLINZO`:

  a list of 4 models that all consider a saturating exponential growth
  and fitting through the origin. These 4 models have different
  distribution to describe equivalent dose values around the palaeodose.

## Details

The different distibutions to describe equivalent dose values around the
palaeodose are:

- `cauchy`:

  a Cauchy distribution with postition parameter equal to the palaeodose
  of the sample

- `gaussian`:

  a Gaussian distribution with mean equal to the palaeodose of the
  sample

- `lognormal_A`:

  a log-normal distribution with mean or **A**verage equal to the
  palaeodose of the sample

- `lognormal_M`:

  a log-normal distribution with **M**edian equal to the palaeodose of
  the sample

For more information we refer to the function
[`Palaeodose_Computation`](https://crp2a.github.io/BayLum/dev/reference/Palaeodose_Computation.md),
section Details.

## References

Plummer, M. (2003). JAGS: A program for analysis of Bayesian graphical
models using Gibbs sampling. In Proceedings of the 3rd international
workshop on distributed statistical computing, volume 124, page 125.
Technische Universit at Wien, Austria.

Plummer, M. (2015). JAGS Version 4.0. 0 user manual.

## See also

[rjags::rjags-package](https://rdrr.io/pkg/rjags/man/rjags-package.html)

## Examples

``` r
data(Model_Palaeodose)
writeLines(Model_Palaeodose$PalaeodosesMultiBF_EXPLIN$gaussian)
#> model {
#> 
#>   for(i0 in 1:I){
#>   D[i0]~dunif(xbound[1],xbound[2])
#>   sD[i0]~dt(0,pow(0.16*D[i0],-2),1)T(0,)
#>   pD[i0]<-pow(sD[i0],-2)
#>   }
#> 
#>   # Likelihood:
#>   for(i in 1:I){
#>   for(bf in (CSBinPerSample[i]-BinPerSample[i]+1):(CSBinPerSample[i])){
#>   for(j in 1:J[bf]){
#>   # prior on growth function
#>   xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
#>   xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
#>   xc[(index[bf]+j)]~dnorm(0.002,1/(0.01^2))T(0,)
#>   xd[(index[bf]+j)]~dnorm(0.5,1/(2.5^2))T(-xa[(index[bf]+j)],)
#>   sigmaf[(index[bf]+j)]~dexp(20)
#> 
#>   De[(index[bf]+j),1]~dnorm(D[i],pD[i])
#>   #
#>   xprecision[(index[bf]+j),1]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),1]^2) ##<-- ???? sN[j,1]^2 ????
#>   N[(index[bf]+j),1]~dnorm(xQ[(index[bf]+j),1],xprecision[(index[bf]+j),1])
#>   xQ[(index[bf]+j),1]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),1]/xb[(index[bf]+j)]))+xc[(index[bf]+j)]*De[(index[bf]+j),1]+xd[(index[bf]+j)]
#> 
#>   for(k in 2:K[bf]){
#>   xprecision[(index[bf]+j),k]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),k]^2)
#>   N[(index[bf]+j),k]~dnorm(xQ[(index[bf]+j),k],xprecision[(index[bf]+j),k])
#>   xQ[(index[bf]+j),k]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),k]/xb[(index[bf]+j)]))+xc[(index[bf]+j)]*De[(index[bf]+j),k]+xd[(index[bf]+j)]
#>   De[(index[bf]+j),k]<-IT[(index[bf]+j),(k-1)]*sDlab[bf]
#>   }
#>   }
#>   }
#>   }
#> 
#>   }
```
