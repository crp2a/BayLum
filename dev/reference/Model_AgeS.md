# JAGS models use in `AgeS_Computation`

A list of JAGS models use to a Bayesian analysis of OSL age of various
samples. There are models for various growth curves and various
distrubution to describe equivalent dose distribution around the
palaeodose.

## Usage

``` r
data("Model_AgeS")
```

## Format

This list contains:

- `AgesMultiCS2_EXPLIN`:

  a list of 4 models that all consider a saturating exponential plus
  linear growth. These 4 models have different distribution to describe
  equivalent dose values around the palaeodose.

- `AgesMultiCS2_EXP`:

  a list of 4 models that all consider a saturating exponential growth.
  These 4 models have different distribution to describe equivalent dose
  values around the palaeodose.

- `AgesMultiCS2_EXPZO`:

  a list of 4 models that all consider a saturating exponential plus
  linear growth and fitting through the origin. These 4 models have
  different distribution to describe equivalent dose values around the
  palaeodose.

- `AgesMultiCS2_EXPLINZO`:

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
[`AgeS_Computation`](https://crp2a.github.io/BayLum/dev/reference/AgeS_Computation.md),
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
data(Model_AgeS)
## The JAGS model for a saturating exponential plus linear growth
## (a function of the type \code{f(x)=a(1-exp(-x/b))+cx+d})
## and a gaussian distribution of equivalent doses around the palaeodose:
writeLines(Model_AgeS$AgesMultiCS2_EXP$gaussian)
#> model {
#> 
#>   D~dmnorm(mu,omega)
#>   for(i1 in 1:I){
#>   sD[i1]~dt(0,pow(0.16*D[i1],-2),1)T(0,)
#>   pD[i1]<-pow(sD[i1],-2)
#>   mu[i1]<-A[i1]*ddot[i1]
#>   for(i2 in 1:I){
#>   Sigma[i1,i2]=A[i1]*A[i2]*Gamma[i1,i2]
#>   }
#>   }
#>   omega<-inverse(Sigma)
#> 
#>   # Likelihood:
#>   for(i in 1:I){
#>   for(bf in (CSBinPerSample[i]-BinPerSample[i]+1):(CSBinPerSample[i])){
#>   for(j in 1:J[bf]){
#>   # prior on growth function
#>   xa[(index[bf]+j)]~dnorm(6.5,1/(9.2^2))T(0,)
#>   xb[(index[bf]+j)]~dnorm(50,1/(1000^2))T(0,)
#>   xd[(index[bf]+j)]~dnorm(0.5,1/(2.5^2))T(-xa[(index[bf]+j)],)
#>   sigmaf[(index[bf]+j)]~dexp(20)
#> 
#>   De[(index[bf]+j),1]~dnorm(D[i],pD[i])
#>   #
#>   xprecision[(index[bf]+j),1]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),1]^2) ##<-- ???? sN[j,1]^2 ????
#>   N[(index[bf]+j),1]~dnorm(xQ[(index[bf]+j),1],xprecision[(index[bf]+j),1])
#>   xQ[(index[bf]+j),1]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),1]/xb[(index[bf]+j)]))+xd[(index[bf]+j)]
#> 
#>   for(k in 2:K[bf]){
#>   xprecision[(index[bf]+j),k]<-1/(sigmaf[(index[bf]+j)]^2+sN[(index[bf]+j),k]^2)
#>   N[(index[bf]+j),k]~dnorm(xQ[(index[bf]+j),k],xprecision[(index[bf]+j),k])
#>   xQ[(index[bf]+j),k]<-xa[(index[bf]+j)]*(1-exp(-De[(index[bf]+j),k]/xb[(index[bf]+j)]))+xd[(index[bf]+j)]
#>   De[(index[bf]+j),k]<-IT[(index[bf]+j),(k-1)]*sDlab[bf]
#>   }
#>   }
#>   }
#>   }
#> 
#>   Atemp[1]=xbound[1]
#>   # i0=2
#>   u[1]~dunif(0,1)
#>   CS[1]=xbound[1]
#>   Atemp[2]<-exp(u[1]*log(xbound[2]/CS[1])+log(CS[1]))
#>   # i0>2
#>   for(i0 in 3:(I+1)){
#>   u[i0-1]~dunif(0,1)
#>   CS[i0-1]=max(StratiConstraints[(1:(i0-1)),(i0-1)]*c(xbound[(2*(i0-1)-1)],Atemp[2:(i0-1)]))
#>   Atemp[i0]<-exp(u[(i0-1)]*log(xbound[2*(i0-1)]/CS[i0-1])+log(CS[i0-1]))
#>   }
#>   A=Atemp[2:(I+1)]
#>   }
```
