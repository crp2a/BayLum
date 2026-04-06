# JAGS models use in `AgeC14_Computation`

A list of JAGS models use to a Bayesian analysis of C14 calibration age
of various sample. Stratigraphic relations can be taken in count to
calibrate C14 ages. This ages take into account that some data can be an
outlier.

## Usage

``` r
data("Model_AgeC14")
```

## Format

This list contains:

- `full`:

  a model considering error on calibration curve.

- `naive`:

  a model not considering error on calibration curve.

## References

Reimer PJ, Bard E, Bayliss A, Beck JW, Blackwell PC, Bronl Ramsey C,
Buck CE, Cheng H, Edwards RL, Friedrich M, Grootes PM, Guilderson TP,
Haflidason H, Hajdas I, Hatte C, Heaton TJ, Hoffmann DL, Hogg AG, Hughen
KA, Kaiser KF, Kromer B,Manning SW, Niu M, Reimer RW, Richards DA, Scott
EM, Southon JR, Staff RA, Turney CSM, van der Plicht J. 2013. IntCal13
ans Marine13 radiocarbon age calibration curves 0-50000 years cal BP.
Radiocarbon 55(4)=1869-1887.

Hogg AG, Hua Q, Blackwell PG, Niu M, Buck CE, Guilderson TP, Heaton TJ,
Palmer JG, Reimer PJ, Reimer RW, Turney CSM, Zimmerman SRH. 2013.
SHCal13 Southern Hemisphere calibration, 0-50000 years cal BP.
Radiocarbon 55(4):1889-1903

## See also

[rjags::rjags-package](https://rdrr.io/pkg/rjags/man/rjags-package.html)

## Examples

``` r
data(Model_AgeC14)
writeLines(Model_AgeC14$full)
#> model{
#>   # vraisemblance
#>   for(i in 1:N){
#>     X[i] ~ dnorm(mu[i], prec[i])
#>     mu[i] <- interp.lin(Age[i], xTableauCalib, yTableauCalib)
#>     Z[i]~dcat(c(0.1,0.9))
#>     err[i] <- interp.lin(Age[i], xTableauCalib, zTableauCalib)
#>     prec[i] <- 1/(alpha[i]^(-Z[i]+2)*(pow(sigma[i],2)+pow(err[i],2)))
#>   }
#>   # a priori
#>   Age[1]~dunif(xbound[1],xbound[2])
#>   invalpha[1]~dgamma(3,4)
#>   alpha[1]<-1/invalpha[1]
#>   for(j in 2:N){
#>     amin[j]<-max(StratiConstraints[1:j,j]*c(xbound[(2*(j-1)+1)],Age[1:(j-1)]))
#>     Age[j]~dunif(amin[j],xbound[2*j])
#>     invalpha[j]~dgamma(3,4)
#>     alpha[j]<-1/invalpha[j]
#>     }
#>   }
```
