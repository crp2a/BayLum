# Marine data for calibration of 14C age

As 14C years is not equal to calendar years because atmospheric 14C
concentration varies through time. Hence, data in marine_CalC14 allows a
calibration for hypothetical "global" marine reservoir.

## Usage

``` r
data("Marine13")
```

## Format

A data frame with 3 variables.

- `CAL.BP`:

  a numeric vector correpondig to calendar years befor present

- `X14C.age`:

  a numeric vector correponding to 14C age

- `Error`:

  a numeric vector correponding to error arround 14C age measurement

## References

Reimer PJ, Bard E, Bayliss A, Beck JW, Blackwell PC, Bronl Ramsey C,
Buck CE, Cheng H, Edwards RL, Friedrich M, Grootes PM, Guilderson TP,
Haflidason H, Hajdas I, Hatte C, Heaton TJ, Hoffmann DL, Hogg AG, Hughen
KA, Kaiser KF, Kromer B, Manning SW, Niu M, Reimer RW, Richards DA,
Scott EM, Southon JR, Staff RA, Turney CSM, van der Plicht J. 2013.
IntCal13 ans Marine13 radiocarbon age calibration curves 0-50000 years
cal BP. Radiocarbon 55(4)=1869-1887.

## Examples

``` r
data(Marine13)
## maybe str(Marine13) ; head(Marine13) ...
```
