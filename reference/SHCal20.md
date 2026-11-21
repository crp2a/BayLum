# Atmospheric South data for calibration of 14C age

As 14C years is not equal to calendar years because atmospheric 14C
concentration varies through time. Hence, data in
AtmosphericSouth_CalC14 allows a calibration for mid-latitude Southern
Hemisphere atmospher reservoir.

## Usage

``` r
data("SHCal20")
```

## Format

A data frame with 3 variables.

- `CAL.BP`:

  a numeric vector correpondig to calendar years (in ky) befor present

- `X14C.age`:

  a numeric vector correponding to 14C age

- `Error`:

  a numeric vector correponding to error arround 14C age measurement

## References

Hogg, A., Heaton, T., Hua, Q., Palmer, J., Turney, C., Southon, J., . .
. Wacker, L. (2020). SHCal20 Southern Hemisphere Calibration, 0–55,000
Years cal BP. Radiocarbon, 62(4), 759-778. doi:10.1017/RDC.2020.59

## Examples

``` r
data(SHCal20)
## maybe str(SHCal20) ; head(SHCal20) ...
```
