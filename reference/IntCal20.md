# Atmospheric North data for calibration of 14C age

As 14C years is not equal to calendar years because atmospheric 14C
concentration varies through time. Hence, data in
AtmosphericNorth_CalC14 allows a calibration for mid-latitude Northern
Hemisphere atmospher reservoir.

## Usage

``` r
data("IntCal20")
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

Reimer, P., Austin, W., Bard, E., Bayliss, A., Blackwell, P., Bronk
Ramsey, C., . . . Talamo, S. (2020). The IntCal20 Northern Hemisphere
Radiocarbon Age Calibration Curve (0–55 cal kBP). Radiocarbon, 62(4),
725-757. doi:10.1017/RDC.2020.41

## Examples

``` r
data(IntCal20)
## maybe str(IntCal20) ; head(IntCal20) ...
```
