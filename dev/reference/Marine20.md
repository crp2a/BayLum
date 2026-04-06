# Marine data for calibration of 14C age

As 14C years is not equal to calendar years because atmospheric 14C
concentration varies through time. Hence, data in marine_CalC14 allows a
calibration for hypothetical "global" marine reservoir.

## Usage

``` r
data("Marine20")
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

Heaton, T., Köhler, P., Butzin, M., Bard, E., Reimer, R., Austin, W., .
. . Skinner, L. (2020). Marine20—The Marine Radiocarbon Age Calibration
Curve (0–55,000 cal BP). Radiocarbon, 62(4), 779-820.
doi:10.1017/RDC.2020.68

## Examples

``` r
data(Marine20)
## maybe str(Marine20) ; head(Marine20) ...
```
