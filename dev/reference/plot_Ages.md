# Create Age Plot

Create Age Plot

## Usage

``` r
plot_Ages(
  object,
  sample_names = NULL,
  sample_order = NULL,
  plot_mode = "ages",
  ...
)
```

## Arguments

- object:

  [list](https://rdrr.io/r/base/list.html) or
  [data.frame](https://rdrr.io/r/base/data.frame.html) (**required**):
  Output as created by functions like
  [AgeC14_Computation](https://crp2a.github.io/BayLum/dev/reference/AgeC14_Computation.md),
  which is a list of class `BayLum.list`. Alternatively the function
  supports a [data.frame](https://rdrr.io/r/base/data.frame.html) as
  input, however, in such a case the
  [data.frame](https://rdrr.io/r/base/data.frame.html) must resemble the
  ages [data.frame](https://rdrr.io/r/base/data.frame.html) created by
  the computation functions otherwise the input will be silently
  ignored.

- sample_names:

  [character](https://rdrr.io/r/base/character.html) (optional):
  alternative sample names used for the plotting. If the length of the
  provided [character](https://rdrr.io/r/base/character.html) vector is
  shorter than the real number of samples, the names are recycled.

- sample_order:

  [numeric](https://rdrr.io/r/base/numeric.html) (optional): argument to
  rearrange the sample order, e.g., `sample_order = c(4:1)` plots the
  last sample first.

- plot_mode:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  allows to switch from displaying ages as points with lines (`"ages"`)
  for the credible intervals to densities (`"density"`)

- ...:

  further arguments to control the plot output, standard arguments are:
  `cex`, `xlim`, `main`, `xlab`, `col` further (non-standard) arguments
  are: `grid` (`TRUE`/`FALSE`), `legend` (`TRUE`/`FALSE`), `legend.text`
  ([character](https://rdrr.io/r/base/character.html) input needed),
  `legend.pos`
  [graphics::legend](https://rdrr.io/r/graphics/legend.html),
  `legend.cex`. Additional arguments: `d_scale` (scales density plots),
  `show_ages` (add ages to density plots)

## Value

The function returns a plot and the
[data.frame](https://rdrr.io/r/base/data.frame.html) used to display the
data

## Details

This function creates an age plot showing the mean ages along with the
credible intervals. The function provides various arguments to modify
the plot output, however, for an ultimate control the function returns
the [data.frame](https://rdrr.io/r/base/data.frame.html) extracted from
the input object for own plots.

## Function version

0.1.5

## See also

[AgeC14_Computation](https://crp2a.github.io/BayLum/dev/reference/AgeC14_Computation.md),
[AgeS_Computation](https://crp2a.github.io/BayLum/dev/reference/AgeS_Computation.md)

## Author

Sebastian Kreutzer, Institute of Geography, Ruprecht-Karl-University of
Heidelberg (Germany), based on code written by Claire Christophe

## Examples

``` r
## load data
data(DATA_C14,envir = environment())
C14Cal <- DATA_C14$C14[,1]
SigmaC14Cal <- DATA_C14$C14[,2]
Names <- DATA_C14$Names
nb_sample <- length(Names)

## Age computation
Age <- AgeC14_Computation(
   Data_C14Cal = C14Cal,
   Data_SigmaC14Cal = SigmaC14Cal,
   SampleNames = Names,
   Nb_sample = nb_sample,
   PriorAge = rep(c(20,60),nb_sample),
   Iter = 500,
   quiet = TRUE)
#> Warning: [plot_MCMC()] 'n.iter' out of range, reset to number of observations




#> 
#> 
#> >> MCMC Convergence of Age parameters <<
#> ----------------------------------------------
#> Sample name   Bayes estimate   Uppers credible interval
#> A_S-EVA-26510     1       1.001 
#> A_S-EVA-26506     1.001       1.011 
#> A_S-EVA-26507     1.007       1.018 
#> A_S-EVA-26508     1.001       1.017 
#> 
#> 
#> ---------------------------------------------------------------------------------------------------
#>  *** WARNING: The following information are only valid if the MCMC chains have converged  ***
#> ---------------------------------------------------------------------------------------------------
#> 
#> 
#> 
#> >> Bayes estimates of Age for each sample and credible interval <<
#> ------------------------------------------------------
#> Sample name   Bayes estimate  Credible interval: 
#> A_S-EVA-26510     41.953194874718 
#>                       lower bound     upper bound
#>               at level 95%    41.504          42.387 
#>               at level 68%    41.812          42.209 
#> ------------------------------------------------------
#> Sample name   Bayes estimate  Credible interval: 
#> A_S-EVA-26506     45.724153700876 
#>                       lower bound     upper bound
#>               at level 95%    44.979          46.24 
#>               at level 68%    45.469          46.039 
#> ------------------------------------------------------
#> Sample name   Bayes estimate  Credible interval: 
#> A_S-EVA-26507     44.9348735138598 
#>                       lower bound     upper bound
#>               at level 95%    43.813          45.889 
#>               at level 68%    44.421          45.403 
#> ------------------------------------------------------
#> Sample name   Bayes estimate  Credible interval: 
#> A_S-EVA-26508     45.095136177623 
#>                       lower bound     upper bound
#>               at level 95%    43.952          46.117 
#>               at level 68%    44.638          45.579 
#> 
#> ------------------------------------------------------



## plot output
plot_Ages(Age)

#>        SAMPLE      AGE HPD68.MIN HPD68.MAX HPD95.MIN HPD95.MAX ALT_SAMPLE_NAME
#> 1 S-EVA-26510 41.95319    41.812    42.209    41.504    42.387              NA
#> 2 S-EVA-26506 45.72415    45.469    46.039    44.979    46.240              NA
#> 3 S-EVA-26507 44.93487    44.421    45.403    43.813    45.889              NA
#> 4 S-EVA-26508 45.09514    44.638    45.579    43.952    46.117              NA
#>   AT
#> 1  4
#> 2  3
#> 3  2
#> 4  1

## plot output
plot_Ages(Age, plot_mode = "density")

#>        SAMPLE      AGE HPD68.MIN HPD68.MAX HPD95.MIN HPD95.MAX ALT_SAMPLE_NAME
#> 1 S-EVA-26510 41.95319    41.812    42.209    41.504    42.387              NA
#> 2 S-EVA-26506 45.72415    45.469    46.039    44.979    46.240              NA
#> 3 S-EVA-26507 44.93487    44.421    45.403    43.813    45.889              NA
#> 4 S-EVA-26508 45.09514    44.638    45.579    43.952    46.117              NA
#>   AT
#> 1  4
#> 2  3
#> 3  2
#> 4  1
```
