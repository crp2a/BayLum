# Vignettes that relies on heavy computations have been precompiled
old_wd <- setwd("./vignettes")
knitr::knit("BayLum.Rmd.orig", output = "BayLum.Rmd", envir = new.env())
setwd(old_wd)
