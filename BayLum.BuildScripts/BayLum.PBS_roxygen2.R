### ===============================================================================================
### R package BayLum BUILDSCRIPTS
### roxygen2
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2017-06-14
### ===============================================================================================

if(!require("devtools"))
  install.packages("devtools")

library(devtools)
document(pkg = ".", roclets = NULL)
