### ===============================================================================================
### R package BayLum BUILDSCRIPTS
### BibTeX
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2017-06-14
### ===============================================================================================

library(tools)
library(BayLum)

##get version number
temp <- readLines("DESCRIPTION")
temp <- temp[grep("Version", temp)]
temp.version <- sub(" ","",unlist(strsplit(temp,":"))[2])

package.citation <- toBibtex(citation("BayLum"))
write(package.citation, file=paste0("BayLum.BuildResults/BayLum_", temp.version,"-bibliography.bib"))
