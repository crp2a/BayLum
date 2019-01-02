### ===============================================================================================
### R package BayLum BUILDSCRIPTS
### knitr
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2019-01-02
### ===============================================================================================

if(!require("knitr"))
  install.packages("knitr")

##set new version number (count one up)
temp <- readLines("DESCRIPTION")
version_id <- grep(pattern = "Version:", x = temp, fixed = TRUE)
version_number <-
  unlist(strsplit(x = temp[version_id], split = ".", fixed =
                    TRUE))
version_number <- paste(c(version_number[-length(version_number)],
                          as.numeric(version_number[length(version_number)]) + 1), collapse = ".")
temp[version_id] <- version_number
writeLines(text = temp, con = "DESCRIPTION")

#knit NEWS
rmarkdown::render("NEWS.Rmd", output_format = "github_document")

##CLEAN-UP
if(file.exists("NEWS.html"))
  file.remove("NEWS.html")
