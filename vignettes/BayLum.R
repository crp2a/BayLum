## ---- echo = FALSE, message = FALSE-----------------------------------------------------------------------------------
knitr::opts_chunk$set(comment = "")
options(width = 120, max.print = 100)
library(BayLum)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  install.packages('BayLum')

## ---------------------------------------------------------------------------------------------------------------------
library(BayLum)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  Path=c("inst/extdata/") # give the name of the path to data folder
#  Names=c("GDB3") # give the name of the folder datat
#  Nb_sample=1     # give the number of sample
#  DATA1=Generate_DataFile(Path,Names,Nb_sample)

## ---- echo=FALSE------------------------------------------------------------------------------------------------------
data("DATA1") # load output of Generate_DataFile function for the sample named "GDB3"

## ---------------------------------------------------------------------------------------------------------------------
str(DATA1)

## ----eval=FALSE,fig.cap = "inst/LT_RegenDose_plot.pdf"----------------------------------------------------------------
#  LT_RegenDose(DATA1,Path=c('inst/extdata/'),Names=c("GDB3"))

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  Age=Age_Computation(DATA1,samplename="GDB3",
#                      distribution="cauchy",
#                      LIN_fit=TRUE,Origin_fit=FALSE,
#                      SavePdf=FALSE,SaveEstimates=FALSE,
#                      Taille=10000)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  Nb_sample=1
#  Nb_binfile = length(Names)
#  BinPerSample = c(length(Names))
#  DATA_BF=Generate_DataFile(Path,
#                            Names,
#                            Nb_sample,
#                            Nb_binfile=Nb_binfile,
#                            BinPerSample=BinPerSample)
#  # Computation of age
#  Age=Age_Computation(DATA_BF,
#                      samplename="Nom",
#                      BinPerSample=BinPerSample)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  Path=c("Path/") # give the name of the path to data folder
#  Names=c("sample1-1","sample1-2","sample2-1") # give the name of the folder datat
#  Nb_sample=2    # give the number of sample
#  DATA=Generate_DataFile(Path,Names,Nb_sample,Nb_binfile=3,BinPerSample=c(2,1))

## ---------------------------------------------------------------------------------------------------------------------
data("DATA1",envir = environment())
data("DATA2",envir = environment())
DATA=Concat_DataFile(DATA2,DATA1)
str(DATA)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  # for the "GDB5"
#  LT_RegenDose(DATA,Path=c('inst/extdata/'),Names=c("GDB5"),SampleNumber = 1)
#  # for the "GDB3"
#  LT_RegenDose(DATA,Path=c('inst/extdata/'),Names=c("GDB3"),SampleNumber = 2)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  Age=AgeS_Computation(DATA,Nb_sample=2,SampleNames=c("GDB5","GDB3"),Taille=10000)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  # using Generate_DataFile function
#  Path=c("inst/extdata/")
#  Names=c("GDB5","GDB3")
#  Nb_sample=2
#  Generate_DataFile(Path,Names,Nb_sample)
#  
#  # using Concat_DataFile function
#  data(DATA1,envir = environment()) # .RData on sample GDB3
#  data(DATA2,envir = environment()) # .RData on sample GDB5
#  DATA=Concat_DataFile(DATA2,DATA1)

## ---------------------------------------------------------------------------------------------------------------------
SC=SCMatrix(2,c("GDB5","GDB3"))
1

## ----eval=TRUE--------------------------------------------------------------------------------------------------------
(SC=matrix(data=c(1,1,0,1,0,0),ncol=2,nrow = (2+1),byrow = T))

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  Age=AgeS_Computation(DATA,Nb_sample=2,SampleNames=c("GDB5","GDB3"),StratiConstraints=SC,Taille=10000)

