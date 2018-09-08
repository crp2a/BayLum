## Release summary

Bugfix release, intended to replace version 0.1.2 on CRAN.

## Addressed CRAN issues

> [Error on r-devel-windows-ix86+x86_64]
> Error: connections left open:
> Model_AgeC14[[Model]] (textConnection)

Fixed in all affected functions, all text connections are now 
closed after usage.

> [Error on r-oldrel-windows-ix86+x86_64]
>combine_DataFiles: no visible global function definition for '...length' [...]

The affected function now checks the installed R version and applies different
code if necessary. We tested the implemented changes on an R version < 3.5.0 and
no error was returned.

> [Error on r-oldrel-osx-x86_64]

The error message has a similar cause on windows, the code was modified as stated above. 

## Addressed CRAN Team requests

Email by Uwe Ligges 2018-09-07 (two issues)

>> data(DATA2,envir = environment())
>Data <- combine_DataFiles(DATA2,DATA1)
>Error in ...length() : could not find function "...length"
>Calls: combine_DataFiles
>Execution halted
>
>Please either declare
>
>Depends: R (>= 3.5.0)

Fixed with the changes stated above. 

>And in R-devel on Windows you see you forgot to close connections.
>The porblem is in AgeC14_Computation() where you use dev.print() after dev.off(), and hence reopen a device.

This error is not related to open devices but text connections left open, This is fixed and now 
all connection are closed after the data had been imported. 

The email mentioned also the fallowing code lines:

>  if(SavePdf){
>    pdf(file=paste(OutputFilePath,OutputFileName[1],'.pdf',sep=""))
>  }
>  plot_MCMC(echantillon, sample_names = SampleNames, variables = "Age")
>  if(SavePdf){
>   dev.off()
>  }

These lines are indeed not very clever, however, they will not leave a connection open if `SavePdf == TRUE`.
We also tried a global closing of all connections, but it caused additional problems in combination 
with the package 'pkgdown'. Our implemented changes effectively fixed the errors on CRAN.

## Other notes or warnings


## Reverse dependency checks

This package has not yet external dependencies.

## Test environments
* local macOS High Sierra 10.13.6, Xcode 9.4.1, R-devel and R-release (patched)
* on AppVeyor CI
  * i386-w64-mingw32/i386 (32-bit), R-devel
  * x86_64_w64-mingw32/64 (64-bit), R-devel
  * x86_64_w64-mingw32/64 (64-bit), R-release
  * i386-w64-mingw32/i386 (32-bit), R-stable
* on Travis CI
  * Ubuntu 14.04.5 LTS, oldrel
  * Ubuntu 14.04.5 LTS, release
  * Ubuntu 14.04.5 LTS, devel
  * macOS Sierra 10.13, R-release, Apple LLVM version 9.1.0 (clang-902.0.39.2)
