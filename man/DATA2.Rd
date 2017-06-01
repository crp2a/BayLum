\name{DATA2}
\alias{DATA2}
\docType{data}
\title{DATA on sample named GDB5
%%   ~~ data name/kind ... ~~
}
\description{
list of objects: LT, sLT, ITimes, dLab, ddot_env, regDose, J,K,Nb_measurement obtained using Generate_DataFile function from luminescence data. 
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("DATA2")}
\format{
  A data frame with 0 observations on the following 2 variables.
  \describe{
   \item{\code{LT}}{(one list per sample); each list contains all L/T values for the corresponding sample;}
   \item{\code{sLT}}{(one list per sample); each list contains all uncertainties on L/T values for the corresponding sample;}
   \item{\code{ITimes}}{(one list per sample); each list contains irradiation time values for the corresponding sample;}
   \item{\code{dLab}}{, a matrix containing in line \code{i}, the laboratory dose rate and its variance for sample \code{i};}
   \item{\code{ddot_env}}{, a matrix containing in line \code{i}, the environmental dose rate and its variance (excluding the common error terms) for sample \code{i};}
   \item{\code{regDose}}{(one list per sample); each list contains all regenerated doses;}
   \item{\code{J}}{, a vector giving, for each BIN file, the number of aliquots selected for the analysis;}
   \item{\code{K}}{, a vector giving, for each BIN file, the number of regenerative doses in the SAR protocol;}
   \item{\code{Nb_measurement}}{, a vector giving, for each BIN file, the number of measurements;}
  }
}
\details{ A FAIRE
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{ A FAIRE
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{ A FAIRE
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(DATA2)
str(DATA2)
}
\keyword{datasets}
