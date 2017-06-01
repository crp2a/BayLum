#' Concatenates two outputs of the function \code{\link{Generate_DataFile}}
#' 
#' This function allows concatenating two lists provided as output of the \code{\link{Generate_DataFile}} function.\cr 
#' Only concatenation of 2 files is possible.
#'
#' @param u1 list of objects: LT, sLT, ITimes, dLab, ddot_env, regDose, J, K, Nb_measurement.
#' @param u2 list of objects: LT, sLT, ITimes, dLab, ddot_env, regDose, J, K, Nb_measurement.
#'
#' @return A List of objects: LT, sLT, ITimes, dLab, ddot_env, regDose, J, K, Nb_measurement. 
#' 
#' @author Claire Christophe, Guillaume Guerin
#' 
#' @seealso \code{\link{Generate_DataFile}}
#' 
#' @examples
#' # load data files
#' data(DATA1,envir = environment())
#' data(DATA2,envir = environment())
#' # concatenate two data files 
#' DATA3=Concat_DataFile(DATA1,DATA2)
#' str(DATA3)
#' @export

Concat_DataFile<-function(u1,u2){
  Data=list("LT"=c(u1$LT,u2$LT),
            "sLT"=c(u1$sLT,u2$sLT),
            "ITimes"=c(u1$ITimes,u2$ITimes),
            "dLab"=cbind(u1$dLab,u2$dLab),
            "ddot_env"=cbind(u1$ddot_env,u2$ddot_env),
            "regDose"=c(u1$regDose,u2$regDose),
            "J"=c(u1$J,u2$J),
            "K"=c(u1$K,u2$K),
            "Nb_measurement"=c(u1$Nb_measurement,u2$Nb_measurement)
  )
  return(Data)
}