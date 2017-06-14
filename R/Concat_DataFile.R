#' Concatenates two outputs of \code{\link{Generate_DataFile}} or \code{\link{Generate_DataFile_MG}}
#'
#' This function allows concatenating two lists provided as output of the \code{\link{Generate_DataFile}}
#' or \code{\link{Generate_DataFile_MG}} function.\cr
#' Only concatenation of 2 files is possible.
#'
#' @param u1 list of objects: LT, sLT, ITimes, dLab, ddot_env, regDose, J, K, Nb_measurement.
#' @param u2 list of objects: LT, sLT, ITimes, dLab, ddot_env, regDose, J, K, Nb_measurement.
#'
#' @details
#' For more information on the stucture of the input list, we refer to \bold{Value} section of
#' \code{\link{Generate_DataFile}} or \code{\link{Generate_DataFile_MG}}.
#'
#' This function is especially usefull in two cases:
#' \itemize{
#'  \item information concerning samples are yet saved in .RData file (that allow to not run again
#'  \code{\link{Generate_DataFile}} or \code{\link{Generate_DataFile_MG}} that can take time);
#'  \item the user want to analyse simultaneously Single-grain and Multi-grain OSL measurements,
#'  because sample are in under stratigraphic constraints or they share systematic errors.
#' }
#'
#' @return A List of objects: LT, sLT, ITimes, dLab, ddot_env, regDose, J, K, Nb_measurement.
#'
#' @author Claire Christophe, Guillaume Guerin
#'
#' @seealso \code{\link{Generate_DataFile}}, \code{\link{Generate_DataFile_MG}}
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
