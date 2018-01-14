#' Concatenates two outputs of \code{\link{Generate_DataFile}} or \code{\link{Generate_DataFile_MG}}
#'
#' This function allows concatenating two lists provided by \code{\link{Generate_DataFile}}
#' or \code{\link{Generate_DataFile_MG}} functions.\cr
#' Only concatenation of 2 files is possible.
#'
#' @param L1 list of objects: LT, sLT, ITimes, dLab, ddot_env, regDose, J, K, Nb_measurement.
#' @param L2 list of objects: LT, sLT, ITimes, dLab, ddot_env, regDose, J, K, Nb_measurement.
#'
#' @details
#' For more information on the stucture of the input list, we refer to \bold{Value} section of
#' \code{\link{Generate_DataFile}} or \code{\link{Generate_DataFile_MG}}.
#'
#' This function is especially usefull in two cases:
#' \itemize{
#'  \item if informations concerning samples are yet saved in RData file (that allow to not run again
#'  \code{\link{Generate_DataFile}} or \code{\link{Generate_DataFile_MG}} that can take time);
#'  \item the user want to analyse simultaneously Single-grain and Multi-grain OSL measurements,
#'  because sample are in under stratigraphic constraints or they share systematic errors.
#' }
#'
#' @return A List of objects: LT, sLT, ITimes, dLab, ddot_env, regDose, J, K, Nb_measurement.
#'
#' @author Claire Christophe, Anne Philippe, Guillaume Guerin
#'
#' @seealso \code{\link{Generate_DataFile}}, \code{\link{Generate_DataFile_MG}}
#'
#' @examples
#' # load data files
#' data(DATA1,envir = environment())
#' data(DATA2,envir = environment())
#' # concatenate two data files
#' DATA3=Concat_DataFile(L1=DATA1,L2=DATA2)
#' str(DATA3)
#' @export
Concat_DataFile<-function(L1,L2){
  Data=list("LT"=c(L1$LT,L2$LT),
            "sLT"=c(L1$sLT,L2$sLT),
            "ITimes"=c(L1$ITimes,L2$ITimes),
            "dLab"=cbind(L1$dLab,L2$dLab),
            "ddot_env"=cbind(L1$ddot_env,L2$ddot_env),
            "regDose"=c(L1$regDose,L2$regDose),
            "J"=c(L1$J,L2$J),
            "K"=c(L1$K,L2$K),
            "Nb_measurement"=c(L1$Nb_measurement,L2$Nb_measurement)
  )
  return(Data)
}
