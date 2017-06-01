#' Definition of the stratigraphic constraint matrix 
#'
#' This function helps to define the stratigraphic relation between samples, with questions.
#' The output of this function can be used in function \code{AgeS_Computation}.
#' 
#' @param Nb_sample interger: the sample number.
#' @param Names charcater vector: sample names.
#' 
#' @return A Matrix that summarise the ordered relation between samples. 
#' This matrix can be intergrate in \code{AgeS_Computation} function.
#' We refer to detail on \code{AgeS_Computation} for more information concerning this matrix.
#' 
#' @seealso \code{AgeS_Computation}
#' 
#' @author Claire Christophe, Guillaume Guerin
#'  
#' @examples
#' ## Assume that "sample1" is younger than "sample2"
#' # SCMatrix(2,c("sample1","sample2"))
#' # 1
#' 
#' @export
#' 

SCMatrix<-function(Nb_sample,Names){
  
  StratiConstraints=matrix(data=0,ncol=Nb_sample,nrow = (Nb_sample+1))
  # for(i in 1:Nb_sample){
  #   R<-readline(paste("Do you want to consider prior age for sample ",Names[i],"? 1 for TRUE or 0 for FALSE --> ",sep=''))
  #   StratiConstraints[1,i]=as.numeric(R)
  # }
  StratiConstraints[1,1]=1
  for(i in 2:Nb_sample){
    StratiConstraints[1,i]=1
    for(j in 1:(i-1)){
      R<-readline(paste(Names[j]," is younger than sample ",Names[i],"? 1 for TRUE or 0 for FALSE --> ",sep=''))
      StratiConstraints[(j+1),i]=as.numeric(R)  
    }
  }
  return(StratiConstraints)
}

