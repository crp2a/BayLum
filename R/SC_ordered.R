#' Stratigraphic matrix for samples ordered
#'
#' Construc the stratigraphic matrix used in fonction \code{AgeS_Computation} and \code{AgeC14_Computation}
#' for samples that are all ordered by increasing oder.
#'
#' @param Nb_sample  interger: the sample number.
#'
#' @return Stratigraphic matrix where each sample are ordered by incresing order
#' This matrix can be intergrate in \code{AgeS_Computation} function.
#' We refer to detail on \code{AgeS_Computation} for more information concerning this matrix.
#'
#' @seealso \code{AgeS_Computation}, \code{SC_Matrix}
#'
#' @author Claire Christophe, Guillaume Guerin
#'
#' @examples
#' SC=SC_Ordered(Nb_sample=3)
#'
#' @export

SC_Ordered<-function(Nb_sample){
  SC=matrix(data=0,ncol=Nb_sample,nrow=(Nb_sample+1))
  SC[1,]=rep(1,Nb_sample)
  for(i in 1:Nb_sample){
    SC[i+1,]=c(rep(0,i),rep(1,(Nb_sample-i)))
  }
}

