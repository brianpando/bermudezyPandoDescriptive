# Libreria para utilitarios de estadistica descriptiva en R
#

assize <- function(N, error=0.05, cl = 0.95 , p = 0.5){
  #' @title This function calculate the adjusted size Sample
  #' @description This function calculate the adjusted size Sample, for install you need
  #' devtools::install_github("repoowner/rdescriptive")
  #' @param N Population
  #' @return adjusted size Sample
  #' @export na
  #' @examples
  #' assize(230)
  #'
  Z <- qnorm( 1-(1-cl)/2)
  no <- (Z^2 * p * (1-p) * N)/(error^2*(N-1) + Z^2*p*(1-p) )
  nf <- no/(1+no/N)
  return(nf)
}
