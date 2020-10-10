#' @export
# calculate variance explained for a binary trait with ascertainment correction
# doi:10.1002/gepi.21614
r2lcc <- function(beta, eaf, prevalence, ncases, ncontrols){
  
  K <- prevalence
  P <- ncases/(ncases + ncontrols)

  if(is.null(K)){ K <- P }
  
  r2obs <- 2*eaf*(1-eaf)*beta^2
  
  thresh <- qnorm(1 - K)
  
  z <- dnorm(thresh)
  
  m <- z/K
  
  C <- (K^2*(1 - K)^2)/(z^2*P*(1 - P))
  
  multip <- m*(P - K)/(1 - K)
  theta <- multip*(multip - thresh)
  
  out <- (r2obs*C)/(1 + r2obs*theta*C)
  
  return(out)
}
