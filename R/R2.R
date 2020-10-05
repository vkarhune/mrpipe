#' @export
# calculate variance explained (R^2) for a genetic variant
R2 <- function(beta = BETA_exposure, eaf = EAF_exposure, var = 1){
  out <- (2*eaf*(1 - eaf)*beta^2)/var
  return(out)
}
