#' @export
# calculate variance explained (R^2) of a genetic variant for a binary trait
# see also TwoSampleMR:::get_r_from_lor
R2_binary <- function(beta = BETA_exposure, eaf = EAF_exposure, ve = pi^2/3){
  vg <- 2*eaf*(1 - eaf)*beta^2
  out <- vg / (vg + ve)
  return(out)
}
