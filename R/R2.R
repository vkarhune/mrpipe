#' @export
# calculate variance explained (R^2) for a genetic variant
R2 <- function(beta = BETA_exposure, eaf = EAF_exposure, var = 1,
               ncases = NULL, ncontrols = NULL, prevalence = NULL){
  
  if(all(ncases == 0)){
    ncases <- NULL
    ncontrols <- NULL
  }
  
  # check whether binary, i.e. if ncases is given
  if(!(is.null(ncases))){
    r2 <- r2lcc(beta = beta, eaf = eaf,
                prevalence = prevalence, ncases = ncases, ncontrols = ncontrols)
  } else {
    r2 <- (2*eaf*(1 - eaf)*beta^2)/var
  }
  
  return(r2)
  
}

if(0){
R2 <- function(beta = BETA_exposure, eaf = EAF_exposure, var = 1, binary = FALSE){
  
  ve <- pi^2/3
  
  if(binary){
    vg <- 2*eaf*(1 - eaf)*beta^2
    out <- vg / (vg + ve)
  } else {
    out <- (2*eaf*(1 - eaf)*beta^2)/var
  }
  
  return(out)
}
}

