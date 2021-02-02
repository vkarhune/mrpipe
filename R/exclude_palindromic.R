#' @export
# exclude palindromic variants
exclude_palindromic <- function(d_input = NULL, mafthresh = 0.4){
  
  # remove palindromic with MAF
  d_out <- d_input[!((
    paste0(EA_exposure, NEA_exposure) %in% c("AT", "TA", "CG", "GC") & EAF_exposure_aligned > mafthresh) & 
      (paste0(EA_outcome, NEA_outcome) %in% c("AT", "TA", "CG", "GC") & EAF_outcome_aligned > mafthresh)),]
  
  return(d_out)
}
