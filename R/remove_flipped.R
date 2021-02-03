#' @export
# remove flipped variants
remove_flipped <- function(d_input = NULL){
  # d_out <- d_input[EA_exposure_aligned == EA_outcome_aligned,]
  
  # removing those that have flipped:
  d_out <- d_input[!(EA_exposure_aligned == NEA_outcome_aligned),]
  
  # retain those that may be strand flips
  d_out <- d_out[(EA_exposure_aligned == EA_outcome_aligned) |
                   (paste0(EA_exposure_aligned, EA_outcome_aligned) %in% c("AT", "TA", "CG", "GC") &
                      NEA_exposure_aligned == NEA_outcome_aligned),]
  
  return(d_out)
}
