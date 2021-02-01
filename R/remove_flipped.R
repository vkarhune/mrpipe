#' @export
# remove flipped variants
remove_flipped <- function(d_input = NULL){
  d_out <- d_input[EA_exposure_aligned == EA_outcome_aligned,]
  return(d_out)
}
