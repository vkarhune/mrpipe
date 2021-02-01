#' @export
# align effect sizes, new version
align_effects <- function(d_input = NULL){
  # align effects for the minor allele
  d_input[,"EA_exposure_aligned" := ifelse(EAF_exposure < 0.5, EA_exposure, NEA_exposure)]
  d_input[,"NEA_exposure_aligned" := ifelse(
    EA_exposure == EA_exposure_aligned, NEA_exposure, EA_exposure
  )]
  d_input[,"BETA_exposure_aligned" := ifelse(
    EA_exposure == EA_exposure_aligned, BETA_exposure, -BETA_exposure
  )]
  d_input[,"EAF_exposure_aligned" := -(abs(EAF_exposure - 0.5)) + 0.5]
  
  
  
  d_input[,"EA_outcome_aligned" := ifelse(EAF_outcome < 0.5, EA_outcome, NEA_outcome)]
  d_input[,"NEA_outcome_aligned" := ifelse(
    EA_outcome == EA_outcome_aligned, NEA_outcome, EA_outcome
  )]
  d_input[,"BETA_outcome_aligned" := ifelse(
    EA_outcome == EA_outcome_aligned, BETA_outcome, -BETA_outcome
  )]
  d_input[,"EAF_outcome_aligned" := -(abs(EAF_outcome - 0.5)) + 0.5]
  
  return(d_input)
  
}
