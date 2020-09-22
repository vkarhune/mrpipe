#' @export
# align effect alleles
align_eas <- function(d = d_mr){
  d_out <- within(d, {
    EA_exposure <- toupper(EA_exposure)
    NEA_exposure <- toupper(NEA_exposure)

    EA_outcome <- toupper(EA_outcome)
    NEA_outcome <- toupper(NEA_outcome)

    BETA_exposure <- BETA_exposure
    BETA_outcome <- BETA_outcome*(2*(EA_exposure == EA_outcome)- 1)

    # SNP exposure to positive
    byg <- BETA_outcome*sign(BETA_exposure)
    bxg <- abs(BETA_exposure)
  })

  # double check that alignment mismatches ok:
  print(mis1 <- nrow(d_out[with(d_out, EA_exposure != EA_outcome),]))
  print(mis2 <- with(d_out[with(d_out, EA_exposure != EA_outcome),],
                     sum(EA_exposure == NEA_outcome)))
  # these numbers should be the same

  if(mis1 != mis2) warning(sprintf("Check alignments for %s", expo))
  ### end align
  
  return(d_out)


}
