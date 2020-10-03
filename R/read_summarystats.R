#' @export
# read summary statistics
read_summarystats <- function(
  phenotype = NULL,
  type = c("exposure", "outcome", "pheno1", "pheno2"),
  file = NULL,
  cols = NULL,
  no_rsid = NULL,
  chrpos_column = NULL,
  keyfile = "data/",
  pthresh = NULL, testing = FALSE, is_nealelab = NULL, custom = NULL){
  
  
  
  file <- filename(file = file)

  cols <- columns(cols = cols)

  if(length(cols) == 7 & type %in% c("exposure", "pheno1")){ cols <- c("CHR", "POS", cols) }
  
  d_out <- read_file(file, is_nealelab = is_nealelab, testing = testing, custom = custom)
  
  
  if(!(is.null(pthresh))){
    pvalcol <- cols[length(cols) - 1]
    d_out <- d_out[get(pvalcol) < pthresh,]
  }
  
  d_out <- rsid_check(d_out, chrpos_column = chrpos_column, keyfile = keyfile)
  
  
  if(type %in% "exposure"){
    d_out[,"id" := phenotype]
    cols <- c(cols, "id")
  }
  
  if(file %in% c("data/DBP.MVP_AFR_MAF_HWE.txt.gz", "data/DBP.MVP_EUR_MAF_HWE.txt.gz",
                 "data/SBP.MVP_AFR_MAF_HWE.txt.gz", "data/SBP.MVP_EUR_MAF_HWE.txt.gz")){
    d_out[,c("CHR", "POS") := lapply(1:2, function(x) sapply(strsplit(CHR.BP, ":"), "[[", x))]
  }
  
  # if(type %in% c("exposure", "pheno1") & !("POS" %in% names(d_out))) { cols[2] <- "BP"}
  
  
  d_out <- d_out[,..cols]
  
  outnames <- switch(type,
                     "exposure" = c("CHR", "POS", "rsid", "EA_exposure", "NEA_exposure", "BETA_exposure", "SE_exposure", "pval", "EAF_exposure", "id"),
                     "outcome" = c("rsid", "EA_outcome", "NEA_outcome", "BETA_outcome", "SE_outcome", "P_outcome", "EAF_outcome"),
                     "pheno1" = c("CHR", "POS", "SNPID", "EA1", "NEA1", "BETA1", "SE1", "P1", "EAF1"),
                     "pheno2" = c("SNPID", "EA2", "NEA2", "BETA2", "SE2", "P2", "EAF2"),
                     NULL
  )
  
  
  
  setnames(d_out, cols, outnames)
  
  return(d_out)
}
