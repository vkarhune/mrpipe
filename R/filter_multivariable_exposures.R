#' @export
# filter exposures for multivariable MR
filter_multivariable_exposures <- function(d_exposures, pthresh, snps = NULL){
  
  if(is.null(snps)){
  snps <- unlist(lapply(d_exposures, function(x){
    out <- x[pval < pthresh,][["rsid"]]
  }))
  }
  
  cat(sprintf("Number of SNPs for instruments: %s\n", length(snps)))
  
  d_out <- lapply(d_exposures, function(x){
    out <- x[rsid %in% snps,]
    return(out)
  })
  
  return(d_out)
}
