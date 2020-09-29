#' @export
# merge and align in multivariable MR
merge_align_multivariable <- function(d_exposure, exposure_names, include_palindromic){
  
  # change names for each
  dlist <- lapply(seq_along(exposure_names), function(i){
    d_out <- d_exposure[[i]]
    expname <- exposure_names[i]
    
    d_out[,paste0(c("EA", "NEA"), "_exposure") := lapply(paste0(c("EA", "NEA"), "_exposure"), function(x) toupper(get(x)))]
    
    oldnames <- c("rsid", paste0(c("EA", "NEA", "BETA", "SE"), "_exposure"), "pval")
    if(i == 1){ oldnames <- c("CHR", "POS", oldnames) }
    newnames <- gsub("exposure", expname, oldnames)
    
    setnames(d_out, oldnames, newnames)
    
    d_out <- d_out[,..newnames]
    
    setnames(d_out, "pval", paste0("pval_", expname))
    
    return(d_out)
  })
  
  # merge
  d_merged <- Reduce(function(x, y) x[y, on = c("rsid" = "rsid"), nomatch = NULL], dlist)
  
  # align effect sizes
  lapply(exposure_names[2:length(exposure_names)], function(x){
    d_merged[,paste0("BETA_", x, "_fixed") := get(paste0("BETA_", x))*(2*(get(paste0("EA_", exposure_names[1])) == get(paste0("EA_", x)))-1)]
  })
  
  # extract min p
  d_merged[,"pmin" := apply(do.call("cbind", lapply(grep("pval", names(d_merged), value = T), function(x) d_merged[[x]])), 1, min)]
  setnames(d_merged, "pmin", "pval")
  
  return(d_merged)
  
}
