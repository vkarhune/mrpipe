#' @export
# Clumping using LDlinkR package
ld_clump_ldlinkr <- function(d = NULL, clump_r2 = NULL, pop = "EUR",
                             ldlink_access_token = ldlink_access_token){
  
  cols <- names(d)
  
  d_out <- d[which.min(pval),]
  
  i <- 0
  
  while(nrow(d) > 1){
    
    leadSNP1 <- d[which.min(pval),][["rsid"]]
    
    d_out <- rbind(d_out, d[rsid %in% leadSNP1,..cols])
    
    LDmat <- LDlinkR::LDmatrix(snps = d$rsid, 
                      pop = pop, r2d = "r2", 
                      token = ldlink_access_token
    )
    
    d_ld <- LDmat[,c("RS_number", leadSNP1)]
    setDT(d_ld)
    d <- d[d_ld, on = c("rsid" = "RS_number"), nomatch = NA]
    
    d <- d[get(leadSNP1) < clump_r2,]
    
    i <- i + 1
    cat(sprintf("%i\n", i))
  }
  
  d_out <- unique(d_out)
  return(d_out)
  
}
