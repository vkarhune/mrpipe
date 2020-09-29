#' @export
# compiles MR results from .RData files
extract_results <- function(file, exposure = exposure, outcome = outcome, methods = NULL){
  
  load(file)
  
  outlist <- lapply(methods, function(mod){
    
    method = switch(mod,
                    "res_ivw" = "IVW", "res_ivwr" = "IVW-RE", "res_egger" = "MR-Egger", "res_wm" = "Weighted Median",
                    "res_mode" = "Weighted Mode", "res_presso" = "MR-PRESSO", res_conmix = "ConMix", NULL
    )
    
    tryCatch(obj <- get(mod), error = function(error){return(cat(sprintf("Object %s not found for %s on %s\n", mod, exposure, outcome)))})
    
    if(!(exists("obj"))){
      
      n <- NA
      beta <- NA
      se <- NA
      cil <- NA
      ciu <- NA
      p <- NA 
      int <- NA
      p_pleiotropy <- NA
      
    } else if(is.null(obj)){
      
      n <- NA
      beta <- NA
      se <- NA
      cil <- NA
      ciu <- NA
      p <- NA 
      int <- NA
      p_pleiotropy <- NA
      
    } else if(method %in% "MR-PRESSO"){
      
      beta <- obj[[1]][2,"Causal Estimate"]
      
      n <- ifelse(is.na(beta), NA,
                  nrow(obj$`MR-PRESSO results`$`Outlier Test`) - length(obj$`MR-PRESSO results`$`Distortion Test`$`Outliers Indices`)
      )
      
      se <- obj[[1]][2,"Sd"]
      cil <- beta - 1.96*se
      ciu <- beta + 1.96*se
      p <- obj[[1]][2,"P-value"]
      int <- NA
      p_pleiotropy <- obj$`MR-PRESSO results`$`Global Test`$Pvalue
      
    } else {
      
      stderr <- switch(mod, "res_egger" = "StdError.Est", "StdError")
      cilower <- switch(mod, "res_egger" = "CILower.Est", "CILower")
      ciupper <- switch(mod, "res_egger" = "CIUpper.Est", "CIUpper")
      pvalue <- switch(mod, "res_egger" = "Pvalue.Est", "Pvalue")
      
      #  obj <- get(mod)
      
      n <- obj@SNPs
      beta <- obj@Estimate
      se <- slot(obj, stderr)
      cil <- slot(obj, cilower)
      ciu <- slot(obj, ciupper)
      p <- slot(obj, pvalue)
      int <- switch(mod, "res_egger" = slot(obj, "Intercept"), 0)
      p_pleiotropy <- switch(mod, "res_egger" = slot(obj, "Pleio.pval"), NA)
    }
    
    df_out <- data.frame(exposure, outcome, method, n, beta, se, cil, ciu, p, int, p_pleiotropy)
    
    return(df_out)
    
  })
  
  d_results <- do.call("rbind", outlist)
  
  return(d_results)
}

