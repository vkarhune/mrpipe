#' @export
# compiles MR results from .RData files
extract_results <- function(file, exposure = exposure, outcome = outcome, methods = NULL,
                            heterogeneity = FALSE){
  
  load(file)
  
  outlist <- lapply(methods, function(mod){
    
    method = switch(mod,
                    "res_ivw" = "IVW", "res_ivwr" = "IVW-RE", "res_egger" = "MR-Egger", "res_wm" = "Weighted Median",
                    "res_wmode" = "Weighted Mode", "res_presso" = "MR-PRESSO", res_conmix = "ConMix", NULL
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
      if(heterogeneity){
        Q <- NA
        p_het <- NA
      }
      
    } else if(is.null(obj)){
      
      n <- NA
      beta <- NA
      se <- NA
      cil <- NA
      ciu <- NA
      p <- NA 
      int <- NA
      p_pleiotropy <- NA
      if(heterogeneity){
        Q <- NA
        p_het <- NA
      }
      
    } else if(method %in% "MR-PRESSO"){
      
      if(heterogeneity){
        Q <- NA
        p_het <- NA
      }
      
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
      
      if(heterogeneity & mod %in% "res_ivwr"){
        Q <- obj@Heter.Stat[1]
        p_het <- obj@Heter.Stat[2]
      } else {
        Q <- NA
        p_het <- NA
      }
      
      
      
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
    if(heterogeneity){
      df_out <- data.frame(exposure, outcome, method,
                           n, beta, se, cil, ciu, p, int, p_pleiotropy, Q, p_het)
    }
    
    return(df_out)
    
  })
  
  d_results <- do.call("rbind", outlist)
  
  return(d_results)
}


