#' @export
# read conmix results
extract_results_conmix_psigrid <- function(file, mod, exposure = exposure, outcome = outcome){
  
  load(file)
  
  
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
    
  } else {
    
    res_out <- lapply(obj, function(ob){
      
      df_out <- data.frame(
        exposure = exposure,
        outcome = outcome,
        method = "ConMix",
        n = length(ob@Valid),
        beta = ob@Estimate,
        se = NA,
        cil = ob@CILower,
        ciu = ob@CIUpper,
        p = NA,
        int = NA,
        p_pleiotropy = NA,
        psi = ob@Psi,
        stringsAsFactors = F
      )
      
      return(df_out)
    })
    
  }
  
  out <- do.call("rbind", res_out)
  return(out)
  
}