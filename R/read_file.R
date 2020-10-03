#' @export
# read summary statistics file
read_file <- function(file,
                      is_nealelab = NULL, testing = FALSE, custom = NULL, phenotype = phenotype){
  
  if(tools::file_ext(file) %in% "Rds"){
    d_out <- readRDS(file)
  } else if(!(is.null(is_nealelab))){
    if(is_nealelab){
      d_out <- fread(cmd = paste0("zcat ", file), check.names = T)
      # d_outcome[,"rsid" := fread(cmd = paste0("zcat ../misc/sumstats/variants.tsv.bgz"), check.names = T)$rsid]
      d_out[,"rsid" := fread(cmd = paste0("zcat data/variants.tsv.bgz"), check.names = T)$rsid]
      d_out[,c("EA", "NEA") := lapply(4:3, function(x) sapply(strsplit(variant, ":"), "[[", x))]
      d_out <- d_out[!(low_confidence_variant),]
    }
    # } else if(phenotype %in% "CRP"){
    #  d_out <- readRDS(file)
  } else if(phenotype %in% "ALT"){
    d_out <- fread(cmd = paste0("zcat ", file), check.names = T)
  } else {
    d_out <- fread(file, check.names = T, nrows = ifelse(testing, 10000, Inf))
  }
  
  if(phenotype %in% c("BMI")){
    d_out[,"SNP" := gsub(":.*", "", SNP)] 
  }
  
  if(phenotype %in% "CRP"){
    d_out[,"EAF" := NA]
    d_out[,"Pval" := 2*pnorm(abs(Effect/StdErr), lower.tail = F)]
  }
  
  if("low_confidence_variant" %in% colnames(d_out)){
    d_out <- d_out[!(low_confidence_variant),]
  }
  
  
}