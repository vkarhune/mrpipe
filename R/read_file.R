#' @export
# read summary statistics file
read_file <- function(file,
                      is_nealelab = NULL, testing = FALSE, custom = NULL, phenotype = phenotype){
  
  if(is.null(is_nealelab)) { is_nealelab <- FALSE }
  
  if(tools::file_ext(file) %in% "Rds"){
    d_out <- readRDS(file)
  } else if(is_nealelab | tools::file_ext(file) %in% "zip"){
    d_out <- fread(cmd = paste0("zcat ", file), check.names = T)
  } else {
    d_out <- fread(file, check.names = T, nrows = ifelse(testing, 10000, Inf))
  }
  
  if(is_nealelab){
    # d_outcome[,"rsid" := fread(cmd = paste0("zcat ../misc/sumstats/variants.tsv.bgz"), check.names = T)$rsid]
    variant_file <- ifelse(testing, "../misc/sumstats/variants.tsv.bgz", "data/variants.tsv.bgz")
    d_ukbb <- fread(cmd = paste0("zcat ", variant_file), check.names = T)
    ukbbcols <- c("rsid", "variant")
    d_ukbb <- d_ukbb[,..ukbbcols]
    d_out <- d_out[d_ukbb, on = c("variant" = "variant"), nomatch = NULL]
    rm(d_ukbb)
    # d_out[,"rsid" := fread(cmd = paste0("zcat ", variant_file), check.names = T)$rsid]
    d_out[,c("EA", "NEA") := lapply(4:3, function(x) sapply(strsplit(variant, ":"), "[[", x))]
    d_out <- d_out[!(low_confidence_variant),]
    # } else if(phenotype %in% "CRP"){
    #  d_out <- readRDS(file)
  }
  
  if(phenotype %in% c("BMI")){
    d_out[,"SNP" := gsub(":.*", "", SNP)] 
  }
  
  if(phenotype %in% "CRP"){
    d_out[,"EAF" := NA]
    d_out[,"Pval" := 2*pnorm(abs(Effect/StdErr), lower.tail = F)]
  }
  
  return(d_out)
  
}