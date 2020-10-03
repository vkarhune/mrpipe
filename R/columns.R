#' @export
# define columns
columns <- function(cols = NULL, phenotype = phenotype){

if(is.null(cols)){
  cols <- switch(phenotype,
                 "T2DM" = c("RSID", "EA", "NEA", "BETA", "SE", "P", "EAF"),
                 "CAD" = c("markername", "effect_allele", "noneffect_allele", "beta", "se_dgc", "p_dgc", "effect_allele_freq"),
                 "strokeAS" = c("MarkerName", "Effect_allele", "Ref_allele", "Effect", "StdErr", "P.value", "Freq1"),
                 "strokeLAS" = c("MarkerName", "Effect_allele", "Ref_allele", "Effect", "StdErr", "P.value", "Freq1"),
                 "strokeCE" = c("MarkerName", "Effect_allele", "Ref_allele", "Effect", "StdErr", "P.value", "Freq1"),
                 "strokeSVS" = c("MarkerName", "Effect_allele", "Ref_allele", "Effect", "StdErr", "P.value", "Freq1"),
                 "PAD" = c("SNP", "Allele1", "Allele2", "Effect", "StdErr", "P.value", "Freq1"),
                 "HF" = c("SNP", "A1", "A2", "b", "se", "p", "freq"),
                 "AD" = c("SNP", "A1", "A2", "BETA", "SE", "P", "EAF"),
                 "CP" = c("MarkerName", "A1", "A2", "Beta", "Pval", "SE", "EAF"),
                 "BMI" = c("SNP", "Tested_Allele", "Other_Allele", "BETA", "SE", "P", "Freq_Tested_Allele"),
                 "SBP" = c("SNP", "Allele1", "Allele2", "Effect", "StdErr", "P.value", "Freq1"),
                 #"CRP" = c("rsid", "Allele1", "Allele2", "Effect", "StdErr"),
                 "CRP" = c("SNP", "Allele1", "Allele2", "Effect", "StdErr", "Pval", "EAF"),
                 "HDL" = c("SNP", "Allele1", "Allele2", "beta", "se", "pval", "minor_AF"),
                 "LDL" = c("SNP", "Allele1", "Allele2", "beta", "se", "pval", "minor_AF"),
                 "TG" = c("SNP", "Allele1", "Allele2", "beta", "se", "pval", "minor_AF"),
                 "ALT" = c("SNP", "A1", "A2", "beta_alt", "se_alt", "pval_alt", "minor_AF"),
                 "CKD" = c("RSID", "Allele1", "Allele2", "Effect", "P.value", "StdErr", "Freq1"),
                 "BUN" = c("RSID", "Allele1", "Allele2", "Effect", "P.value", "StdErr", "Freq1"),
                 "eGFR" = c("RSID", "Allele1", "Allele2", "Effect", "P.value", "StdErr", "Freq1"),
                 "hair" = c("rsid", "EA", "NEA", "beta", "se", "pval")
  )
  
}
  
  return(cols)
}
