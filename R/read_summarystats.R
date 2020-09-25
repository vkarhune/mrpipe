#' @export
# read summary statistics
read_summarystats <- function(
  phenotype = NULL,
  type = c("exposure", "outcome", "pheno1", "pheno2"),
  file = NULL,
  cols = NULL,
  no_rsid = NULL,
  chrpos_column = NULL){
  
  
  if(is.null(file)){
    file <- switch(phenotype,
                   "T2DM" = "data/MVP.T2D.ME.MAF001.dbGaP.txt.gz",
                   "CAD" = "data/cad.add.160614.website.txt",
                   "strokeAS" = "data/MEGASTROKE.1.AS.MANTRA.GC_filtered_X_nocases_het.TBL",
                   "strokeLAS" = "data/MEGASTROKE.3.LAS.MANTRA.GC_filtered_X_nocases_het.TBL",
                   "strokeCE" = "data/MEGASTROKE.4.CE.MANTRA.GC_filtered_X_nocases_het.TBL",
                   "strokeSVS" = "data/MEGASTROKE.5.SVD.MANTRA.GC_filtered_X_nocases_het.TBL",
                   "PAD" = "data/pad_gwas.csv",
                   "HF" = "data/HERMES_Jan2019_HeartFailure_summary_data.txt",
                   "AD" = "data/AD_sumstats_Jansenetal_2019sept.txt.gz",
                   "CP" = "data/GWAS_CP_all.txt",
                   "BMI" = "data/bmi.giant-ukbb.meta-analysis.combined.23May2018.txt.gz",
                   "SBP" = "data/sbp_gwas.csv",
                   # "CRP" = "data/crp_sumstats_with_rsid.Rds",
                   "CRP" = "data/CRP_gwas.csv",
                   "HDL" = "data/hdl_gwas_neale.csv",
                   "LDL" = "data/ldl_gwas_neale.csv",
                   "TG" = "data/tg_gwas_neale.csv",
                   "ALT" = "data/alt_ast_neale.zip",
                   "CKD" = "data/CKD_overall_ALL_JW_20180223_nstud30.dbgap.txt.gz",
                   "BUN" = "data/BUN_overall_ALL_YL_20171017_METAL1_nstud_33.dbgap.txt.gz",
                   "eGFR" = "data/20171016_MW_eGFR_overall_ALL_nstud61.dbgap.txt.gz",
                   "hair" = "data/1747_1.gwas.imputed_v3.both_sexes.tsv.bgz"
    )
  }
  
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
    
    if(type %in% c("exposure", "pheno1")){
      cols <- c("CHR", "POS", cols)
    }
  }
  
  
  if(phenotype %in% "hair"){
    d_out <- fread(cmd = paste0("zcat ", file), check.names = T)
    # d_outcome[,"rsid" := fread(cmd = paste0("zcat ../misc/sumstats/variants.tsv.bgz"), check.names = T)$rsid]
    d_out[,"rsid" := fread(cmd = paste0("zcat data/variants.tsv.bgz"), check.names = T)$rsid]
    d_out[,c("EA", "NEA") := lapply(4:3, function(x) sapply(strsplit(variant, ":"), "[[", x))]
    d_out <- d_out[!(low_confidence_variant),]
    # } else if(phenotype %in% "CRP"){
    #  d_out <- readRDS(file)
  } else if(phenotype %in% "ALT"){
    d_out <- fread(cmd = paste0("zcat ", file), check.names = T)
  } else {
    d_out <- fread(file, check.names = T)
  }
  
  if(phenotype %in% c("BMI")){
    d_out[,"SNP" := gsub(":.*", "", SNP)] 
  }
  
  if(phenotype %in% "CRP"){
    d_out[,"EAF" := NA]
    d_out[,"Pval" := 2*pnorm(abs(Effect/StdErr), lower.tail = F)]
  }
  
  if(!(is.null(no_rsid))){
    if(no_rsid){
      # cat(sprintf("No rsids in the summary statistics for %s\n", phenotype))
      dlist <- split(d_out, d[["CHR"]])
      d_out <- rbindlist(names(dlist), function(x, chrpos = chrpos_column){
        d_key <- read_key(file = "data/sysdata.rda", chr = x)
        dd <- dlist[[x]][d_key, on = c("chrpos" = chrpos), nomatch = NULL]
        return(dd)
      })
      rm(dlist)
    }
  }
  
  d_out <- d_out[,..cols]
  
  outnames <- switch(type,
                     "exposure" = c("CHR", "POS", "rsid", "EA_exposure", "NEA_exposure", "BETA_exposure", "SE_exposure", "P_exposure", "EAF_exposure"),
                     "outcome" = c("rsid", "EA_outcome", "NEA_outcome", "BETA_outcome", "SE_outcome", "P_outcome", "EAF_outcome"),
                     "pheno1" = c("CHR", "POS", "SNPID", "EA1", "NEA1", "BETA1", "SE1", "P1", "EAF1"),
                     "pheno2" = c("SNPID", "EA2", "NEA2", "BETA2", "SE2", "P2", "EAF2"),
                     NULL
  )
  
  setnames(d_out, cols, outnames)
  
  return(d_out)
}
