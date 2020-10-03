#' @export
# define filename
filename <- function(file = NULL, phenotype = phenotype){

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

return(file)
}

