#' @export
# performs MR and creates output

perform_mr <- function(d_input = d_mr){
  mr_input_data <- MendelianRandomization::mr_input(
    bx = d_input$BETA_exposure,
    bxse = d_input$SE_exposure,
    by = d_input$BETA_outcome,
    byse = d_input$SE_outcome,

    snps = d_input$SNP
  )

  # TODO: edit options
  if(dim(d_input)[1] > 0){
    (res_ivwr <- MendelianRandomization::mr_ivw(mr_input_data, model = "random"))
  } else { cat(sprintf("No proxies found for MR of %s on %s\n", expo, outcome))}

  if(dim(d_input)[1] > 2){
    (res_egger <- MendelianRandomization::mr_egger(mr_input_data))
    (res_wm <- MendelianRandomization::mr_median(mr_input_data, weighting = "weighted", seed = 123))
    (res_wmode <- MendelianRandomization::mr_mbe(mr_input_data, weighting = "weighted", seed = 123))
  }

  ### output results
  data_frame <- d_input

  cat(sprintf("Saving objects\n"))
  # ADD HERE IF NEEDED
  objects_to_save <- ls()[ls() %in% c("d_input", "res_ivwr", "res_egger", "res_wm", "res_wmode")]

  output <- paste0(c(expo, args[-1]), collapse="_")

  save(list=objects_to_save, file = paste0("results/", output, ".RData"))


}
