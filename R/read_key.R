#' @export
# reads chrpos-rsid correspondence file

read_key <- function(file = NULL, chr = NULL){
  # load(file)
  # out <- get(paste0("chr", chr))
  out <- readRDS(paste0(file, "chr", chr, ".Rds"))
  return(out)
}