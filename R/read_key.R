read_key <- function(file = NULL, chr = NULL){
  load(file)
  out <- get(paste0("chr", chr))
  return(out)
}