#' @export
# calculate F statistic for a genetic variant
Fstat <- function(R2 = R2, n = NULL, k = 1){
  out <- (R2*(n - 1 - k))/((1 - R2)*k)
  return(out)
}