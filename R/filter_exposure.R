#' @import data.table
#' @import ieugwasr
#' @import MendelianRandomization


#' @export
filter_exposure <- function(d = d_exposure, pthresh = pthresh_exposure){
  d_out <- d[pval < pthresh_exposure,]
  return(d_out)
}
