#' Search for proxies
#' 
#' Function to search proxies for instrumental variables not found in the outcome summary statistics.
#' Not implemented yet.
#' 
#' @param r2 $r^2$ parameter for proxies
#'
#' @param d_input Input data
#'

proxysearch <- function(r2 = proxy_r2, d_input = d_mr){
  
  if(proxy_r2 < 0){
    cat(sprintf("No proxysearch\n"))
    d_out <- d_input
    } else {
      # TODO: implement proxysearch
      cat(sprintf("Proxysearch not yet implemented!\n"))
      d_out <- d_input
    }

  return(d_out)
}
