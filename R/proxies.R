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
