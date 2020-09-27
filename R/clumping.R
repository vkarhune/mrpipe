#' @export
clumping <- function(d_input = d, r2 = clump_r2, kb = clump_kb, bychr = T, ...){
  if(r2 < 0){
    cat(sprintf("No clumping done\n"))
    d_out <- d_input
  } else {
    dots <- list(...)
    # TODO: bychr
    if(!bychr){ cat(sprintf("Option not to clump by chr not implemented yet!\n")) }
    d_out <- data.table::rbindlist(
      lapply(split(d_input, d_input$chr_name),function(d_nonclumped){
        counter <- 0
        while(counter < 10){
          tryCatch( {d_output <- ieugwasr::ld_clump(d_nonclumped,
                                                    clump_kb = kb, clump_r2 = r2, dots)},
                    error = function(error){
                    cat(paste0(error))
                    cat("Recovered from error in ieugwasr::ld_clump()\n")
                  } )
        if(exists("d_output")){ counter <- 10
        } else {
          counter <- counter + 1
          cat(counter,"\n")
        }
      }

      return(d_output)

    }))

  }

  return(d_out)

}
