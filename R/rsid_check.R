#' @export
# check rsid column
rsid_check <- function(d_out = d_out, chrpos_column = chrpos_column,
                       keyfile = keyfile, no_rsid = no_rsid){

if(!(is.null(no_rsid))){
  if(no_rsid){
    # cat(sprintf("No rsids in the summary statistics for %s\n", phenotype))
    if(is.null(chrpos_column)){
      d_out[,"chrpos_column" := paste0(CHR, ":", POS)]
      chrpos_column <- "chrpos_column"
    } else {
      d_out[,c("CHR", "POS") := lapply(1:2, function(x) sapply(strsplit(get(chrpos_column), ":"), "[[", x))]
    }
    
    d_out <- d_out[CHR %in% 1:22,]
    dlist <- split(d_out, d_out[["CHR"]])
    d_out <- rbindlist(lapply(names(dlist), function(x){
      d_key <- read_key(file = keyfile, chr = x)
      d0 <- dlist[[x]]
      # dd <- merge.data.table(d0, d_key, by.x = get(chrpos_column), by.y = "chrpos")
      dd <- d_key[d0, on = c("chrpos" = (chrpos_column)), nomatch = NULL]
      return(dd)
    }))
    rm(dlist)
  }
}
  
return(d_out)
}
