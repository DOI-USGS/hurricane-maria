process.order_sites <- function(viz = as.viz("order-sites")){
  
  library(dplyr)
  
  depends <- readDepends(viz)
  
  sites <- depends[["nwis-sites"]]
  
  reordered_sites <- sites %>%
    arrange(desc(dec_long_va)) 
  
  reordered_sites$station_nm <- sapply(reordered_sites$station_nm, siteCapper)
    
  saveRDS(reordered_sites, viz[['location']])
}

siteCapper <- function(x) {
  rules <- list(PR = "PR", NR = "near", DE = "de", 
                BLW = "below", AT = "at", LA = "la", 
                ABV = "above")
  
  s <- strsplit(x, " ")[[1]]
  s[which(!s %in% names(rules))] <- stringi::stri_trans_general(s[which(!s %in% names(rules))],
                                                                id = "Title")
  s[which(s %in% names(rules))] <- unlist(rules[s[which(s %in% names(rules))]])
  s <- paste(s, collapse=" ")
  return(s)
}
