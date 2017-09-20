process.order_sites <- function(viz = as.viz("order-sites")){
  
  library(dplyr)
  
  depends <- readDepends(viz)
  
  sites <- depends[["nwis-sites"]]
  
  reordered_sites <- sites %>%
    arrange(desc(dec_long_va))
    
  
  
  saveRDS(reordered_sites, viz[['location']])
}