process.order_sites <- function(viz = as.viz("order-sites")){
  
  library(dplyr)
  
  depends <- readDepends(viz)
  
  sites <- as.data.frame(depends[["sp-sites"]])
  
  reordered_sites <- sites %>%
    arrange(coords.x1) 
    
  saveRDS(reordered_sites, viz[['location']])
}
