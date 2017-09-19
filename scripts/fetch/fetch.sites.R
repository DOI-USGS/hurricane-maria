fetch.sites <- function(viz = as.viz("nwis-sites")){
  library(dplyr)
  
  depData <- readDepends(viz)
  site_table <- depData[["nws"]]
  params <- depData[["config"]]
  dates <- depData[["dates"]]
  
  USGS_sites <- dataRetrieval::readNWISdata(sites = site_table$USGS,
                                            service="site",
                                            seriesCatalogOutput=TRUE)
                                            
  USGS_sites <- USGS_sites %>%
    filter(parm_cd %in% params[["pCode"]])
  
  saveRDS(conversion.table.all, file=viz[["location"]])
}