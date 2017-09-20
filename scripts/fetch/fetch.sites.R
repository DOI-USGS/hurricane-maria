fetch.sites <- function(viz = as.viz("nwis-sites")){
  library(dplyr)
  
  depData <- readDepends(viz)
  site_table <- depData[["nws"]]
  params <- depData[["config"]]
  dates <- depData[["dates"]]
  
  USGS_sites <- dataRetrieval::readNWISdata(sites = site_table$USGS,
                                            service="site",
                                            seriesCatalogOutput=TRUE)
                                            
  sites <- USGS_sites %>%
    filter(parm_cd %in% params[["pCode"]],
           data_type_cd %in% "uv") %>%
    left_join(select(site_table, NWS, site_no=USGS), by="site_no") %>%
    select(NWS, site_no, dec_lat_va, dec_long_va, station_nm) %>%
    distinct()
  
  saveRDS(sites, file=viz[["location"]])
}

fetchTimestamp.sites <- alwaysCurrent
