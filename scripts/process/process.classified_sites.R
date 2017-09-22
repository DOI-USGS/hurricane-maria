process.classified_sites <- function(viz = as.viz("classified-sites")) {
  deps <- readDepends(viz)
  checkRequired(deps, c('sites','gage-height','nws-threshold', 'config'))
  nwis_sites <- deps[['sites']]
  gage_data <- deps[["gage-height"]]$timestep_q
  nws_data <- deps[["nws-threshold"]]
  config_data <- deps[["config"]]
  stopifnot(config_data$pCode == "00065")
  nils <- deps[["gage-height"]]$gage_mask_vals
  
  site_nos <- names(gage_data)
  
  library(sp)
  library(dplyr)
  
  sites <- nwis_sites[which(nwis_sites@data$site_no %in% site_nos), ]
  
  class_df <- data.frame(stringsAsFactors = FALSE)
  #actually create the classes
  for(site in site_nos) {
    flood_stage <- nws_data$flood.stage[which(nws_data$site_no == site)]
    which_floods <- which(gage_data[[site]] > flood_stage)
    
    offline <- nils[site]
    which_offline <- which(gage_data[[site]] == offline)
    
    flood_online <- which_floods[!(which_floods %in% which_offline)]
    
    site_class_flood <- paste(paste("f", flood_online, sep = "-"), collapse = " ")
    site_class_offline <- paste(paste("off", which_offline, sep = "-"), collapse = " ")
    
    if(site_class_flood == "f-") site_class_flood <- ""
    if(site_class_offline == "off-") site_class_offline <- ""
    
    nwis_class <- which(!(1:length(gage_data[[site]]) %in% c(which_offline, which_floods)))
    site_class_nwis <- paste(paste("nwis-dot", nwis_class, sep = "-"), collapse = " ")
    
    site_class <- paste(site_class_flood, site_class_offline, site_class_nwis)
    
    class_df_row <- data.frame(site_no = site, class = site_class, 
                               stringsAsFactors = FALSE)
    class_df <- dplyr::bind_rows(class_df, class_df_row)
  }
  
  site_data <- data.frame(site_no = sites@data$site_no,
                          id = paste0('nwis-', sites@data$site_no), 
                          r = '5',
                          onmousemove = sprintf("hovertext('%s',evt);", sites@data$station_nm),
                          onmouseout = "hovertext(' ');", 
                          onclick = sprintf("openNWIS('%s', evt);", sites@data$site_no), 
                          stringsAsFactors = FALSE) %>%
    left_join(class_df, by="site_no") %>%
    select(-site_no)
  
  sites@data <- site_data
  
  saveRDS(sites, file = viz[['location']])
}
