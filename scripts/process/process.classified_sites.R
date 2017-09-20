process.classified_sites <- function(viz = as.viz("classified-sites")) {
  deps <- readDepends(viz)
  checkRequired(deps, c('sites','gage-height','nws-threshold', 'config'))
  nwis_sites <- deps[['sites']]
  gage_data <- deps[["gage-height"]]
  nws_data <- deps[["nws-threshold"]]
  config_data <- deps[["config"]]
  stopifnot(config_data$pCode == "00065")
  
  site_nos <- names(gage_data)
  
  class_df <- data.frame(stringsAsFactors = FALSE)
  #actually create the classes
  for(site in site_nos) {
    flood_stage <- nws_data$flood.stage[which(nws_data$site_no == site)]
    which_floods <- which(gage_data[[site]]$y > flood_stage)
    site_class <- paste(paste("f", which_floods, sep = "-"), collapse = " ")
    class_df_row <- data.frame(site_no = site, class = site_class, 
                               stringsAsFactors = FALSE)
    class_df <- dplyr::bind_rows(class_df, class_df_row)
  }
  
  sites <- nwis_sites[which(nwis_sites@data$site_no %in% site_nos), ]
  
  sites@data <- data.frame(id = paste0('nwis-', sites@data$site_no), 
                                    class = 'nwis-dot', 
                                    r = '3.5',
                                    onmousemove = sprintf("hovertext('USGS %s',evt);", sites@data$site_no),
                                    onmouseout = sprintf("hovertext(' ');", sites@data$site_no), 
                                    onclick = sprintf("openNWIS('%s', evt);", sites@data$site_no), 
                                    stringsAsFactors = FALSE)
  saveRDS(sites, file = viz[['location']])
}