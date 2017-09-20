process.classified_sites <- function(viz){
  deps <- readDepends(viz)
  checkRequired(deps, c('sites','gage-height','nws-threshold'))
  sites <- deps[['sites']]
  site.no <- sites$site_no

  
  sites@data <- data.frame(id = paste0('nwis-', site.no), 
                                    class = 'nwis-dot', 
                                    r = '3.5',
                                    onmousemove = sprintf("hovertext('USGS %s',evt);", site.no),
                                    onmouseout = sprintf("hovertext(' ');",site.no), 
                                    onclick = sprintf("openNWIS('%s', evt);", site.no), 
                                    stringsAsFactors = FALSE)
  saveRDS(sites, file = viz[['location']])
}