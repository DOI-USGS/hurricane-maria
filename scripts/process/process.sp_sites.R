process.sp_sites <- function(viz){
  deps <- readDepends(viz)
  checkRequired(deps, c('sites', 'metadata'))
  sites <- deps[['sites']]
  metadata <- deps[['metadata']]
  
  sites <- sp::SpatialPointsDataFrame(
    cbind(sites$dec_long_va,sites$dec_lat_va), 
    data = sites[c('site_no')],
    proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  
  storm.sites <- sp::spTransform(sites, sp::CRS(metadata$proj.string))
  
  saveRDS(storm.sites, viz[['location']])
  
}