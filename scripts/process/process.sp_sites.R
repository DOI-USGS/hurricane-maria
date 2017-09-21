process.sp_sites <- function(viz = as.viz("sp-sites")){
  deps <- readDepends(viz)
  checkRequired(deps, c('sites', 'metadata'))
  sites <- deps[['sites']]
  metadata <- deps[['metadata']]
  
  sites <- sp::SpatialPointsDataFrame(
    cbind(sites$dec_long_va,sites$dec_lat_va), 
    data = sites[c('site_no', "station_nm")],
    proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  
  sites@data$station_nm <- as.character(sapply(sites@data$station_nm, siteCapper))
  
  storm.sites <- sp::spTransform(sites, sp::CRS(metadata$proj.string))
  
  saveRDS(storm.sites, viz[['location']])
  
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
}