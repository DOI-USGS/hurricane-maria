#' Make a SpatialLines object from the storm track data
process.storm_track <- function(viz = as.viz('storm-track')){
  library(rgeos)
  library(sp)
  depends <- readDepends(viz)
  view.lims <- depends[['view-limits-mobile']]
  track <- depends[["hurricane-track"]]

  track <- spTransform(track, sp::CRS(view.lims$proj.string))
  track <- as(track, 'SpatialLines')
  
  # here do "over" analysis for masking?
  
  saveRDS(track, viz[['location']])
}

