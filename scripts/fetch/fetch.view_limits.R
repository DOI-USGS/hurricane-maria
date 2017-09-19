fetch.view_limits <- function(viz = as.viz('view-limits-mobile')){
  bbox <- viz[['bbox']]
  metadata <- readDepends(viz)[['metadata']]
  sp.bbox <- as.sp_box(xs = bbox[c(1,3)], ys = bbox[c(2,4)], 
                       proj.string = sp::CRS("+proj=longlat +datum=WGS84"))
  bbox.transformed <- sp::spTransform(sp.bbox, metadata[['proj.string']])
  
  view.limits <- get_sp_lims(bbox.transformed, # it would be nice to just use defaults if these aren't included:
                             width = metadata[['width']], 
                             height = metadata[['height']], 
                             pointsize = metadata[['pointsize']])
  
  out <- append(view.limits, 
                list(proj.string = metadata[['proj.string']],
                     width = metadata[['width']], 
                     height = metadata[['height']], 
                     pointsize = metadata[['pointsize']]))
  
  saveRDS(out, viz[['location']])
}

fetchTimestamp.view_limits <- function(viz){
  NULL
}