

fetchTimestamp.islands <- alwaysCurrent

fetch_map_data <- function(..., viz){
  
  view <- readDepends(viz)[['view-limits']]
  out <- to_sp(..., proj.string = view[['proj.string']], 
               within = as.sp_box(view$xlim, view$ylim, CRS(view[['proj.string']])))
  saveRDS(out, viz[['location']])
  
  fetchTimestamp(viz) # update the timestamp to match the newly written file (optional but saves one fetch)
}

fetch.islands <- function(viz = as.viz('islands')){
  fetch_map_data('world2Hires', "(?!USA)", xlim = c(275, 300), ylim = c(16, 30), viz = viz)
}
