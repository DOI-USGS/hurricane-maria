#' take the compiled-precip object and turn it into 
#' a single sp.points.d.f w/ proper classes per timestep
process.cell_precip_sub <- function(viz){
  library(sp)
  deps <- readDepends(viz)
  checkRequired(deps, c("classified-precip", "clip"))
  clip <- deps$clip
  offset <- viz[["cell.offset"]]
  pts <- deps$`classified-precip`
  place <- viz[["place"]]
  
  # build a set of square polygons for each centroid
  pols.out <- list()
  for (j in seq_len(length(pts))){
    poly <- getPoly(pts[j,1]@coords[1], pts[j,1]@coords[2], id = paste0('p',j), offset)
    pols.out <- append(pols.out, poly)
  }
  
  
  SpP = SpatialPolygons(pols.out, 1:j, proj4string = CRS(proj4string(pts)))
  sp <- as(SpP, 'SpatialPolygonsDataFrame')
  sp@data <- pts@data
  
  
  # a lot of copy paste from `plotting_utils`, but that doesn't do quite what we want
  # if we do more than one island, this code doesn't work, so using PR only
  # We will take those square cells and remove those that aren't within PR
  # those that hit PR's border will be clipped to it
  g.i <- rgeos::gIntersects(sp, clip[clip$id == place,], byid = T) 
  
  out <- lapply(which(g.i), function(i) {
    sub_sp <- sp[i,]
    g.out <- rgeos::gIntersection(sub_sp, clip)
    if (!is.null(g.out) & length(g.out) == 1){
      row.names(g.out) <- row.names(sp)[i]  
      return(g.out)
    }        
  })
  
  clipped.sp <- do.call("rbind", out)
  data.out <- as.data.frame(sp)[g.i, ]
  row.names(data.out) <- row.names(sp)[g.i]
  clipped.sp <- as(object = clipped.sp, Class = class(sp))
  clipped.sp@data <- data.out
  
  
  saveRDS(clipped.sp, file = viz[['location']])
}

getPoly <- function(x,y, offset, id){
  # make a diamond using the offset 
  Sr1 = Polygon(cbind(c(x,x+offset,x,x-offset,x),c(y+offset,y, y-offset, y, y+offset)))
  Srs1 = Polygons(list(Sr1), id)
  return(Srs1)
}

process.cell_precip <- function(viz=as.viz("cell-precip")){
  deps <- readDepends(viz)
  
  # Could make this more automatic someday:
  PR <- deps[["cell-precip-PR"]]
  VI <- deps[["cell-precip-VI"]]

  if (!is.null(VI)){
    combo <- rbind(PR, VI)
  } else {
    combo <- PR
  }
  
  
  saveRDS(combo, file = viz[['location']])
  
}