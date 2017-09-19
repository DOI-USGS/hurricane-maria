#' take map arguments and return a projected sp object
#' 
#' @param \dots arguments passed to \code{\link[maps]{map}} excluding \code{fill} and \code{plot}
#' 
library(maps)
library(mapdata)
library(sp)
to_sp <- function(..., proj.string, within = NULL){
  library(maptools)
  library(maps)
  map <- maps::map(..., fill=TRUE, plot = FALSE)
  IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
  map.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  map.sp.t <- spTransform(map.sp, CRS(proj.string))
  
  if (!is.null(within)){
    g.i <- rgeos::gIntersects(map.sp.t, within, byid = T) 
    map.sp.t <- map.sp.t[g.i]
  }
  
  return(map.sp.t)
}