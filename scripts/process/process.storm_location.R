#' Make a 
process.storm_location <- function(viz = as.viz('storm-location')){
  
  message("someday we should support the pattern of multiple layers in a single shapefile zip")
  library(sp)
  library(dplyr)
  
  depends <- readDepends(viz)
  
  # -- read the shapefile --
  
  # get the hurricane-track info
  track.meta <- getContentInfo("hurricane-track")
  
  # write a time parser for identifying times in the shapefiles
  as.time <- function(YEAR, MONTH, DAY, HHMM){
    as.POSIXct(sprintf('%s-%s-%s %s', YEAR, MONTH, DAY, HHMM), format='%Y-%m-%d %H%M', tz="America/Puerto_Rico")
  }
  
  # make a very temporary directory to work in
  shp.path <- file.path(tempdir(), 'tmp')
  if (!dir.exists(shp.path)){
    dir.create(shp.path)
  }
  
  # unpack the zipped shapefiles into the temptempdir
  unzip(track.meta$location, exdir = shp.path)
  
  # read and munge the shapefile data, filtering to this storm and parsing DateTimes
  warning('NWS file pattern is hard-coded here and is storm-specific. They may change it?')
  # figure out which layer to read
  pts.layer <- sprintf("%s%s%s_pts", toupper(track.meta$ocean), track.meta$stormnum, track.meta$year)
  # read and parse that layer
  shp.data <- rgdal::readOGR(shp.path, layer = pts.layer) %>% data.frame %>% 
    filter(STORMNAME==track.meta$storm) %>% 
    mutate(DateTime = as.time(YEAR, MONTH, DAY, HHMM)) %>% 
    select(LAT, LON, DateTime, INTENSITY)
  
  # delete our very temporary directory now that the shapefiles are read in
  unlink(shp.path)
  
  # get the vector of timepoints we want to include
  times <- as.POSIXct(strptime(depends[['timesteps']]$times, format = '%b %d %I:%M %p', tz = "America/Puerto_Rico"))
  
  # interpolate latitudes and longitudes to the timestamps we actually want
  lat.out <- approx(shp.data$DateTime, shp.data$LAT, xout = times)$y
  lon.out <- approx(shp.data$DateTime, shp.data$LON, xout = times)$y
  pts <- cbind(lon.out[!is.na(lon.out)], lat.out[!is.na(lon.out)])
  location <- SpatialPoints(pts, proj4string=CRS("+proj=longlat +datum=WGS84"))

  
  data.out <- data.frame(id = paste0('storm-', 1:length(location)), 
                         class = "storm-dot", 
                         r = "12", 
                         stringsAsFactors = FALSE) 
  row.names(data.out) <- row.names(location)
  sp.data.frame <- as(object = location, Class = paste0(class(location), "DataFrame"))
  sp.data.frame@data <- data.out
  row.names(sp.data.frame) <- row.names(data.out)
  
  
  location <- spTransform(sp.data.frame, CRS(depends[['view-limits-mobile']]$proj.string))
  
  saveRDS(location, viz[['location']])
}
