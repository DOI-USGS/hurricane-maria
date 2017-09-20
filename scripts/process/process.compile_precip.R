process.compile_precip <- function(viz = as.viz('compile-precip')) {
  library(dplyr)
  
  ## Grab files from the fetcher and untar along side them.
  tar_files <- unlist(readDepends(viz)[["precip-shapefiles"]]) 
  temp_precip_dir <- file.path(dirname(tar_files[1]), "untar")
  for(tFile in tar_files) {
    untar(tarfile = tFile, compressed = "gzip", exdir = temp_precip_dir)
  }
  
  ## Get the list of shapefiles.
  shp_files <- list.files(temp_precip_dir, pattern = "*.shp", recursive = T)
  precip_data <- data.frame()
  
  ## loop over shapefiles
  for(sFile in shp_files) {
    # Grab the file path and setup ogr inputs
    sFolder <- file.path(temp_precip_dir, dirname(sFile))
    sLayer <- gsub(".shp","", basename(sFile))
    
    # Using readOGR to read shapefile but getting rid of spatial.
    sData <- rgdal::readOGR(sFolder, sLayer, stringsAsFactors = F)@data %>%
      dplyr::filter(Lat < 19) %>% # PR is far south!!!
      dplyr::select(Id, Lat, Lon, Globvalue) %>%
      dplyr::rename(id = Id, lat = Lat, lon = Lon, precip_inches = Globvalue)
    
    # Some don't have data in PR
    if(nrow(sData) > 0) {
      # Grab time stamp. Verified UTC by file refresh rate.
      sData$time_stamp <- as.POSIXct(strptime(gsub("nws_precip_", "", sLayer), 
                                              format = "%Y%m%d%H", tz = "UTC"))
      
      precip_data <- dplyr::bind_rows(precip_data, sData)
    }
  }
  
  # Delete the temporary folder.
  unlink(temp_precip_dir , recursive = T)
  
  # Grab unique points and create spatialPointsDataFrame
  precip_data_points <- precip_data %>% 
    select(id, lat, lon) %>%
    distinct()
  precip_data_points <- 
    sp::SpatialPointsDataFrame(precip_data_points[c("lon", "lat")],
                               data = data.frame(id=precip_data_points$id,
                                                 stringsAsFactors = F),
                               proj4string = sp::CRS("+init=EPSG:4326"))
  
  # Select data needed in output.
  precip_data <- precip_data %>% 
    dplyr::select(id, precip_inches, time_stamp)
  
  saveRDS(list(precip_cell_points = precip_data_points, 
               precip_data = precip_data), 
          viz[["location"]])
}




