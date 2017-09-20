process.compile_precip <- function(viz = as.viz('compile-precip')) {
  library(dplyr)
  
  tar_files <- unlist(readDepends(viz)[["precip-shapefiles"]]) 
  
  temp_precip_dir <- file.path(dirname(tar_files[1]), "untar")
  
  for(tFile in tar_files) {
    untar(tarfile = tFile, compressed = "gzip", exdir = temp_precip_dir)
  }
  
  shp_files <- list.files(temp_precip_dir, pattern = "*.shp", recursive = T)
  precip_data <- data.frame()
  
  for(sFile in shp_files) {
    sFolder <- file.path(temp_precip_dir, dirname(sFile))
    
    sLayer <- gsub(".shp","", basename(sFile))
    
    sData <- rgdal::readOGR(sFolder, sLayer, stringsAsFactors = F)@data %>%
      filter(Lat < 19) %>%
      select(Id, Lat, Lon, Globvalue) %>%
      rename(id = Id, lat = Lat, lon = Lon, precip_inches = Globvalue)
    
    if(nrow(sData) > 0) {
      sData$time_stamp <- as.POSIXct(strptime(gsub("nws_precip_", "", sLayer), format = "%Y%m%d%H", tz = "UTC"))
      
      precip_data <- dplyr::bind_rows(precip_data, sData)
    }
  }
  
  unlink(temp_precip_dir , recursive = T)
  
  precip_data_points <- precip_data %>% 
    select(id, lat, lon) %>%
    distinct()
  
  precip_data_points <- 
    sp::SpatialPointsDataFrame(precip_data_points[c("lon", "lat")],
                               data = data.frame(id=precip_data_points$id,
                                                 stringsAsFactors = F),
                               proj4string = sp::CRS("+init=EPSG:4326"))
  
  precip_data <- precip_data %>% 
    select(id, precip_inches, time_stamp)
    
  saveRDS(list(precip_cell_points = precip_data_points, precip_data = precip_data), 
          viz[["location"]])
}




