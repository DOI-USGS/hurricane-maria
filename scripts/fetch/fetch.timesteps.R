#' Create a JSON file of timesteps to use in the animation
fetch.timesteps <- function(viz = as.viz('timesteps')){
  library(dplyr)
  tar_files <- unlist(readDepends(viz)[["precip-shapefiles"]])
  
  # basenames like: nws_precip_2017092001.tar.gz
  time_stamps <- gsub(".tar.gz", "", 
                      gsub("nws_precip_", "", 
                           basename(tar_files)))
  
  time_stamps <- as.POSIXct(strptime(time_stamps,
                                     format = "%Y%m%d%H", 
                                     tz = "UTC"))
  
  attr(time_stamps, "tzone") <- "America/Puerto_Rico"
  
  time_stamps <- data.frame(time_stamp = time_stamps)
  
  time.info <- time_stamps %>% 
    summarize(start.date = min(time_stamp), end.date = max(time_stamp))
  times <- data.frame(DateTime=seq(
    from = time.info$start.date, 
    to = time.info$end.date, 
    by='hours')) %>% 
    pull(DateTime) %>% 
    as.POSIXct %>% 
    format('%b %d %I:%M %p')
  cat(jsonlite::toJSON(list(times=times)), file = viz[['location']])
}

fetchTimestamp.timesteps <- vizlab:::fetchTimestamp.file
