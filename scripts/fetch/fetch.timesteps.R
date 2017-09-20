#' Create a JSON file of timesteps to use in the animation
fetch.timesteps <- function(viz = as.viz('timesteps')){
  library(dplyr)
  times <- 
    # readDepends(viz)[["precip-cell-data"]] %>% # DO THIS WHEN PRECIP IS AVAILBLE
    # select(DateTime) %>% 
    data.frame(DateTime=seq(
      as.POSIXct('2017-09-18 00:00:00', tz = "America/Puerto_Rico"),
      as.POSIXct('2017-09-20 00:00:00', tz = "America/Puerto_Rico"),
      by=as.difftime(1, units='hours'))) %>% # PLACEHOLDER UNTIL PRECIP DATA
    unique() %>% 
    pull(DateTime) %>% 
    as.POSIXct %>% 
    format('%b %d %I:%M %p')
  cat(jsonlite::toJSON(list(times=times)), file = viz[['location']])
}

fetchTimestamp.timesteps <- alwaysCurrent
