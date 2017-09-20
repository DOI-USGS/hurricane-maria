#' Create a JSON file of timesteps to use in the animation
fetch.timesteps <- function(viz = as.viz('timesteps')){
  library(dplyr)
  precip.data <- readDepends(viz)[["compiled-precip"]]$precip_data
  attr(precip.data$time_stamp, "tzone") <- "America/Puerto_Rico"
  time.info <- precip.data %>% select(time_stamp) %>% 
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
