
precip_to_class <- function(precip, p.times, times.out, breaks, compress = TRUE){
  
  
  if (p.times[1] != times.out[1]){
    p.times <- c(times.out[1], p.times)
    precip <- c(0, precip)
  }
  
  if (tail(p.times, 1) != tail(times.out, 1)){
    p.times <- c(p.times, tail(times.out, 1))
    precip <- c(precip, tail(precip, 1))
  }
  precip.full <- approx(x = p.times, y = precip, xout = times.out, method = 'constant')$y
  bins <- cut(precip.full, breaks = breaks, labels = F, right = FALSE)
  if (compress){
    changed <- which(as.logical(c(TRUE, diff(bins))))  
    classes <- paste(' p-', changed, '-', bins[changed], sep = '', collapse = '')
  } else {
    classes <- paste(' p', seq_len(length(precip.full)), bins, sep='-', collapse = '')
  }
  return(paste0('precip-cell', classes))
  
}


#' take the compiled-precip object and turn it into 
#' a single sp.points.d.f w/ proper classes per timestep
process.classified_precip <- function(viz){
  deps <- readDepends(viz)
  checkRequired(deps, c("compiled-precip", "timesteps", "precip-breaks"))
  
  precip.data <- deps$`compiled-precip`$precip_data
  attr(precip.data$time_stamp, "tzone") <- "America/Puerto_Rico"
  times <- as.POSIXct(strptime(deps[['timesteps']]$times, format='%b %d %I:%M %p', tz="America/Puerto_Rico"))
  precip.breaks <- deps$`precip-breaks`
  
  library(dplyr)
  precip.classified <- precip.data %>% 
    group_by(id) %>% arrange(time_stamp) %>% 
    summarize(class = precip_to_class(precip_inches, time_stamp, times, precip.breaks))
  
  
  sp.out <- deps$`compiled-precip`$precip_cell_points
  
  # we'd be missing coverage if this were true:
  stopifnot(length(sp.out) == nrow(precip.classified))
  
  sp.data <- sp.out@data %>% 
    left_join(precip.classified)
  sp.out@data <- sp.data
  saveRDS(sp.out, file = viz[['location']])
}