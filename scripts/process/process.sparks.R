sparkbox.height <- 0.35
sparkbox.width <- 3.5

grab_spark <- function(vals){
  
  xs <- seq_len(length(vals))
  xs <- c(head(xs, 1), xs, tail(xs, 1))
  ys <- c(min(vals, na.rm = T), vals, min(vals, na.rm = T))
  x = svglite::xmlSVG({
    par(omi=c(0,0,0,0), mai=c(0,0,0,0))
    plot(xs, ys, type='l', axes=F, ann=F)
  }, height=sparkbox.height, width=sparkbox.width)
  xml2::xml_attr(xml2::xml_find_first(x, '//*[local-name()="polyline"]'),'points')
}

grab_clip_rect <- function(vals, flood){
  
  x = svglite::xmlSVG({
    par(omi=c(0,0,0,0), mai=c(0,0,0,0))
    plot(vals, type='l', axes=F, ann=F)
    abline(h = flood)
  }, height=sparkbox.height, width=sparkbox.width)
  y.out <- xml2::xml_attr(xml2::xml_find_first(x, '//*[local-name()="line"]'),'y1')
  if (is.na(y.out)){
    return("0")
  } else {
    return(y.out)
  }
}

offline_mark <- function(mask_values, gage_data, timesteps, vert_adjust, site_no){
  
  offline_index <- which(gage_data == mask_values)
  
  if(length(offline_index) == 0){
    circle = "0,0"
  } else {
    offline_index <- offline_index[1L]-1
    offline_height <- gage_data[offline_index]
    
    x = svglite::xmlSVG({
      par(omi=c(0,0,0,0), mai=c(0,0,0,0))
      plot(offline_index, offline_height,axes=F, ann=F,
           xlim = c(0,length(timesteps)),
           ylim=c(range(gage_data)))
    }, height=sparkbox.height, width=sparkbox.width)
    
    circle_x <- xml2::xml_attr(xml2::xml_find_first(x, '//*[local-name()="circle"]'),'cx')
    circle_y <- xml2::xml_attr(xml2::xml_find_first(x, '//*[local-name()="circle"]'),'cy')
    
    circle = paste(circle_x, circle_y, sep=",")
  }
  
  x <- svglite::xmlSVG({
    par(omi=c(0,0,0,0), mai=c(0,0,0,0))
    plot(1, 2, type='l', axes=F, ann=F)
  }, height=sparkbox.height, width=sparkbox.width)
  
  view_box <- strsplit(xml2::xml_attr(x, 'viewBox'), '[ ]')[[1]] %>% as.numeric()
  
  view_height <- view_box[4]
  view_width <- view_box[3]
  r.buffer <- 0.04 * view_width # R does a 4% buffer automatically need to take it into account it later.
  plotting.width <- view_width - 2 * r.buffer
  time.locations <- seq(from = r.buffer, to = view_width - r.buffer, 
                        length.out = length(timesteps))
  
  # compute the x range of 
  block_times <- which(gage_data == mask_values)
  if(length(block_times) > 1 && length(unique(diff(block_times))) != 1) {
    stop(paste("expected block_times to be contiguous for", site_no))
  }
  if(length(block_times) == 0) {
    block_times <- length(gage_data)
  } else {
    # append one additional timestep to the left to cover the transition
    block_times <- c(block_times[1]-1, block_times)
  }
  block_bounds <- range(time.locations[block_times])
  block_range <- diff(block_bounds)
  
  # prepare a rectangle clip path definition. we'll be using the time series of
  # stage points for the blocker line, so here we're just defining a rectanglar
  # area to clip to
  # x's are 8px long
  clip_params <- data_frame(
    circle = circle,
    x0=r.buffer,
    x1=block_bounds[1]-5,
    width=11,
    y0=-5,
    height=view_height + 10) %>%
    # make every column a character string with just 2 decimal places
    lapply(function(col) sprintf('%s', col)) %>%
    as_data_frame()
  
  return(clip_params)
}

grab_blocker <- function(mask_values, gage_data, timesteps, vert_adjust, site_no) {
  # grab an empty standard size plot to work with.
  x <- svglite::xmlSVG({
    par(omi=c(0,0,0,0), mai=c(0,0,0,0))
    plot(1, 2, type='l', axes=F, ann=F)
  }, height=sparkbox.height, width=sparkbox.width)
  
  view_box <- strsplit(xml2::xml_attr(x, 'viewBox'), '[ ]')[[1]] %>% as.numeric()
  
  view_height <- view_box[4]
  view_width <- view_box[3]
  r.buffer <- 0.04 * view_width # R does a 4% buffer automatically need to take it into account it later.
  plotting.width <- view_width - 2 * r.buffer
  time.locations <- seq(from = r.buffer, to = view_width - r.buffer, 
                        length.out = length(timesteps$times))
  
  # compute the x range of 
  block_times <- which(gage_data == mask_values)
  if(length(block_times) > 1 && length(unique(diff(block_times))) != 1) {
    stop(paste("expected block_times to be contiguous for", site_no))
  }
  if(length(block_times) == 0) {
    block_times <- length(gage_data)
  } else {
    # append one additional timestep to the left to cover the transition
    block_times <- c(block_times[1]-1, block_times)
  }
  block_bounds <- range(time.locations[block_times])
  block_range <- diff(block_bounds)
  
  # prepare a rectangle clip path definition. we'll be using the time series of
  # stage points for the blocker line, so here we're just defining a rectanglar
  # area to clip to
  clip_params <- data_frame(
    x0=r.buffer,
    x1=block_bounds[1],
    datawidth=x1-x0,
    width=diff(block_bounds),
    x3=view_width - r.buffer,
    y0=-5,
    height=view_height + 10) %>%
    # make every column a character string with just 2 decimal places
    lapply(function(col) sprintf('%1.2f', col)) %>%
    as_data_frame()
}

process.sparks <- function(viz = as.viz('sparks')){
  
  library(dplyr)
  depends <- readDepends(viz)
  checkRequired(depends, c("gage-data", "sites", "nws-data", "timesteps"))
  sites <- depends[["sites"]]
  gage_data <- depends[["gage-data"]]$timestep_q
  mask_values <- depends[["gage-data"]]$gage_mask_vals
  nws_data <- depends[["nws-data"]]
  timesteps <- depends[['timesteps']]
  
  sites <- sites[which(sites$site_no %in% names(gage_data)),]
  
  gage_sparks <- data.frame(points = sapply(sites$site_no, function(x) grab_spark(gage_data[[x]]), USE.NAMES = FALSE),
                       site_no = sites$site_no, station_nm = sites$station_nm, stringsAsFactors = FALSE) %>% 
    mutate(class = "sparkline", 
           id = sprintf("sparkline-%s", site_no), 
           "mask" = "url(#spark-opacity);",
           onmouseover=sprintf("setEmphasis('%s');", site_no), 
           onmouseout=sprintf("clearEmphasis();"),
           "clip-path"=sprintf("url(#stage-clip-%s)", site_no), 
           onclick=sprintf("openNWIS('%s', evt);", site_no),
           onmousemove=sprintf("hovertext('%s',evt);", station_nm)) %>%
    select(-station_nm)
  
  flood_sparks <- data.frame(y = sapply(sites$site_no, function(x) grab_clip_rect(gage_data[[x]], 
                                                                            nws_data$flood.stage[nws_data$site_no == x]), 
                                  USE.NAMES = FALSE),
                       site_no = sites$site_no, station_nm = sites$station_nm, stringsAsFactors = FALSE) %>% 
    mutate(class = "floodline", 
           id = sprintf("floodline-%s", site_no), 
           "mask" = "url(#flood-opacity);",
           "clip-path"=sprintf("url(#flood-clip-%s)", site_no), 
           onmouseover=sprintf("setEmphasis('%s');", site_no),
           onmouseout=sprintf("clearEmphasis();"),
           onclick=sprintf("openNWIS('%s', evt);", site_no),
           onmousemove=sprintf("hovertext('%s',evt);", station_nm)) %>%
    select(-station_nm)
  
  blockers <- bind_rows(lapply(sites$site_no, function(s) {
    grab_blocker(mask_values=mask_values[s], 
                 gage_data=gage_data[[s]],
                 timesteps,
                 vert_adjust=0, 
                 site_no=s)
  })) %>%
    mutate(
      site_no = sites$site_no,
      class = 'gage-blocker',
      id = paste0('blocker-',site_no),
      "mask" = "url(#flood-opacity);",
      "clip-path"=sprintf("url(#blocker-clip-%s)", site_no), 
      onmouseover=sprintf("setEmphasis('%s');", site_no),
      onmouseout=sprintf("clearEmphasis();"),
      onclick=sprintf("openNWIS('%s', evt);", site_no),
      onmousemove="hovertext('No Data Available',evt);") %>% 
    select(-site_no)
  
  
  view_width <- sparkbox.width * 72 # px per inch
  x.0 <- 0.04 * view_width
  x.1 <- view_width - 0.04 * view_width
  
  r.buffer <- 0.04 * view_width # R does a 4% buffer automatically need to take it into account it later.
  
  offline <- bind_rows(lapply(sites$site_no, function(s) {
    offline_mark(mask_values=mask_values[s], 
                 gage_data=gage_data[[s]],
                 timesteps$times,
                 vert_adjust=0, 
                 site_no=s)
  })) %>%
    
    mutate(
      site_no = sites$site_no,
      class = 'treasure-spark',
      id = paste0('offline-',site_no),
      "mask" = "url(#flood-opacity);",
      "clip-path"=sprintf("url(#offline-clip-%s)", site_no), 
      onmouseover=sprintf("setEmphasis('%s');", site_no),
      onmouseout=sprintf("clearEmphasis()"),
      onclick=sprintf("openNWIS('%s', evt);", site_no),
      onmousemove="hovertext('No Data Available',evt);") %>% 
    filter(circle != "0,0", circle != "NA,NA") %>% 
    mutate(cx = sapply(circle, function(x) strsplit(x,'[,]')[[1]][1])) %>% 
    mutate(cy = sapply(circle, function(x) strsplit(x,'[,]')[[1]][2])) %>% 
    mutate(d = sprintf('M%1.1f,0v0 M%1.2f,%1.2f l6,6 m0,-6 l-6,6 M%1.1f,0v0', x.0, as.numeric(cx)-3, as.numeric(cy)-3, x.1)) %>% 
    select(-site_no, -cx, -cy, -circle)
    
  
  
  
  saveRDS(list(gage_sparks = gage_sparks, 
               flood_sparks = flood_sparks, 
               gage_blockers = blockers,
               #spark_blockers = spark_blockers,
               offline_points = offline), viz[['location']])
}




