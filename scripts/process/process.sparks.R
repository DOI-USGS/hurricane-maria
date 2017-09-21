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

process.sparks <- function(viz = as.viz('sparks')){
  library(dplyr)
  depends <- readDepends(viz)
  checkRequired(depends, c("gage-data", "sites", "nws-data"))
  sites <- depends[["sites"]]
  gage_data <- depends[["gage-data"]]$timestep_q
  nws_data <- depends[["nws-data"]]
  
  sites <- sites[which(sites$site_no %in% names(gage_data)),]
  
  gage_sparks <- data.frame(points = sapply(sites$site_no, function(x) grab_spark(gage_data[[x]]), USE.NAMES = FALSE),
                       site_no = sites$site_no, station_nm = sites$station_nm, stringsAsFactors = FALSE) %>% 
    mutate(class = "sparkline", 
           id = sprintf("sparkline-%s", site_no), 
           "mask" = "url(#spark-opacity);",
           onmouseover=sprintf("setBold('nwis-%s');", site_no), 
           onmouseout=sprintf("setNormal('nwis-%s');hovertext(' ');", site_no),
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
           onmouseover=sprintf("setBold('nwis-%s');setBold('sparkline-%s');", site_no, site_no),
           onmouseout=sprintf("setNormal('nwis-%s');setNormal('sparkline-%s');hovertext(' ');", site_no, site_no),
           onclick=sprintf("openNWIS('%s', evt);", site_no),
           onmousemove=sprintf("hovertext('%s',evt);", station_nm)) %>%
    select(-station_nm)
          
  saveRDS(list(gage_sparks = gage_sparks, flood_sparks = flood_sparks), viz[['location']])
}

#' create a path that greys out the area where the data aren't available
process.gage_blocker <- function(viz = as.viz('gage-blocker')){
  library(xml2)
  library(dplyr)
  depends <- readDepends(viz)
  checkRequired(depends, c("timesteps", "gage_info"))
  
  # grab an empty standard size plot to work with.
  x <- svglite::xmlSVG({
    par(omi=c(0,0,0,0), mai=c(0,0,0,0))
    plot(1, 2, type='l', axes=F, ann=F)
    }, height=sparkbox.height, width=sparkbox.width)
  
  view_box <- strsplit(xml_attr(x, 'viewBox'), '[ ]')[[1]] %>% as.numeric()
  
  r.buffer <- 0.04 * view_box[3] # R does a 4% buffer automatically need to take it into account it later.
  plotting.width <- view_box[3] - 2 * r.buffer
  height <- view_box[4]
  time.locations <- seq(from = r.buffer, to = view_box[3] - r.buffer, 
                        length.out = length(depends[['timesteps']]$times))
  
  # no matter what, we need a zero-length dot at the beginning of this set of times, and at the end, so the width of the 
  # elements are always the same (for the "reveal" animation). The zero length is the `v0`, which is a vertical line of length 0
  # Z "closes" a part of a path by drawing a straight line to the previous
  
  mask_values <- depends[["gage_info"]]$gage_mask_vals
  gage_data <- depends[["gage_info"]]$timestep_q
  blockers <- data.frame(d = rep(NA_character_, length(gage_data)), 
                         id = NA_character_,
                         "mask" = "url(#flood-opacity);", 
                         stringsAsFactors = FALSE, class = 'gage-blocker')
  
  # these need to be in the same order (and same length) as the site sparklines, otherwise we need to treat them differently in visualize
  blck.i <- 1
  
  vert_adjust <- length(names(mask_values)) * 0.1
  
  for(site in names(mask_values)) {
    
    block_times <- which(gage_data[[site]] == mask_values[site]) # assuming these are contiguous for now!
    
    if(length(block_times) == 0) {
      block_times <- length(gage_data[[site]])
    }
    
    block_range <- diff(time.locations[c(block_times[1], block_times[length(block_times)])])
    
    blockers$d[blck.i] = sprintf('M%1.2f,0v0 M%1.2f,%1.2fv%1.2f h%1.2f v-%1.2fZ M%1.2f,0v0', 
                                       r.buffer, 
                                       time.locations[block_times[1]],
                                       vert_adjust,
                                       height-vert_adjust, 
                                       block_range, 
                                       height-vert_adjust, 
                                       view_box[3] - r.buffer)
    blockers$id[blck.i] <- paste0('blocker-',site)
    blck.i <- blck.i + 1
  }
  
  # to use this, we'd need the data.frame to have a row for each gage and be in the same order as the sparks (or make sure the ids match)
  # then we'd need to change a line in the visualize.hurricane_map code to use a do.call instead of just the `d` element here
  saveRDS(blockers, file = viz[['location']])
}
