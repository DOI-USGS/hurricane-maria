sparkbox.height <- 0.4
sparkbox.width <- 2
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

process.gage_sparks <- function(viz = as.viz('gage-sparks')){
  library(dplyr)
  depends <- readDepends(viz)
  checkRequired(depends, c("gage-data", "sites"))
  
  sites <- depends[["sites"]]
  gage_data <- depends[["gage-data"]]
  
  sites <- sites[which(sites$site_no %in% names(gage_data)),]

  sparks <- data.frame(points = sapply(sites$site_no, function(x) grab_spark(gage_data[[x]]), USE.NAMES = FALSE),
                       site_no = sites$site_no, station_nm = sites$station_nm, stringsAsFactors = FALSE) %>% 
    mutate(class = "sparkline", 
           id = sprintf("sparkline-%s", site_no), 
           style = "mask: url(#spark-opacity);",
           onmouseover=sprintf("setBold('nwis-%s');", site_no), 
           onmouseout=sprintf("setNormal('nwis-%s');hovertext(' ');", site_no),
           onclick=sprintf("openNWIS('%s', evt);", site_no),
           onmousemove=sprintf("hovertext('%s',evt);", station_nm)) %>%
    select(-station_nm)
          

  saveRDS(sparks, viz[['location']])
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

# Should rename at some point. Not always going to be discharge.
process.flood_sparks <- function(viz = as.viz('flood-sparks')){
  library(dplyr)
  depends <- readDepends(viz)
  checkRequired(depends, c("gage-data", "sites", "nws-data"))
  
  sites <- depends[["sites"]]
  nws_data <- depends[["nws-data"]]
  gage_data <- depends[["gage-data"]]
  
  sites <- sites[which(sites$site_no %in% names(gage_data)),]

  sparks <- data.frame(y = sapply(sites$site_no, function(x) grab_clip_rect(gage_data[[x]], 
                                                                            nws_data$flood.stage[nws_data$site_no == x]), 
                                       USE.NAMES = FALSE),
                       site_no = sites$site_no, station_nm = sites$station_nm, stringsAsFactors = FALSE) 
  
  sparks <- sparks %>% 
    mutate(class = "floodline", 
           id = sprintf("floodline-%s", site_no), 
           style = "mask: url(#flood-opacity);",
           "clip-path"=sprintf("url(#flood-clip-%s)", site_no), 
           onmouseover=sprintf("setBold('nwis-%s');setBold('sparkline-%s');", site_no, site_no),
           onmouseout=sprintf("setNormal('nwis-%s');setNormal('sparkline-%s');hovertext(' ');", site_no, site_no),
           onclick=sprintf("openNWIS('%s', evt);", site_no),
           onmousemove=sprintf("hovertext('%s',evt);", station_nm)) %>%
    select(-station_nm)
           
  saveRDS(sparks, viz[['location']])
}

#' create a path that greys out the area where the data aren't available
process.gage_blocker <- function(viz = as.viz('gage-blocker')){
  library(xml2)
  library(dplyr)
  depends <- readDepends(viz)
  checkRequired(depends, c("timesteps")) # more to come when we use real data
  x = svglite::xmlSVG({
    par(omi=c(0,0,0,0), mai=c(0,0,0,0))
    plot(1, 2, type='l', axes=F, ann=F)
    }, height=sparkbox.height, width=sparkbox.width)
  
  vbox <- strsplit(xml_attr(x, 'viewBox'), '[ ]')[[1]] %>% as.numeric()
  
  r.buffer <- 0.04 * vbox[3] # R does a 4% buffer automatically
  plotting.width <- vbox[3] - 2 * r.buffer
  height <- vbox[4]
  time.locations <- seq(from = r.buffer, to = vbox[3] - r.buffer, length.out = length(depends[['timesteps']]$times))
  # no matter what, we need a zero-length dot at the beginning of this set of times, and at the end, so the width of the 
  # elements are always the same (for the "reveal" animation). The zero length is the `v0`, which is a vertical line of length 0
  # Z "closes" a part of a path by drawing a straight line to the previous
  # here I will give every gage the same "blocker" path, which will happen for timesteps 62 to 71
  blockers <- data.frame(stringsAsFactors = FALSE,
                         d = sprintf('M%1.2f,0v0 M%1.2f,0v%1.2f h%1.2f v-%1.2fZ M%1.2f,0v0', 
                                     r.buffer, time.locations[62], height, 
                                     diff(time.locations[c(62,71)]), height, vbox[3] - r.buffer),
                         class= 'gage-blocker')
  
  # to use this, we'd need the data.frame to have a row for each gage and be in the same order as the sparks (or make sure the ids match)
  # then we'd need to change a line in the visualize.hurricane_map code to use a do.call instead of just the `d` element here
  saveRDS(blockers, file = viz[['location']])
}