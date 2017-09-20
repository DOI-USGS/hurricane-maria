grab_spark <- function(vals){
  
  xs <- seq_len(length(vals))
  xs <- c(head(xs, 1), xs, tail(xs, 1))
  ys <- c(min(vals, na.rm = T), vals, min(vals, na.rm = T))
  x = svglite::xmlSVG({
    par(omi=c(0,0,0,0), mai=c(0,0,0,0))
    plot(xs, ys, type='l', axes=F, ann=F)
  }, height=0.4, width=2)
  xml2::xml_attr(xml2::xml_find_first(x, '//*[local-name()="polyline"]'),'points')
}

process.gage_sparks <- function(viz = as.viz('gage-sparks')){
  library(dplyr)
  depends <- readDepends(viz)
  checkRequired(depends, c("gage-data", "sites"))
  
  sites <- depends[["sites"]]
  gage_data <- depends[["gage-data"]]
  
  site.nos <- sites$site_no[which(sites$site_no %in% names(gage_data))]

  sparks <- data.frame(points = sapply(site.nos, function(x) grab_spark(gage_data[[x]]), USE.NAMES = FALSE),
                       site_no = site.nos, stringsAsFactors = FALSE) %>% 
    mutate(class = "sparkline", 
           id = sprintf("sparkline-%s", site_no), 
           style = "mask: url(#spark-opacity);",
           onmouseover=sprintf("setBold('nwis-%s');", site_no), 
           onmouseout=sprintf("setNormal('nwis-%s');hovertext(' ');", site_no),
           onclick=sprintf("openNWIS('%s', evt);", site_no)) %>%
    left_join(select(sites, site_no, station_nm), by="site_no") %>%
    mutate(onmousemove=sprintf("hovertext(%s',evt);", station_nm))

  saveRDS(sparks, viz[['location']])
}

grab_clip_rect <- function(vals, flood){
  
  x = svglite::xmlSVG({
    par(omi=c(0,0,0,0), mai=c(0,0,0,0))
    plot(vals, type='l', axes=F, ann=F)
    abline(h = flood)
  }, height=0.4, width=2)
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
  
  site.nos <- sites$site_no[which(sites$site_no %in% names(gage_data))]

  sparks <- data.frame(y = sapply(site.nos, function(x) grab_clip_rect(gage_data[[x]], 
                                                                            nws_data$flood.stage[nws_data$site_no == x]), 
                                       USE.NAMES = FALSE),
                       site_no = site.nos, stringsAsFactors = FALSE) 
  
  sparks <- sparks %>% 
    mutate(class = "floodline", 
           id = sprintf("floodline-%s", site_no), 
           style = "mask: url(#flood-opacity);",
           "clip-path"=sprintf("url(#flood-clip-%s)", site_no), 
           onmouseover=sprintf("setBold('nwis-%s');setBold('sparkline-%s');", site_no, site_no),
           onmouseout=sprintf("setNormal('nwis-%s');setNormal('sparkline-%s');hovertext(' ');", site_no, site_no),
           onclick=sprintf("openNWIS('%s', evt);", site_no)) %>%
    left_join(select(sites, site_no, station_nm), by="site_no") %>%
    mutate(onmousemove=sprintf("hovertext(%s',evt);", station_nm))
           
  saveRDS(sparks, viz[['location']])
}