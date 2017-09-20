grab_spark <- function(vals){
  
  xs <- seq_len(length(vals))
  xs <- c(head(xs, 1), xs, tail(xs, 1))
  ys <- c(min(vals), vals, min(vals))
  x = svglite::xmlSVG({
    par(omi=c(0,0,0,0), mai=c(0,0,0,0))
    plot(xs, ys, type='l', axes=F, ann=F)
  }, height=0.4, width=2)
  xml2::xml_attr(xml2::xml_find_first(x, '//*[local-name()="polyline"]'),'points')
}


# Should rename at some point. Not always going to be discharge.
process.discharge_sparks <- function(viz = as.viz('discharge-sparks')){
  library(dplyr)
  depends <- readDepends(viz)
  checkRequired(depends, c("storm-gage-height", "nwis-sites"))
  
  sites <- depends[["nwis-sites"]]
  gage_data <- depends[["storm-gage-height"]]
  
  site.nos <- names(gage_data)
  
  sparks <- data.frame(points = sapply(gage_data, function(x) grab_spark(x$y), USE.NAMES = FALSE),
                       site_no = site.nos, stringsAsFactors = FALSE) %>% 
    mutate(class = "sparkline", 
           id = sprintf("sparkline-%s", site_no), 
           style = "mask: url(#spark-opacity);",
           onmouseover=sprintf("setBold('nwis-%s');", site_no), 
           onmouseout=sprintf("setNormal('nwis-%s');hovertext(' ');", site_no),
           onclick=sprintf("openNWIS('%s', evt);", site_no),
           onmousemove=sprintf("hovertext('USGS %s',evt);", site_no))
  
  sites <- sites[c("site_no", "dec_lat_va")]
  
  sparks <- sparks %>% 
    left_join(sites, by = "site_no") %>%
    arrange(desc(dec_lat_va)) %>%
    select(-site_no, -dec_lat_va) 
  
  sparks <- unique(sparks)
  
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
  checkRequired(depends, c("storm-gage-height", "nwis-sites", "nws-data"))
  
  sites <- depends[["nwis-sites"]]
  nws_data <- depends[["nws-data"]]
  gage_data <- depends[["storm-gage-height"]]
  site.nos <- names(depends[["storm-gage-height"]])

  sparks <- data.frame(site_no = site.nos, 
                       y = NA, stringsAsFactors = FALSE)
  
  for (row in 1:nrow(sparks)){
    site <- sparks$site_no[row]
    flood <- filter(nws_data, site_no == site) %>% .$flood.stage %>% .[1]
    sparks$y[row] = grab_clip_rect(gage_data[[site]]$y, flood)
  }
  
  sparks <- sparks %>% 
    mutate(class = "floodline", 
           id = sprintf("floodline-%s", site_no), 
           style = "mask: url(#flood-opacity);",
           "clip-path"=sprintf("url(#flood-clip-%s)", site_no), 
           onmouseover=sprintf("setBold('nwis-%s');setBold('sparkline-%s');", site_no, site_no),
           onmouseout=sprintf("setNormal('nwis-%s');setNormal('sparkline-%s');hovertext(' ');", site_no, site_no),
           onclick=sprintf("openNWIS('%s', evt);", site_no),
           onmousemove=sprintf("hovertext('USGS %s',evt);", site_no))
  
  sites <- sites[,c("site_no", "dec_lat_va")]
  
  sparks <- sparks %>% 
    left_join(sites, by = "site_no") %>%
    arrange(desc(dec_lat_va)) %>%
    select(-site_no, -dec_lat_va) 
  
  sparks <- unique(sparks)
  
  saveRDS(sparks, viz[['location']])
}