#' function for placing add-ons to the svg base map
#' 
visualize_hurricane_map <- function(viz, height, width, mode, ...){
  library(xml2)
  
  depends <- readDepends(viz)
  checkRequired(depends, c("base-map", "watermark"))
  svg <- depends[["base-map"]]

  xml_attr(svg, "id") <- viz[['id']]
  vb <- strsplit(xml_attr(svg, 'viewBox'),'[ ]')[[1]]
  
  # get the big dog that has all the stuff that is geo:
  map.elements <- xml2::xml_find_first(svg, "//*[local-name()='g'][@id='map-elements']") 
  
  
  non.geo.bot <- xml_add_sibling(map.elements, 'g', 'id' = 'non-geo-bottom', .where='before')
  xml_add_sibling(xml_children(svg)[[1]], 'desc', .where='before', viz[["alttext"]])
  xml_add_sibling(xml_children(svg)[[1]], 'title', .where='before', viz[["title"]])
  non.geo.top <- xml_add_sibling(map.elements, 'g', 'id' = 'non-geo-top')
  
  xml_add_child(non.geo.bot, 'rect', width="100%", height="100%", class='ocean-water viz-pause', id='ocean-background')
  
  g.tool <- xml_add_child(svg,'g',id='tooltip-group')
  map.elements.top <- xml_add_child(svg, 'g', id=sprintf('map-elements-%s-top', mode))
  
  xml_add_child(g.tool, 'rect', id="tooltip-box", height="24", class="tooltip-box")
  xml_add_child(g.tool, 'path', id="tooltip-point", d="M-6,-11 l6,10 l6,-11", class="tooltip-box")
  xml_add_child(g.tool, 'text', id="tooltip-text", dy="-1.1em", 'text-anchor'="middle", class="tooltip-text-label svg-text", " ")
  
  g.watermark <- xml_add_child(non.geo.top, depends[["watermark"]])
  xml_attr(g.watermark, "transform") <- sprintf('translate(%s,%s)scale(0.20)', 
                                                as.character(as.numeric(vb[3])-110), 
                                                as.character(as.numeric(vb[4])-40))
  
  g.legend <- xml_add_child(non.geo.top, 'g', id='legend', transform=sprintf("translate(10,%s)", as.numeric(vb[4])-50))
  warning('need to extract color and bin info from other targets')
  add_legend(g.legend, n.bins = 9, color.name = "Blues")
  
  
  return(svg)
}


visualize.hurricane_map_portrait <- function(viz = as.viz('hurricane-map-portrait')){
  height <- viz[['height']]
  width <- viz[['width']]
  svg <- visualize_hurricane_map(viz, height = height, width = width, mode =  'portrait')
  
  write_xml(svg, file = viz[['location']])
  
}

visualize.hurricane_map_landscape <- function(viz = as.viz('hurricane-map-landscape')){
  height <- viz[['height']]
  width <- viz[['width']]
  svg <- visualize_hurricane_map(viz, height = height, width = width, mode =  'landscape')
  
  write_xml(svg, file = viz[['location']])
}

add_legend <- function(parent.ele, n.bins, color.name){
  
  # lower left legend:
  xml_add_child(parent.ele, 'text', "Total rainfall amount (inches)", class='svg-text legend-text', dy="-1em",
                transform="translate(0,35)")
  g.rains <- xml_add_child(parent.ele, 'g', id = 'rain-legend')
  g.irma <- xml_add_child(parent.ele, 'g', id = 'irma-legend', transform="translate(15,-65)")
  g.gage_isFlood <- xml_add_child(parent.ele, 'g', id = 'gage-legend', transform="translate(15,-10)")
  g.gage_predFlood <- xml_add_child(parent.ele, 'g', id = 'gage-legend', transform="translate(15,-25)")
  g.rains.bn <- xml_add_child(g.rains, 'g', id = 'rain-legend-bin', transform="translate(0,35)")
  g.rains.tx <- xml_add_child(g.rains, 'g', id = 'rain-legend-text', transform="translate(0,35)")
  xml_add_child(g.irma, 'circle', r="8", class="storm-dot-legend")
  xml_add_child(g.gage_isFlood, 'circle', r="4", class="nwis-flooding-legend")
  xml_add_child(g.irma, 'text', "Hurricane Maria", class='svg-text legend-text', dx='20', dy="0.33em")
  xml_add_child(g.gage_isFlood, 'text', "Above flood stage", class='svg-text legend-text', dx='20', dy="0.33em")
  xml_add_child(g.gage_predFlood, 'circle', r="4", class="nwis-dot-legend")
  xml_add_child(g.gage_predFlood, 'text', "Below flood stage", class='svg-text legend-text', dx='20', dy="0.33em")
  
  g.main_gage_text <- xml_add_child(parent.ele, 'text', "USGS Stream Gages", class='svg-text legend-text', dy="-1em",
                                    transform="translate(0,-23)")
  g.main_gage_text <- xml_add_child(parent.ele, 'text', "(< 1% of US total)", class='svg-text smallprint-text legend-text', dy="-1.33em",
                                    transform="translate(135,-23)")
  
  
  rain.w <- 28 # width of a rain legend bin
  rain.h <- 14
  x0 <- 0
  
  cols <- RColorBrewer::brewer.pal(n.bins, color.name)
  for (i in 1:n.bins){
    xml_add_child(g.rains.bn, 'rect', x=as.character(x0), y="-10", 
                  height = as.character(rain.h), width = as.character(rain.w), 
                  class='rain-box', style=sprintf("fill:%s;",cols[i]))
    x0 <- x0+rain.w
  }
  
}
