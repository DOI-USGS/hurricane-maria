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
  
  # --- this is all temporary so we can see it w/o publish and css in place: -----
  xml_attr(map.elements, 'style') <- "fill: green;"
  xml_attr(non.geo.bot, 'style') <- "fill: blue;"
  # --- this is all temporary so we can see it w/o publish and css in place: -----
  
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
