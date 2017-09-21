#' function for placing add-ons to the svg base map
#' 
visualize_hurricane_map <- function(viz, mode, ...){
  library(xml2)
  
  depends <- readDepends(viz)
  checkRequired(depends, c("base-map", "watermark", "precip-colors", "precip-breaks", "sparks"))
  svg <- depends[["base-map"]]
  sparks <- depends[["sparks"]]$gage_sparks
  fl.sparks <- depends[["sparks"]]$flood_sparks

  xml_attr(svg, "id") <- viz[['id']]
  vb <- strsplit(xml_attr(svg, 'viewBox'),'[ ]')[[1]]
  side.panel <- 145
  
  # get the big dog that has all the stuff that is geo:
  map.elements <- xml2::xml_find_first(svg, "//*[local-name()='g'][@id='map-elements']") 
  
  non.geo.bot <- xml_add_sibling(map.elements, 'g', 'id' = 'non-geo-bottom', .where='before')
  xml_add_sibling(xml_children(svg)[[1]], 'desc', .where='before', viz[["alttext"]])
  xml_add_sibling(xml_children(svg)[[1]], 'title', .where='before', viz[["title"]])
  
  non.geo.top <- xml_add_sibling(map.elements, 'g', 'id' = 'non-geo-top')
  g.tool <- xml_add_sibling(non.geo.top,'g',id='tooltip-group')
  d <- xml_add_child(svg, 'defs', .where='before') 
  
  xml_add_child(non.geo.bot, 'rect', width="100%", height="100%", class='ocean-water viz-pause', id='ocean-background')
  
  
  
  # map-elements-{mode}-top is where all of the mouseovers, tips, and click events go
  map.elements.top <- xml_add_child(svg, 'g', id = "map-elements-mouser")
  
  to.mouse <- c('nwis-gages') # and sparklines, cities, etc later
  as.mouse_topper(svg, to.mouse[1], mouser.parent.id = 'map-elements-mouser')
  
  xml_add_child(g.tool, 'rect', id="tooltip-box", height="24", class="tooltip-box")
  xml_add_child(g.tool, 'path', id="tooltip-point", d="M-6,-11 l6,10 l6,-11", class="tooltip-box")
  xml_add_child(g.tool, 'text', id="tooltip-text", dy="-1.1em", 'text-anchor'="middle", class="tooltip-text-label svg-text", " ")
  
  g.watermark <- xml_add_child(non.geo.top, depends[["watermark"]])
  xml_attr(g.watermark, "transform") <- sprintf('translate(%s,%s)scale(0.20)', 
                                                as.character(as.numeric(vb[3])-110), 
                                                as.character(as.numeric(vb[4])-40))
  
  g.legend <- xml_add_child(non.geo.top, 'g', id='legend', transform=sprintf("translate(10,%s)", as.numeric(vb[4])-50))
  add_legend(g.legend, colors = depends$`precip-colors`, break.step = getContentInfo('precip-breaks')$stepSize)
  
  cp <- xml_add_child(d, 'clipPath', id="islands-clip")
  storm.islands <- xml_attr(xml_children(xml_find_first(svg, "//*[local-name()='g'][@id='storm-islands']") ), 'id')
  .jnk <- lapply(storm.islands, function(x) xml_add_child(cp, 'use', 'xlink:href'=sprintf("#%s", x)))
  
  xml_add_child(non.geo.bot, 'text', ' ', id='timestamp-text', class='time-text svg-text legend-text', 
                y=as.character(as.numeric(vb[4])-40), x = vb[3], dy = "-0.4em", dx = "-1em", 'text-anchor'='end')
  
  
  g.spark <- xml_add_child(non.geo.top, 'g', id = 'sparkline-container', transform=sprintf('translate(%s,0)', as.numeric(vb[3])-side.panel))
  xml_add_child(g.spark, 'rect', width = as.character(side.panel), height='100%', class='legend-box')
  # sparklines within container:
  g.sparkle.blck <- xml_add_child(g.spark, 'g', id = sprintf('sparkline-squiggle-block-%s', mode))
  xml_add_child(g.sparkle.blck, 'text', x=as.character(side.panel/2), 'Featured USGS gages', dy="1.5em", 'text-anchor'='middle', class='svg-text legend-text')
  xml_add_child(g.sparkle.blck, 'text', x=as.character(side.panel/2), '(normalized stage)', dy='3em', 'text-anchor'='middle', class='svg-text smallprint-text legend-text')
  g.sparkles <- xml_add_child(g.sparkle.blck, 'g', id = sprintf('sparkline-squiggles-%s', mode))
  
  ys <- seq(45, as.numeric(vb[4]) - 120, length.out = nrow(sparks))
  
  for (i in 1:nrow(sparks)){ 
    g.single <- xml_add_child(g.sparkles, 'g', transform=sprintf('translate(0,%s)', ys[i])) 
    do.call(xml_add_child, append(list(.x = g.single, .value = 'polyline'), sparks[i, ]))
    fl.spark <- fl.sparks[i,]
    cp <- xml_add_child(d, "clipPath", id=sprintf("flood-clip-%s", strsplit(fl.spark$id, '[-]')[[1]][2]))
    xml_add_child(cp, 'rect', width ='100%', height = fl.spark$y, y = "0")
    fl.spark$y <- NULL
    fl.spark$points <- sparks[i, ]$points
    do.call(xml_add_child, append(list(.x = g.single, .value = 'polyline'), fl.spark))
    # now add flood spark
  }
  
  m = xml_add_child(d, 'mask', id="spark-opacity", x="0", y="-1", width="1", height="3", maskContentUnits="objectBoundingBox")
  xml_add_child(m, 'rect', x="0", y="-1", width="1", height="3", style="fill-opacity: 0.18; fill: white;", id='spark-light-mask')
  xml_add_child(m, 'rect', x="0", y="-1", width="0", height="3", style="fill-opacity: 1; fill: white;", id='spark-full-mask')
  
  xml_add_child(g.sparkle.blck, 'text', ' ', id='timestamp-text', class='time-text svg-text legend-text', 
                y=as.character(ys[i]+50), x = as.character(side.panel/2), 'text-anchor'='middle')
  
  
  return(svg)
}

#' remove mouser events from style geometries, and add them to a new invisible mouser group overlay
as.mouse_topper <- function(svg, style.group.id, mouser.parent.id){
  
  style.kids <- xml_children(
    xml_find_first(
      svg, sprintf("//*[local-name()='g'][@id='%s']", style.group.id)
      )
    )
  
  parent.element <- xml_find_first(
    svg, sprintf("//*[local-name()='g'][@id='%s']", mouser.parent.id)
  )
  g.mouser <- xml_add_child(parent.element, 'g', id = paste0(style.group.id, '-mousers'))
  
  
  transfers <- c('onmousemove', 'onmouseout', 'onclick')
  for (style.kid in style.kids){
    mouse.kid <- xml_add_child(g.mouser, 'use', 'xlink:href'=sprintf("#%s", xml_attr(style.kid, 'id')), class = 'mouser')
    .jnk <- lapply(X = transfers, FUN = function(x) {
        xml_attr(mouse.kid, x) <- xml_attr(style.kid, x)
        xml_attr(style.kid, x) <- NULL
      }
      )
  }
  # does it work to let the reference element keep its class? no
  
}

visualize.hurricane_map_portrait <- function(viz = as.viz('hurricane-map-mobile')){

  svg <- visualize_hurricane_map(viz, mode =  'portrait')
  
  write_xml(svg, file = viz[['location']])
  
}

visualize.hurricane_map_landscape <- function(viz = as.viz('hurricane-map-landscape')){
  svg <- visualize_hurricane_map(viz, mode =  'landscape')
  
  write_xml(svg, file = viz[['location']])
}

add_legend <- function(parent.ele, colors, break.step){
  
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
  n.bins <- length(colors)
  col.breaks <- seq(0, length.out = n.bins, by = break.step)
  col.rng <- paste(head(col.breaks, -1L), tail(col.breaks, -1L), sep='-')
  col.txt <- c(col.rng, sprintf('%s+', tail(col.breaks, 1)))
  
  for (i in 1:n.bins){
    
    xml_add_child(g.rains.bn, 'rect', x=as.character(x0), y="-10", 
                  height = as.character(rain.h), width = as.character(rain.w), 
                  class='rain-box', style=sprintf("fill:%s;",colors[i]))
    if (i == 1 | i == n.bins){
      # only do the extreme values
      text.class <- ifelse(any(col2rgb(colors[i]) < 100), 'svg-text light-rain-legend', 'svg-text dark-rain-legend')
      xml_add_child(g.rains.tx, 'text', col.txt[i], class = text.class, x= as.character(x0+rain.w/2), 'text-anchor'='middle')  
    }
    x0 <- x0+rain.w
  }
  
}
