visualize.monitor_gages <- function(viz=as.viz('monitor_gages')) {
  library(ggplot2)
  gage <- readDepends(viz)[['gage-data']]
  g <- ggplot(gage, aes(x=dateTime, y=p_Inst, color=p_Inst_cd)) +
    geom_line() + 
    scale_x_datetime(date_breaks = '24 hours', date_labels="%m/%d") +
    facet_wrap(~ site_no, scales='free_y') + 
    theme_classic()
  ggsave(filename=viz$location, plot=g, width=12, height=8)
}
