fetchTimestamp.precip_shapefiles <- vizlab:::fetchTimestamp.file

fetch.precip_shapefiles <- function(viz = as.viz('precip_shapefiles')){
  
  # if(!viz[["refetch"]]) {
  #   return(NULL)
  # }
  
  seDates <- readDepends(viz)[["dates"]]
  startDate <- as.Date(seDates$startDate)
  endDate <- as.Date(seDates$endDate)
  dates <- seq(startDate, endDate, by="days")
  
  precip_page_url_base <- viz[["url_base"]]
  precip_page_urls <- paste0(precip_page_url_base, as.character(dates, format="%Y%m%d"), "/")
  
  tempdir <- viz[["location"]]
  
  for(precip_page_url in precip_page_urls) {
    
    precip_files <- rvest::html_attr(
                      rvest::html_nodes(
                        xml2::read_html(precip_page_url), 
                        "a"), 
                      "href")
    
    precip_files <- precip_files[which(grepl("tar.gz", precip_files))]

    for(dFile in precip_files) {
      outfile <- file.path(tempdir, dFile)
      if(!file.exists(outfile)) download.file(paste0(precip_page_url, dFile), 
                                              outfile)
    }
  }
}