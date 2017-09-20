# Download all available shapefiles in our date range
fetch.precip_shapefiles <- function(viz = as.viz('precip_shapefiles')){
  
  precip_page_urls <- get_precip_page_urls(viz)
  
  precip_dir <- viz[["location"]]
  if(!dir.exists(precip_dir)) dir.create(precip_dir)
  
  for(precip_page_url in precip_page_urls) {
    
    precip_files <- get_precip_files_on_page(precip_page_url)

    for(dFile in precip_files) {
      outfile <- file.path(precip_dir, dFile)
      precip.tars <- c(precip.tars, outfile)
      if(!file.exists(outfile)) {
        download.file(paste0(precip_page_url, dFile), outfile)
      }
    }
  }
  # remove files that shouldn't be there
  files.now <- dir(precip_dir)
  rmv.i <- !(files.now %in% basename(precip.tars))
  unlink(file.path(precip_dir, files.now[rmv.i]))
  
  # update the timstamp of the last outfile to make sure the directory timestamp
  # is always updated by now, even if there were no new files to download
  Sys.setFileTime(precip_dir, Sys.time())
}

# Monitor the timestamp of the last-available precip file in our date range
fetchTimestamp.precip_shapefiles <- function(viz) {
  
  # find the latest page of precip data
  precip_page_urls <- get_precip_page_urls(viz)
  last_precip_page <- precip_page_urls[length(precip_page_urls)]
  
  # find the last zip file on that last page
  precip_files <- get_precip_files_on_page(last_precip_page)
  last_precip_file <- sort(precip_files, decreasing=TRUE)[1]
  
  # construct the url and pass to urlLastModified, which will create/modify the
  # timestamp file as needed
  url <- paste0(last_precip_page, last_precip_file)
  fetcher <- urlFetcher(id=viz[['id']], location=NA, remoteURL=url)
  fetchTimestamp(fetcher)
}

# helper: get all precip files noted on a single page
get_precip_files_on_page <- function(precip_page_url) {
  precip_files <- rvest::html_attr(
    rvest::html_nodes(
      xml2::read_html(precip_page_url), 
      "a"), 
    "href")
  
  precip_files <- grep("tar.gz", precip_files, value=TRUE)
  
  return(precip_files)
}

# helper: get all precip page urls relevant to our date range
get_precip_page_urls <- function(viz = as.viz('precip_shapefiles')) {
  seDates <- readDepends(viz)[["dates"]]
  startDate <- as.Date(seDates$startDate)
  endDate <- as.Date(seDates$endDate)
  dates <- seq(startDate, min(endDate, as.Date(Sys.time())), by="days")
  
  precip_page_url_base <- viz[["url_base"]]
  precip_page_urls <- paste0(precip_page_url_base, 
                             as.character(dates, format="%Y%m%d"), "/")
  
  return(precip_page_urls)
}
