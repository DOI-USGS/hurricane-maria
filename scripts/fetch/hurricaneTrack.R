#' Pull the shapefile(s) for a hurricane and writes the file(s) to disk
fetch.hurricaneTrack <- function(viz=as.viz('hurricane-track')) {
  # create the folder and use the built-in url fetcher to download the item
  setupFoldersForFile(file = viz[['location']])
  fetchviz <- getHurricaneTrackURLfetcher(viz)
  fetch(fetchviz)
}

fetchTimestamp.hurricaneTrack <- function(viz=as.viz('hurricane-track')) {
  fetchviz <- getHurricaneTrackURLfetcher(viz)
  fetchTimestamp(fetchviz)
}

#' viz fields:
#' ocean: al or ep
#' stormnum: 01-n
#' year: 2017
getHurricaneTrackURLfetcher <- function(viz=as.viz('hurricane-track')) {
  library(httr)
  library(magrittr)
  required <- c("location", "ocean", "stormnum", "year")
  checkRequired(viz, required)
  
  nhc.url <- "http://www.nhc.noaa.gov/gis/best_track/%s%s%s_best_track.zip"
  download.url <- sprintf(nhc.url, viz[['ocean']], viz[['stormnum']], viz[['year']])
  
  urlFetcher(id=viz[['id']], location=viz[['location']], remoteURL=download.url)
}
