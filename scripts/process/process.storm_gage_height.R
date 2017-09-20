process.storm_gage_height <- function(viz){
  # resample the gage data down to the same steps as timesteps
  deps <- readDepends(viz)
  saveRDS(deps[['gage-height']], file = viz[['location']])
}