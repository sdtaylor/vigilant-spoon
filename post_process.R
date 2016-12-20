library(tidyverse)
library(raster)
library(rgdal)

#Do some smoothing on the final raster. At different window sizes to try in qgis
quail_map = raster::raster('./results/pa_and_abund.tif')

smoothing_windows = list(matrix(1,5,5),
                      matrix(1,11,11),
                      matrix(1,21,21))

window_names = c('5x5','10x10','20x20')

for(this_window in seq_along(smoothing_windows)){
  window_name = window_names[this_window]
  
  smoothed = raster::focal(quail_map, w=smoothing_windows[[this_window]], mean)
  
  filename = paste0('./results/quail_map_',window_name)
  writeRaster(smoothed, filename, format = 'GTiff')
  
}
