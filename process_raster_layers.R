library(raster)
library(rgdal)
library(stringr)

file_list = list.files('~/data/habitat_metrics', full.names = T)
file_list=file_list[!grepl('xml', file_list)]
study_area=readOGR('./gis', 'study_area')


stacked = stack(file_list)

stacked = crop(stacked, study_area)
stacked = stack(stacked)

#Convert teh layer name to just the first word of the original filename
#ie. from cv_01_05_1km_uint16 to cv
for(layer_num in 1:length(stacked@layers)){
  layer_name = stacked[[layer_num]]@data@names
  layer_name = stringr::word(layer_name, 1, 1, sep='_')
  stacked[[layer_num]]@data@names = layer_name
  
}

writeRaster(stacked, './data/habitat_layers', format = 'GTiff')

#Make the urban landcover a seperate file
urban_full = raster('~/data/consensus_landcover/consensus_full_class_9.tif')

urban_cropped = crop(urban_full, study_area)

writeRaster(urban_cropped, './data/urban_cover', format = 'GTiff')
