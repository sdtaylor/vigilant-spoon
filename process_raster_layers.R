library(raster)
library(rgdal)
library(stringr)

##############################################
#Habitat heterogeniety metrics http://www.earthenv.org/texture
file_list = list.files('~/data/habitat_metrics', full.names = T)
file_list=file_list[!grepl('xml', file_list)]
study_area=readOGR('./gis', 'study_area')


stacked = stack(file_list)

stacked = crop(stacked, study_area)
stacked = stack(stacked)

layer_names=c()
#Convert the layer name to just the first word of the original filename
#ie. from cv_01_05_1km_uint16 to cv
for(layer_num in 1:length(stacked@layers)){
  layer_names = c(layer_names, stacked[[layer_num]]@data@names)
  layer_names[layer_num] = stringr::word(layer_names[layer_num], 1, 1, sep='_')
  stacked[[layer_num]]@data@names = layer_names[layer_num]
  
}

writeRaster(stacked, './data/habitat', format = 'GTiff', bylayer=T, suffix=layer_names)

##################################################
#Make the urban landcover a seperate file
urban_full = raster('~/data/consensus_landcover/consensus_full_class_9.tif')

urban_cropped = crop(urban_full, study_area)

writeRaster(urban_cropped, './data/urban_cover', format = 'GTiff')

##############################################
#landcover http://www.earthenv.org/landcover
file_list = list.files('~/data/consensus_landcover/', full.names = T)
file_list=file_list[!grepl('xml', file_list)]
study_area=readOGR('./gis', 'study_area')

actual_layer_names = c('class_1'='Conifer', 'class_2'='Evergreen_Broadleaf', 'class_3'='Decid_Broadleaf', 'class_4'='Mixed_Trees',
                       'class_5'='Shrubs', 'class_6'='Herbaceous', 'class_7'='Cultivated', 'class_8'='Wetlands', 'class_9'='Urban',
                       'class_10'='Snow', 'class_11'='Barren', 'class_12'='Water')

stacked = stack(file_list)

stacked = crop(stacked, study_area)
stacked = stack(stacked)

layer_names=c()
#Convert teh layer name to just the first word of the original filename
#ie. from cv_01_05_1km_uint16 to cv
for(layer_num in 1:length(stacked@layers)){
  layer_names = c(layer_names, stacked[[layer_num]]@data@names)
  layer_names[layer_num] = stringr::word(layer_names[layer_num], 3, 4, sep='_')
  stacked[[layer_num]]@data@names = layer_names[layer_num]
  
}

#Replace class_x with above actual names. Probably a better way to do this. 
for(i in 1:length(layer_names)){
  layer_names[i] = as.character(actual_layer_names[layer_names[i]])
}

writeRaster(stacked, './data/landcover', format = 'GTiff', bylayer=T, suffix=layer_names)
