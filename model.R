library(tidyverse)
library(lubridate)
library(raster)
library(sp)
library(gbm)

raw_data = read_delim('~/data/ebird/ebird_study_area.csv', delim = ',') %>%
  mutate(date = as.Date(paste(year,month,day,sep='-')))

quail = 'Callipepla gambelii'

#Find absences.
#First group observations by the same observer on the same day at the same place. If that session had > 2
#recorded species, assumed they recorded *all* species they saw. If it doesn't include gambel's quail, thats an absence
#absences = raw_data %>%
#  group_by(date, recordedBy, locality, decimalLatitude, decimalLongitude) %>%
#  filter(!quail %in% scientificName, n() > 2) %>%
#  summarize(total_spp = n()) %>%
#  ungroup() %>%
#  dplyr::select(date, decimalLatitude, decimalLongitude)

#absences$count=0

presences= raw_data %>%
  filter(scientificName == quail, !is.na(individualCount)) %>%
  dplyr::select(date, decimalLatitude, decimalLongitude, count=individualCount)

#Add in pseudo randomly located absences
max_lat = max(presences$decimalLatitude)
min_lat = min(presences$decimalLatitude)
max_lon = max(presences$decimalLongitude)
min_lon = min(presences$decimalLongitude)

num_absences = 5*nrow(presences)

absences = data.frame(decimalLongitude = runif(num_absences, min_lon, max_lon),
                      decimalLatitude  = runif(num_absences, min_lat, max_lat),
                      count =0)


quail_data = presences %>%
  bind_rows(absences)

quail_data$presence = ifelse(quail_data$count>0, 1, 0)

quail_data_spatial = SpatialPointsDataFrame(cbind(quail_data$decimalLongitude, quail_data$decimalLatitude), data=as.data.frame(quail_data), 
                                             proj4string = CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))

rm(absences, presences, max_lat, min_lat, max_lon, min_lon)

#################################################################################
#Remove points that are taken in urban areas
#urban_cover = raster::raster('./data/urban_cover.tif')

#quail_data$urban_cover = raster::extract(urban_cover, quail_data_spatial)

#quail_data = quail_data %>%
  filter(urban_cover < 5)

#quail_data_spatial = SpatialPointsDataFrame(cbind(quail_data$decimalLongitude, quail_data$decimalLatitude), data=as.data.frame(quail_data), 
#                                            proj4string = CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))

#################################################################################
raster_file_list = list.files('./data', full.names = T)
raster_file_list=raster_file_list[!grepl('xml', raster_file_list)]

habitat_metrics = raster::stack(raster_file_list)

model_data = as.data.frame(raster::extract(habitat_metrics, quail_data_spatial))
model_data$presence = quail_data$presence
model_data$count = quail_data$count

model_data = model_data %>%
  filter(landcover_Urban < 5)

############################################
run_cv = F

pa_formula = as.formula(presence ~ habitat_cv + habitat_Entropy + habitat_evenness + habitat_Homogeneity + habitat_Maximum + habitat_range + 
                          habitat_shannon + habitat_simpson + habitat_std + habitat_Uniformity + landcover_Barren + landcover_Conifer + 
                          landcover_Cultivated + landcover_Decid_Broadleaf + landcover_Evergreen_Broadleaf + landcover_Herbaceous + landcover_Mixed_Trees + 
                          landcover_Shrubs + landcover_Snow + landcover_Water + landcover_Wetlands )

abund_formula = as.formula(count ~ habitat_cv + habitat_Entropy + habitat_evenness + habitat_Homogeneity + habitat_Maximum + habitat_range + 
                          habitat_shannon + habitat_simpson + habitat_std + habitat_Uniformity + landcover_Barren + landcover_Conifer + 
                          landcover_Cultivated + landcover_Decid_Broadleaf + landcover_Evergreen_Broadleaf + landcover_Herbaceous + landcover_Mixed_Trees + 
                          landcover_Shrubs + landcover_Snow + landcover_Water + landcover_Wetlands )

if(run_cv){
  test_size = 0.2
  n = nrow(model_data)
  test_set = sample(1:n, round(n*test_size), replace = F)
  
  pa_model = glm(pa_formula, family = 'binomial', data = model_data[-test_set,])
  #pa_model = gbm(pa_formula, distribution = 'bernoulli', n.trees = 2000, data = model_data[-test_set,])
  
  abund_model = glm(abund_formula, family = 'poisson', data = model_data[-test_set,])
  #abund_model = gbm(abund_formula, distribution = 'poisson', n.trees = 2000, data = model_data[-test_set,])
  
  test_prediction = data.frame(presence = model_data$presence[test_set])
  test_prediction$abundance = model_data$count[test_set]
  
  test_prediction$presence_predict = predict(pa_model, newdata = model_data[test_set,], type = 'response')
  test_prediction$abund_predict    = predict(abund_model, newdata = model_data[test_set,], type = 'response')
  
  test_prediction = filter(test_prediction, !is.na(abund_predict))
  
  print('AUC of test set')
  print(Metrics::auc(test_prediction$presence, test_prediction$presence_predict))
  
  Metrics::mse(test_prediction$abundance, test_prediction$abund_predict)
} else {

  pa_model = glm(pa_formula, family = 'binomial', data = model_data)
  pa_model = gbm(pa_formula, distribution = 'bernoulli', n.trees = 2000, data = model_data)
  
  abund_model = gbm(abund_formula, distribution = 'poisson', n.trees = 2000, data = model_data, n.cores=2)
  
  pa_predict = raster::predict(habitat_metrics, pa_model, type='response')
  abund_predict = raster::predict(habitat_metrics, abund_model, type='response', n.trees=2000)

  writeRaster(pa_predict, './results/presence', format = 'GTiff')
  writeRaster(abund_predict, './results/abundance', format = 'GTiff')
  
}

##################################################################################








