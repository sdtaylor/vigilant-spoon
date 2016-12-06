library(tidyverse)
library(lubridate)
library(raster)
library(sp)

raw_data = read_delim('~/data/ebird/ebird_study_area.csv', delim = ',') %>%
  mutate(date = as.Date(paste(year,month,day,sep='-')))

quail = 'Callipepla gambelii'

#Find absences.
#First group observations by the same observer on the same day at the same place. If that session had > 2
#recorded species, assumed they recorded *all* species they saw. If it doesn't include gambel's quail, thats an absence
absences = raw_data %>%
  group_by(date, recordedBy, locality, decimalLatitude, decimalLongitude) %>%
  filter(!quail %in% scientificName, n() > 2) %>%
  summarize(total_spp = n()) %>%
  ungroup() %>%
  dplyr::select(date, decimalLatitude, decimalLongitude)

absences$count=0

presences= raw_data %>%
  filter(scientificName == quail, !is.na(individualCount)) %>%
  dplyr::select(date, decimalLatitude, decimalLongitude, count=individualCount)


quail_data = presences %>%
  bind_rows(absences)

quail_data$presence = ifelse(quail_data$count>0, 1, 0)

quail_data_spatial = SpatialPointsDataFrame(cbind(quail_data$decimalLongitude, quail_data$decimalLatitude), data=as.data.frame(quail_data), 
                                             proj4string = CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))

rm(absences, presences)

#################################################################################
#Remove points that are taken in urban areas
urban_cover = raster::raster('./data/urban_cover.tif')

quail_data$urban_cover = raster::extract(urban_cover, quail_data_spatial)

quail_data = quail_data %>%
  filter(urban_cover < 20)

quail_data_spatial = SpatialPointsDataFrame(cbind(quail_data$decimalLongitude, quail_data$decimalLatitude), data=as.data.frame(quail_data), 
                                            proj4string = CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))

#################################################################################
habitat_metrics = raster::stack('./data/habitat_layers.tif')

model_data = as.data.frame(raster::extract(habitat_metrics, quail_data_spatial))
model_data$presence = quail_data$presence
model_data$count = quail_data$count

############################################
run_cv = T

pa_formula = as.formula(presence ~ habitat_layers.1 + habitat_layers.2 + habitat_layers.3 + habitat_layers.4 + habitat_layers.5 + habitat_layers.6 +
                          habitat_layers.7 + habitat_layers.8 + habitat_layers.9 + habitat_layers.10)

abund_formula = as.formula(count ~ habitat_layers.1 + habitat_layers.2 + habitat_layers.3 + habitat_layers.4 + habitat_layers.5 + habitat_layers.6 +
                          habitat_layers.7 + habitat_layers.8 + habitat_layers.9 + habitat_layers.10)
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
}


pa_model_data = as.data.frame(raster::extract(habitat_metrics, occurances_spatial))
pa_model_data$presence = occurances$presence

pa_model = glm(presence ~ ., family = 'binomial', data = pa_model_data)

abund_model_data = as.data.frame(raster::extract(habitat_metrics, abundance_spatial))
abund_model_data$individualCount = abundance$individualCount

abund_model = glm(individualCount ~ ., family = 'poisson', data = abund_model_data)

##################################################################################








