# Transforms datasets from brick to a single data table (large memory requirements) ----
source('source/evap_trend.R')
source('source/geo_functions.R')
source("database/07_dataset_fnames_evap.R")

## additional libraries ----
### parallel ----
library(doParallel)

## Data 

data_names <- list.files(PATH_SAVE_EVAP_TREND_RAW, full.names = T)
evap_2000_2019 <- lapply(data_names, brick)
names(evap_2000_2019) <- EVAP_FNAMES_SHORT_2000_2019_FULL_RECORD

## Analysis
registerDoParallel(cores = N_CORES - 1)
evap_datasets <- foreach(dataset_count = 1:n_datasets_2000_2019, .combine = rbind) %do% {
  dummy <- raster_to_dt(evap_2000_2019[[dataset_count]])
  dummy$dataset <- names(evap_2000_2019)[[dataset_count]]
  dummy
}

evap_datasets <- evap_datasets[, .(lon = lon, lat = lat, year = factor(year(date)), dataset, evap = value)]

## Save data
saveRDS(evap_datasets, paste0(PATH_SAVE_EVAP_TREND, "evap_datasets.rds"))

