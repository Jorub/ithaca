# Transforms datasets from brick to a single data table (large memory requirements)

source('source/partition_evap.R')
source('source/geo_functions.R')

## additional libraries ----
### parallel ----
library(doParallel)

## Data 
load("~/shared/data_projects/ithaca/misc/evap_fnames_2000_2019_full_record.Rdata")
evap_2000_2019 <- lapply(EVAP_FNAMES_2000_2019, brick)
names(evap_2000_2019) <- EVAP_FNAMES_SHORT_2000_2019 

## Analysis
registerDoParallel(cores = N_CORES - 1)
evap_datasets <- foreach(dataset_count = 1:n_datasets_2000_2019, .combine = rbind) %do% {
  dummy <- raster_to_dt(evap_2000_2019[[dataset_count]])
  dummy$dataset <- names(evap_2000_2019)[[dataset_count]]
  dummy
}

evap_datasets <- evap_datasets[, .(lon = lon, lat = lat, year = factor(year(date)), dataset, evap = value)]

## Save data
saveRDS(evap_datasets, paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets.rds"))
