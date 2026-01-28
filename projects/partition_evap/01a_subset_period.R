# Reads and subsets data as netcdf files for the specified period.
source('source/main.R')
source('source/geo_functions.R')
source('projects/partition_evap/00_initialize.R')

## additional libraries ----
### parallel ----
library(doParallel)

load("~/shared/data_projects/ithaca/misc/evap_fnames_2000_2019_full_record.Rdata") # Created by database/07_dataset_fnames_evap.R

registerDoParallel(cores = N_CORES - 1)
n_datasets_2000_2019 <- length(EVAP_FNAMES_2000_2019_FULL_RECORD)
period_start <- as.Date("2000-01-01") 
period_end <- ITHACA_PERIOD_END

foreach(dataset_count = 1:n_datasets_2000_2019) %dopar% {
  result <- crop_time(brick(EVAP_FNAMES_2000_2019_FULL_RECORD[[dataset_count]]),
                            period_start, period_end)
  nc_out <- paste0(PATH_SAVE_PARTITION_EVAP_RAW,
                   EVAP_FNAMES_SHORT_2000_2019_FULL_RECORD[[dataset_count]],
                   "_e_mm_land_200001_201912_025_yearly.nc")
  save_nc(result, nc_out, name_def = "e", longname_def = "Total evaporation")
} 

EVAP_FNAMES_2000_2019 <- list.files(path = PATH_SAVE_PARTITION_EVAP_RAW, full.names = TRUE)
dummy <- strsplit(EVAP_FNAMES_2000_2019, split = '//')
dummy <- sapply(dummy, "[[", 2)
dummy <- strsplit(dummy, split = '_')
EVAP_FNAMES_SHORT_2000_2019 <- sapply(dummy, "[[", 1)

save(EVAP_FNAMES_2000_2019, EVAP_FNAMES_SHORT_2000_2019, file = paste0(PATH_SAVE_PARTITION_EVAP, "evap_names_2000_2019.Rdata"))
