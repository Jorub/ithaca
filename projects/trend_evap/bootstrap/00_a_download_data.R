# Download data from Zenodo and save as RDS ----
source('source/evap_trend.R')
source('source/geo_functions.R')
source("database/07_dataset_fnames_evap.R")

## additional libraries ----
### parallel ----
library(doParallel)

download_data <- function(name, folder_path = PATH_SAVE_EVAP_TREND_RAW){
  zenodo_base <- "https://zenodo.org/records/18150992/files/"
  file_name <- paste0(name,"_e_mm_land_200001_201912_025_yearly.nc")
  zenodo_end <- "?download=1"
  file_url <- paste0(zenodo_base, file_name, zenodo_end)
  file_destination <- paste(folder_path, file_name, sep = "/")
  try(download.file(file_url, file_destination, mode = "wb"), silent = TRUE)
} 

for(dataset in EVAP_FNAMES_SHORT_2000_2019_FULL_RECORD){
 download_data(name = dataset)  
}
