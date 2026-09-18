# Calculate evap slopes and significance for each grid for all datasets using and p-values using bootstrap ----
# Significant slopes have p-value <= 0.05 derived from bootstrap ----

## source and libraries ----
source('source/evap_trend.R')
library("openair")

## Data ----
### Input Data generated in projects/partition_evap/01_c
evap_datasets <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_datasets_clean.rds"))
evap_datasets[, year := as.numeric(as.character(year))]
evap_datasets <- evap_datasets[!(dataset == "etmonitor" & year == 2000), ]
evap_datasets[, date := paste0(year, "-01-01 00:00:00")]
evap_datasets[, date := as.POSIXct(date)]

## Slope Analysis ----

global_datasets <- evap_datasets[, unique(dataset)]

### Calculate slopes and save ----
for (dataset_num in global_datasets){
  print(dataset_num)
  evap_dataset_sel <- evap_datasets[dataset == dataset_num,]
  evap_dataset_sel_trend <- evap_trends_boot(evap_dataset_sel)
  
  evap_dataset_sel_trend[p > 0.05, significant_theil_sen:= FALSE] 
  evap_dataset_sel_trend[p <= 0.05, significant_theil_sen:= TRUE] 
  
  saveRDS(evap_dataset_sel_trend, paste0(PATH_SAVE_EVAP_TREND, "evap_dataset_",dataset_num,"_trend_bootstrap.rds"))  
}

### Assemble data ----

for (dataset_num in global_datasets){
  dummy_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_dataset_",dataset_num,"_trend_bootstrap.rds"))  
  if(dataset_num != global_datasets[1]){
    evap_trend <- rbind( evap_trend, dummy_trend)
  }else{
    evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_dataset_",dataset_num,"_trend_bootstrap.rds"))  
  }
}

### Merged data save
saveRDS(evap_trend, paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_bootstrap_merged.rds"))  
