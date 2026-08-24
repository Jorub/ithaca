# Trend (Theil-Sen Slope) for each grid for global ensemble of all data products with bootstrap----
source('source/evap_trend.R')
source('source/geo_functions.R')

library(openair)

## Data ----
evap_datasets <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_datasets_clean.rds"))

## Analysis ----
evap_datasets[, year := as.numeric(as.character(year))]
evap_datasets <- evap_datasets[year > 2004]
evap_datasets[, date := paste0(year, "-01-01 00:00:00")]
evap_datasets[, date := as.POSIXct(date)]

global_datasets <- evap_datasets[, unique(dataset)]

### Calculate slopes and save ----
for (dataset_num in global_datasets){
  print(dataset_num)
  evap_dataset_sel <- evap_datasets[dataset == dataset_num,]
  evap_dataset_sel_trend <- evap_trends_boot(evap_dataset_sel)
  
  evap_dataset_sel_trend[p > 0.05, significant_theil_sen:= FALSE] 
  evap_dataset_sel_trend[p <= 0.05, significant_theil_sen:= TRUE] 
  
  saveRDS(evap_dataset_sel_trend, 
          paste0(PATH_SAVE_EVAP_TREND, "evap_dataset_",dataset_num,"_trend_bootstrap_2005_2019.rds"))  
}

### Assemble data ----

for (dataset_num in global_datasets){
  dummy_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_dataset_",dataset_num,"_trend_bootstrap_2005_2019.rds"))  
  if(dataset_num != global_datasets[1]){
    evap_trend <- rbind( evap_trend, dummy_trend)
  }else{
    evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_dataset_",dataset_num,"_trend_bootstrap_2005_2019.rds"))  
  }
}

### Merged data save
saveRDS(evap_trend, paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_bootstrap_merged_2005_2019.rds"))  

### Trend direction
evap_trend[significant_theil_sen*slope > 0, trend_direction := "positive significant" ]
evap_trend[significant_theil_sen*slope <= 0, trend_direction := "negative significant"  ]
evap_trend[significant_theil_sen == FALSE & slope > 0, trend_direction := "positive" ]
evap_trend[significant_theil_sen == FALSE & slope <= 0, trend_direction := "negative"  ]

evap_trend <- evap_trend[complete.cases(evap_trend)]
evap_trend <- evap_trend[,dataset_count := .N,.(lon, lat)]
evap_trend_complete_only <- evap_trend[dataset_count >= n_datasets_2000_2019,]

## save data----
saveRDS(evap_trend, paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_bootstrap_2005_2019.rds"))  
saveRDS(evap_trend_complete_only , paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_intersection_lat_lon_bootstrap_2005_2019.rds"))  
