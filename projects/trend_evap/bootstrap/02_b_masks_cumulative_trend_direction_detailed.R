# Cumulative significant trend direction per mask item  ----
source('source/evap_trend.R')
source('source/graphics.R')
source('source/geo_functions.R')

## Data ----
### Input Data generated in projects/partition_evap/04
PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "/partition_evap/")
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
### Input Data generated in projects/trend_evap/bootstrap/01_a
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_bootstrap.rds"))  

## Analysis ----
evap_trend_masks <- merge(evap_trend, evap_mask, all.x = T, by = c("lon", "lat"))
grid_cell_area <- unique(evap_trend[, .(lon, lat)]) %>% grid_area() # m2
evap_trend_masks <- grid_cell_area[evap_trend_masks, on = .(lon, lat)]

evap_trend_masks[slope >= 0 , trend_direction_detailed := "positive p > 0.1"]
evap_trend_masks[slope > 0 & p <= 0.1 , trend_direction_detailed := "positive p <= 0.1"]
evap_trend_masks[slope > 0 & p < 0.05 , trend_direction_detailed := "positive p < 0.05"]
evap_trend_masks[slope < 0  , trend_direction_detailed := "negative p > 0.1"]
evap_trend_masks[slope < 0 & p <= 0.1 , trend_direction_detailed := "negative p <= 0.1"]
evap_trend_masks[slope < 0 & p < 0.05 , trend_direction_detailed := "negative p < 0.05"]
evap_trend_masks[, trend_direction_detailed  := factor(trend_direction_detailed, 
                                                 level = c("positive p < 0.05", "positive p <= 0.1", "positive p > 0.1",
                                                           "negative p > 0.1", "negative p <= 0.1", "negative p < 0.05"), ordered = T),]

### Biome types ----
biome_trends <- evap_trend_masks[,.(trend_area = sum(area)),.(trend_direction_detailed, biome_class, dataset)]
biome_trends <- biome_trends[complete.cases(biome_trends)]
biome_trends[, biome_area:= sum(trend_area), .(biome_class, dataset)]
biome_trends[, biome_fraction:= trend_area/biome_area]
biome_trends[grepl("Tundra", biome_class) == TRUE, biome_short_class := "Tundra"]
biome_trends[grepl("Boreal Forests", biome_class) == TRUE, biome_short_class := "B. Forests"]
biome_trends[grepl("Dry Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Dry BL Forests"]
biome_trends[grepl("Moist Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Moist BL Forests"]
biome_trends[grepl("Subtropical Coniferous Forests", biome_class) == TRUE, biome_short_class := "T/S Coni. Forests"]
biome_trends[grepl("Temperate Conifer Forests", biome_class) == TRUE, biome_short_class := "T. Coni. Forests"]
biome_trends[grepl("Temperate Broadleaf & Mixed Forests", biome_class) == TRUE, biome_short_class := "T. BL Forests"]
biome_trends[grepl("Temperate Grasslands", biome_class) == TRUE, biome_short_class := "T. Grasslands"]
biome_trends[grepl("Subtropical Grasslands", biome_class) == TRUE, biome_short_class := "T/S Grasslands"]
biome_trends[grepl("Montane Grasslands", biome_class) == TRUE, biome_short_class := "M. Grasslands"]
biome_trends[grepl("Flooded", biome_class) == TRUE, biome_short_class := "Flooded"]
biome_trends[grepl("Mangroves", biome_class) == TRUE, biome_short_class := "Mangroves"]
biome_trends[grepl("Deserts", biome_class) == TRUE, biome_short_class := "Deserts"]
biome_trends[grepl("Mediterranean", biome_class) == TRUE, biome_short_class := "Mediterranean"]
biome_trends[grepl("N/A", biome_class) == TRUE, biome_short_class := NA]
biome_trends[, biome_short_class := factor(biome_short_class)]
biome_trends <- biome_trends[complete.cases(biome_trends)]



### ipcc ref regions  ----
ipcc_trends <- evap_trend_masks[,.(trend_area = sum(area)),.(trend_direction_detailed, IPCC_ref_region, dataset)]
ipcc_trends <- ipcc_trends[complete.cases(ipcc_trends)]
ipcc_trends[, ipcc_area:= sum(trend_area), .(IPCC_ref_region, dataset)]
ipcc_trends[, ipcc_fraction:= trend_area/ipcc_area]


### land use ----
land_trends <- evap_trend_masks[,.(trend_area = sum(area)),.(trend_direction_detailed, land_cover_short_class, dataset)]
land_trends <- land_trends[complete.cases(land_cover_short_class)]
land_trends[, land_area:= sum(trend_area), .(land_cover_short_class, dataset)]
land_trends[, land_fraction:= trend_area/land_area]



### elevation ----
elev_trends <- evap_trend_masks[,.(trend_area = sum(area)),.(trend_direction_detailed, elev_class, dataset)]
elev_trends <- elev_trends[complete.cases(elev_class)]
elev_trends[, elev_area:= sum(trend_area), .(elev_class, dataset)]
elev_trends[, elev_fraction:= trend_area/elev_area]

### evaporation ----
evap_trends <- evap_trend_masks[,.(trend_area = sum(area)),.(trend_direction_detailed, evap_quant, dataset)]
evap_trends <- evap_trends[complete.cases(evap_trends)]
evap_trends[, evap_area:= sum(trend_area), .(evap_quant, dataset)]
evap_trends[, evap_fraction:= trend_area/evap_area]


### KG 3 ----
KG_class_3_trends <- evap_trend_masks[,.(trend_area = sum(area)),.(trend_direction_detailed, KG_class_3, dataset)]
KG_class_3_trends <- KG_class_3_trends[complete.cases(KG_class_3_trends)]
KG_class_3_trends[, KG_class_3_area:= sum(trend_area), .(KG_class_3, dataset)]
KG_class_3_trends[, KG_class_3_fraction:= trend_area/KG_class_3_area]

### KG beck ----
KG_beck_trends <- evap_trend_masks[,.(trend_area = sum(area)),.(trend_direction_detailed, KG_beck, dataset)]
KG_beck_trends <- KG_beck_trends[complete.cases(KG_beck_trends)]
KG_beck_trends[, KG_beck_area:= sum(trend_area), .(KG_beck, dataset)]
KG_beck_trends[, KG_beck_fraction:= trend_area/KG_beck_area]



## Save data ----
saveRDS(biome_trends, paste0(PATH_SAVE_EVAP_TREND, "biome_cumulative_trend_direction_detailed_per_dataset_bootstrap.rds"))
saveRDS(ipcc_trends, paste0(PATH_SAVE_EVAP_TREND, "ipcc_cumulative_trend_direction_detailed_per_dataset_bootstrap.rds"))
saveRDS(land_trends, paste0(PATH_SAVE_EVAP_TREND, "land_cover_cumulative_trend_direction_detailed_per_dataset_bootstrap.rds"))
saveRDS(elev_trends, paste0(PATH_SAVE_EVAP_TREND, "elevation_cumulative_trend_direction_detailed_per_dataset_bootstrap.rds"))
saveRDS(evap_trends, paste0(PATH_SAVE_EVAP_TREND, "evap_quantiles_cumulative_trend_direction_detailed_per_dataset_bootstrap.rds"))
saveRDS(KG_class_3_trends, paste0(PATH_SAVE_EVAP_TREND, "KG_3_cumulative_trend_direction_detailed_per_dataset_bootstrap.rds"))
saveRDS(KG_beck_trends, paste0(PATH_SAVE_EVAP_TREND, "KG_beck_cumulative_trend_direction_detailed_per_dataset_bootstrap.rds"))

