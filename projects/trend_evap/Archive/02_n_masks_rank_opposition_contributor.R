# Rank datasets according to contributing opposition across masks ----
source('source/evap_trend.R')
source('source/geo_functions.R')

## Data ----
## Created in trend_evap/01_f
evap_opposing <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_topology_contributing_opposition.rds"))

### Input Data generated in projects/partition_evap/04
PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "/partition_evap/")
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))


## Estimate opposing fraction of all data ----
### latitude ----
lat_data <- evap_opposing
lat_data[, lat_brk := cut(lat, seq(-60, 90, 15))]

total_area_land <- lat_data[, .(total_area = sum(area)), .(lat_brk, variable, dataset_leftout)]
total_area_land <- total_area_land[, .(total_area = unique(total_area)), .(lat_brk)]

evap_sum_opposing <- lat_data[contributing_opposition == 1, .(sum_var = sum(area)), .(variable, lat_brk, dataset_leftout)]

dataset_list <- unique(lat_data$dataset)
lat_list <- unique(lat_data$lat_brk)
variable_list <- unique(lat_data$variable)

fill_data <- 
  data.table(dataset_leftout = rep(rep(dataset_list, each = length(lat_list)), 5), 
             lat_brk = rep(rep(lat_list, length(dataset_list)),5),
             variable = rep(variable_list, 
                            each = length(dataset_list)*length(lat_list)))

fill_data[,sum_var := 0]

data_check <- merge(fill_data, evap_sum_opposing,
                    by = c('variable', 'dataset_leftout', 'lat_brk'),
                    all = T)

data_check[, sum_var := sum_var.y]
data_check[is.na(sum_var.y), sum_var := 0]
data_check[, sum_var.x := NULL]
data_check[, sum_var.y := NULL]
lat_opposing <- merge(data_check, total_area_land, by = "lat_brk")

lat_opposing[, fraction := sum_var/total_area]
lat_opposing[, rank_opp := rank(-fraction, ties = "first"), .(variable, lat_brk)]

#### Save data ----
saveRDS(lat_opposing, paste0(PATH_SAVE_EVAP_TREND, "lat_groups_datasets_opposing_p_thresholds_bootstrap.rds"))

### land use ----
data_merge <- merge(evap_mask[, .(lat, lon, land_cover_short_class)], 
                  evap_opposing, 
                  by = c("lon", "lat"))

total_area_land <- data_merge[, .(total_area = sum(area)), .(land_cover_short_class, variable, dataset_leftout)]
total_area_land <- total_area_land[, .(total_area = unique(total_area)), .(land_cover_short_class)]

evap_sum_opposing <- data_merge[contributing_opposition == 1, .(sum_var = sum(area)), .(variable, land_cover_short_class, dataset_leftout)]

dataset_list <- unique(data_merge$dataset)
mask_list <- unique(data_merge$land_cover_short_class)
variable_list <- unique(data_merge$variable)

fill_data <- 
  data.table(dataset_leftout = rep(rep(dataset_list, each = length(mask_list)), 5), 
             land_cover_short_class = rep(rep(mask_list, length(dataset_list)),5),
             variable = rep(variable_list, 
                            each = length(dataset_list)*length(mask_list)))

fill_data[,sum_var := 0]

data_check <- merge(fill_data, evap_sum_opposing,
                    by = c('variable', 'dataset_leftout', 'land_cover_short_class'),
                    all = T)

data_check[, sum_var := sum_var.y]
data_check[is.na(sum_var.y), sum_var := 0]
data_check[, sum_var.x := NULL]
data_check[, sum_var.y := NULL]
data_opposing <- merge(data_check, total_area_land, by = "land_cover_short_class")

data_opposing[, fraction := sum_var/total_area]
data_opposing[, rank_opp := rank(-fraction, ties = "first"), .(variable, land_cover_short_class)]

#### Save data ----
saveRDS(data_opposing, paste0(PATH_SAVE_EVAP_TREND, "land_use_datasets_opposing_p_thresholds_bootstrap.rds"))

### biomes ----
data_merge <- merge(evap_mask[, .(lat, lon, biome_short_class)], 
                    evap_opposing, 
                    by = c("lon", "lat"))

total_area_land <- data_merge[, .(total_area = sum(area)), .(biome_short_class, variable, dataset_leftout)]
total_area_land <- total_area_land[, .(total_area = unique(total_area)), .(biome_short_class)]

evap_sum_opposing <- data_merge[contributing_opposition == 1, .(sum_var = sum(area)), .(variable, biome_short_class, dataset_leftout)]

dataset_list <- unique(data_merge$dataset)
mask_list <- unique(data_merge$biome_short_class)
variable_list <- unique(data_merge$variable)

fill_data <- 
  data.table(dataset_leftout = rep(rep(dataset_list, each = length(mask_list)), 5), 
             biome_short_class = rep(rep(mask_list, length(dataset_list)),5),
             variable = rep(variable_list, 
                            each = length(dataset_list)*length(mask_list)))

fill_data[,sum_var := 0]

data_check <- merge(fill_data, evap_sum_opposing,
                    by = c('variable', 'dataset_leftout', 'biome_short_class'),
                    all = T)

data_check[, sum_var := sum_var.y]
data_check[is.na(sum_var.y), sum_var := 0]
data_check[, sum_var.x := NULL]
data_check[, sum_var.y := NULL]
data_opposing <- merge(data_check, total_area_land, by = "biome_short_class")

data_opposing[, fraction := sum_var/total_area]
data_opposing[, rank_opp := rank(-fraction, ties = "first"), .(variable, biome_short_class)]

#### Save data ----
saveRDS(data_opposing, paste0(PATH_SAVE_EVAP_TREND, "biome_datasets_opposing_p_thresholds_bootstrap.rds"))

### IPCC reference regions ----

data_merge <- merge(evap_mask[, .(lat, lon, IPCC_ref_region)], 
                    evap_opposing, 
                    by = c("lon", "lat"))

total_area_land <- data_merge[, .(total_area = sum(area)), .(IPCC_ref_region, variable, dataset_leftout)]
total_area_land <- total_area_land[, .(total_area = unique(total_area)), .(IPCC_ref_region)]

evap_sum_opposing <- data_merge[contributing_opposition == 1, .(sum_var = sum(area)), .(variable, IPCC_ref_region, dataset_leftout)]

dataset_list <- unique(data_merge$dataset)
mask_list <- unique(data_merge$IPCC_ref_region)
variable_list <- unique(data_merge$variable)

fill_data <- 
  data.table(dataset_leftout = rep(rep(dataset_list, each = length(mask_list)), 5), 
             IPCC_ref_region = rep(rep(mask_list, length(dataset_list)),5),
             variable = rep(variable_list, 
                            each = length(dataset_list)*length(mask_list)))

fill_data[,sum_var := 0]

data_check <- merge(fill_data, evap_sum_opposing,
                    by = c('variable', 'dataset_leftout', 'IPCC_ref_region'),
                    all = T)

data_check[, sum_var := sum_var.y]
data_check[is.na(sum_var.y), sum_var := 0]
data_check[, sum_var.x := NULL]
data_check[, sum_var.y := NULL]
data_opposing <- merge(data_check, total_area_land, by = "IPCC_ref_region")

data_opposing[, fraction := sum_var/total_area]
data_opposing[, rank_opp := rank(-fraction, ties = "first"), .(variable, IPCC_ref_region)]

#### Save data ----
saveRDS(data_opposing, paste0(PATH_SAVE_EVAP_TREND, "IPCC_datasets_opposing_p_thresholds_bootstrap.rds"))

### Elevation classes ----
data_merge <- merge(evap_mask[, .(lat, lon, elev_class)], 
                    evap_opposing, 
                    by = c("lon", "lat"))

total_area_land <- data_merge[, .(total_area = sum(area)), .(elev_class, variable, dataset_leftout)]
total_area_land <- total_area_land[, .(total_area = unique(total_area)), .(elev_class)]

evap_sum_opposing <- data_merge[contributing_opposition == 1, .(sum_var = sum(area)), .(variable, elev_class, dataset_leftout)]

dataset_list <- unique(data_merge$dataset)
mask_list <- unique(data_merge$elev_class)
variable_list <- unique(data_merge$variable)

fill_data <- 
  data.table(dataset_leftout = rep(rep(dataset_list, each = length(mask_list)), 5), 
             elev_class = rep(rep(mask_list, length(dataset_list)),5),
             variable = rep(variable_list, 
                            each = length(dataset_list)*length(mask_list)))

fill_data[,sum_var := 0]

data_check <- merge(fill_data, evap_sum_opposing,
                    by = c('variable', 'dataset_leftout', 'elev_class'),
                    all = T)

data_check[, sum_var := sum_var.y]
data_check[is.na(sum_var.y), sum_var := 0]
data_check[, sum_var.x := NULL]
data_check[, sum_var.y := NULL]
data_opposing <- merge(data_check, total_area_land, by = "elev_class")

data_opposing[, fraction := sum_var/total_area]
data_opposing[, rank_opp := rank(-fraction, ties = "first"), .(variable, elev_class)]

#### Save data ----
saveRDS(data_opposing, paste0(PATH_SAVE_EVAP_TREND, "elevation_classes_datasets_opposing_p_thresholds_bootstrap.rds"))

### Evaporation quantiles ----
data_merge <- merge(evap_mask[, .(lat, lon, evap_quant)], 
                    evap_opposing, 
                    by = c("lon", "lat"))

total_area_land <- data_merge[, .(total_area = sum(area)), .(evap_quant, variable, dataset_leftout)]
total_area_land <- total_area_land[, .(total_area = unique(total_area)), .(evap_quant)]

evap_sum_opposing <- data_merge[contributing_opposition == 1, .(sum_var = sum(area)), .(variable, evap_quant, dataset_leftout)]

dataset_list <- unique(data_merge$dataset)
mask_list <- unique(data_merge$evap_quant)
variable_list <- unique(data_merge$variable)

fill_data <- 
  data.table(dataset_leftout = rep(rep(dataset_list, each = length(mask_list)), 5), 
             evap_quant = rep(rep(mask_list, length(dataset_list)),5),
             variable = rep(variable_list, 
                            each = length(dataset_list)*length(mask_list)))

fill_data[,sum_var := 0]

data_check <- merge(fill_data, evap_sum_opposing,
                    by = c('variable', 'dataset_leftout', 'evap_quant'),
                    all = T)

data_check[, sum_var := sum_var.y]
data_check[is.na(sum_var.y), sum_var := 0]
data_check[, sum_var.x := NULL]
data_check[, sum_var.y := NULL]
data_opposing <- merge(data_check, total_area_land, by = "evap_quant")

data_opposing[, fraction := sum_var/total_area]
data_opposing[, rank_opp := rank(-fraction, ties = "first"), .(variable, evap_quant)]

#### Save data ----
saveRDS(data_opposing, paste0(PATH_SAVE_EVAP_TREND, "evaporation_quantiles_datasets_opposing_p_thresholds_bootstrap.rds"))

### Koeppen-Geiger classes ----

data_merge <- merge(evap_mask[, .(lat, lon, KG_beck)], 
                    evap_opposing, 
                    by = c("lon", "lat"))

total_area_land <- data_merge[, .(total_area = sum(area)), .(KG_beck, variable, dataset_leftout)]
total_area_land <- total_area_land[, .(total_area = unique(total_area)), .(KG_beck)]

evap_sum_opposing <- data_merge[contributing_opposition == 1, .(sum_var = sum(area)), .(variable, KG_beck, dataset_leftout)]

dataset_list <- unique(data_merge$dataset)
mask_list <- unique(data_merge$KG_beck)
variable_list <- unique(data_merge$variable)

fill_data <- 
  data.table(dataset_leftout = rep(rep(dataset_list, each = length(mask_list)), 5), 
             KG_beck = rep(rep(mask_list, length(dataset_list)),5),
             variable = rep(variable_list, 
                            each = length(dataset_list)*length(mask_list)))

fill_data[,sum_var := 0]

data_check <- merge(fill_data, evap_sum_opposing,
                    by = c('variable', 'dataset_leftout', 'KG_beck'),
                    all = T)

data_check[, sum_var := sum_var.y]
data_check[is.na(sum_var.y), sum_var := 0]
data_check[, sum_var.x := NULL]
data_check[, sum_var.y := NULL]
data_opposing <- merge(data_check, total_area_land, by = "KG_beck")

data_opposing[, fraction := sum_var/total_area]
data_opposing[, rank_opp := rank(-fraction, ties = "first"), .(variable, KG_beck)]

data_opposing <- data_opposing[!is.na(KG_beck)]

#### Save data ----
saveRDS(data_opposing, paste0(PATH_SAVE_EVAP_TREND, "KG_beck_datasets_opposing_p_thresholds_bootstrap.rds"))


