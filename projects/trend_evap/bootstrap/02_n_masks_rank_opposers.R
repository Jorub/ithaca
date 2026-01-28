# Rank datasets according to signal ----
source('source/evap_trend.R')
source('source/geo_functions.R')


## Data ----
## Created in trend_evap/01_d
evap_trend_all <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_DCI_trend_groups_p_thresholds_bootstrap.rds"))

## Created in trend_evap/01_e
evap_trend_leftout <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_DCI_trend_groups_p_thresholds_bootstrap_dataset_leftout.rds"))

### Input Data generated in projects/partition_evap/04
PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "/partition_evap/")
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))


## Estimate opposing fraction of all data ----
## add area estimate 
grid_cell_area <- unique(evap_trend_all[, .(lon, lat)]) %>% grid_area() # m2

evap_trend_all  <- grid_cell_area[evap_trend_all, on = .(lon, lat)]


### land use ----
land_use <- merge(evap_mask[, .(lat, lon, land_cover_short_class)], 
                  evap_trend_all[, .(lon, lat, trend_0_01, trend_0_05, trend_0_1, trend_0_2, trend_all, area)], 
                  by = c("lon", "lat"))

setnames(land_use, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

land_use_melt <- melt(land_use, measure.vars = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

total_area_land <- land_use_melt[, .(total_area = sum(area)), .(land_cover_short_class, variable)]
total_area_land <- total_area_land[, .(total_area = unique(total_area)), .(land_cover_short_class)]

evap_sum_opposing <- land_use_melt[value == "opposing", .(sum_var = sum(area)), .(variable, land_cover_short_class)]
evap_sum_opposing <- merge(evap_sum_opposing, total_area_land, by = "land_cover_short_class")
evap_sum_opposing[, fraction := sum_var/total_area]

## Estimate opposing fraction dataset leftout
land_use_leftout <- merge(evap_mask[, .(lat, lon, land_cover_short_class)], 
                  evap_trend_leftout[, .(lon, lat, trend_0_01, trend_0_05, trend_0_1, trend_0_2, trend_all, dataset_leftout)], 
                  by = c("lon", "lat"))

land_use_leftout  <- grid_cell_area[land_use_leftout, on = .(lon, lat)]
setnames(land_use_leftout, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

land_use_leftout_melt <- melt(land_use_leftout, measure.vars = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))
gleam_add <- land_use_leftout_melt[variable == "p <= 0.01" & dataset_leftout == 'gleam' 
                                   & lat == 22.875 & land_cover_short_class == 'Other']
gleam_add[, area := 0]
gleam_add[, value := 'opposing']

land_use_leftout_melt <- rbind(land_use_leftout_melt, gleam_add)

land_use_leftout_sum_opposing <- land_use_leftout_melt[value == "opposing", .(sum_leftout = sum(area)), .(variable, dataset_leftout, land_cover_short_class)]
land_use_leftout_sum_opposing <- merge(land_use_leftout_sum_opposing, total_area_land, by = "land_cover_short_class")
land_use_leftout_sum_opposing[, fraction_leftout := sum_leftout/total_area]

land_opposing <- merge(land_use_leftout_sum_opposing, evap_sum_opposing, 
                       by = c("variable", "land_cover_short_class", "total_area"),
                       all = T)

land_opposing[, fraction_diff := fraction- fraction_leftout]

land_opposing[, rank_opp := rank(-fraction_diff, ties = "first"), .(variable, land_cover_short_class)]

#### Save data ----
saveRDS(land_opposing, paste0(PATH_SAVE_EVAP_TREND, "land_use_datasets_opposing_p_thresholds_bootstrap.rds"))


### biomes ----
data_merge <- merge(evap_mask[, .(lat, lon, biome_short_class)], 
                  evap_trend_all[, .(lon, lat, trend_0_01, trend_0_05, trend_0_1, trend_0_2, trend_all, area)], 
                  by = c("lon", "lat"))

setnames(data_merge, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_merge_melt <- melt(data_merge, measure.vars = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

total_area_land <- data_merge_melt[, .(total_area = sum(area)), .(biome_short_class, variable)]
total_area_land <- total_area_land[, .(total_area = unique(total_area)), .(biome_short_class)]

evap_sum_opposing <- data_merge_melt[value == "opposing", .(sum_var = sum(area)), .(variable, biome_short_class)]
evap_sum_opposing <- merge(evap_sum_opposing, total_area_land, by = "biome_short_class")
evap_sum_opposing[, fraction := sum_var/total_area]

#### Estimate opposing fraction dataset leftout ----
data_merge_leftout <- merge(evap_mask[, .(lat, lon, biome_short_class)], 
                          evap_trend_leftout[, .(lon, lat, trend_0_01, trend_0_05, trend_0_1, trend_0_2, trend_all, dataset_leftout)], 
                          by = c("lon", "lat"))

data_merge_leftout  <- grid_cell_area[data_merge_leftout, on = .(lon, lat)]
setnames(data_merge_leftout, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_merge_leftout_melt <- melt(data_merge_leftout, measure.vars = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_merge_leftout_sum_opposing <- data_merge_leftout_melt[value == "opposing", .(sum_leftout = sum(area)), .(variable, dataset_leftout, biome_short_class)]
data_merge_leftout_sum_opposing <- merge(data_merge_leftout_sum_opposing, total_area_land, by = "biome_short_class")
data_merge_leftout_sum_opposing[, fraction_leftout := sum_leftout/total_area]

data_opposing <- merge(data_merge_leftout_sum_opposing, evap_sum_opposing, by = c("variable", "biome_short_class", "total_area"))

data_opposing[, fraction_diff := fraction- fraction_leftout]

data_opposing[, rank_opp := rank(-fraction_diff, ties = "first"), .(variable, biome_short_class)]

#### Save data ----
saveRDS(data_opposing, paste0(PATH_SAVE_EVAP_TREND, "biome_datasets_opposing_p_thresholds_bootstrap.rds"))

### IPCC reference regions ----

data_merge <- merge(evap_mask[, .(lat, lon, IPCC_ref_region)], 
                    evap_trend_all[, .(lon, lat, trend_0_01, trend_0_05, trend_0_1, trend_0_2, trend_all, area)], 
                    by = c("lon", "lat"))

setnames(data_merge, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_merge_melt <- melt(data_merge, measure.vars = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

total_area_land <- data_merge_melt[, .(total_area = sum(area)), .(IPCC_ref_region, variable)]
total_area_land <- total_area_land[, .(total_area = unique(total_area)), .(IPCC_ref_region)]

evap_sum_opposing <- data_merge_melt[value == "opposing", .(sum_var = sum(area)), .(variable, IPCC_ref_region)]

ipcc_list <- unique(evap_sum_opposing$IPCC_ref_region)
variable_list <- unique(evap_sum_opposing$variable)

fill_data <- data.table(IPCC_ref_region = rep(rep(ipcc_list,5)),
                        variable = rep(variable_list, each = length(ipcc_list)))

fill_data[,sum_var := 0]

evap_sum_fill <- merge(evap_sum_opposing, fill_data, 
                       all = T,
                       by = c('IPCC_ref_region', 'variable'))

evap_sum_fill[,sum_var := sum_var.x]
evap_sum_fill[is.na(sum_var.x),sum_var := 0]
evap_sum_fill[,sum_var.x := NULL]
evap_sum_fill[,sum_var.y := NULL]

evap_sum_opposing <- merge(evap_sum_fill, total_area_land, by = "IPCC_ref_region")
evap_sum_opposing[, fraction := sum_var/total_area]

#### Estimate opposing fraction dataset leftout ----
data_merge_leftout <- merge(evap_mask[, .(lat, lon, IPCC_ref_region)], 
                            evap_trend_leftout[, .(lon, lat, trend_0_01, trend_0_05, trend_0_1, trend_0_2, trend_all, dataset_leftout)], 
                            by = c("lon", "lat"))

data_merge_leftout  <- grid_cell_area[data_merge_leftout, on = .(lon, lat)]
setnames(data_merge_leftout, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_merge_leftout_melt <- melt(data_merge_leftout, measure.vars = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

### fill data with no opposing area when leftout for given p-value and grid cell

data_merge_leftout_sum_opposing <- data_merge_leftout_melt[value == "opposing", .(sum_leftout = sum(area)), .(variable, dataset_leftout, IPCC_ref_region)]

dataset_list <- unique(data_merge_leftout_sum_opposing$dataset)
ipcc_list <- unique(evap_sum_opposing$IPCC_ref_region)
variable_list <- unique(evap_sum_opposing$variable)

fill_data <- 
  data.table(dataset_leftout = rep(rep(dataset_list, each = length(ipcc_list)), 5), 
             IPCC_ref_region = rep(rep(ipcc_list, length(dataset_list)),5),
             variable = rep(variable_list, 
                            each = length(dataset_list)*length(ipcc_list)))

fill_data[,sum_leftout := 0]

data_check <- merge(fill_data, data_merge_leftout_sum_opposing,
                    by = c('variable', 'dataset_leftout', 'IPCC_ref_region'),
                    all = T)
data_check[, sum_leftout := sum_leftout.y]
data_check[is.na(sum_leftout.y), sum_leftout := 0]
data_check[, sum_leftout.x := NULL]
data_check[, sum_leftout.y := NULL]

data_merge_leftout_sum_opposing <- merge(data_check, total_area_land, by = "IPCC_ref_region")

data_merge_leftout_sum_opposing[, fraction_leftout := sum_leftout/total_area]

data_opposing <- merge(data_merge_leftout_sum_opposing, 
                       evap_sum_opposing, 
                       by = c("variable", "IPCC_ref_region", "total_area"),
                       all = T)

data_opposing[, fraction_diff := fraction- fraction_leftout]

data_opposing[, rank_opp := rank(-fraction_diff, ties = "first"), .(variable, IPCC_ref_region)]

#### Save data ----
saveRDS(data_opposing, paste0(PATH_SAVE_EVAP_TREND, "IPCC_datasets_opposing_p_thresholds_bootstrap.rds"))

### Elevation classes ----

data_merge <- merge(evap_mask[, .(lat, lon, elev_class)], 
                    evap_trend_all[, .(lon, lat, trend_0_01, trend_0_05, trend_0_1, trend_0_2, trend_all, area)], 
                    by = c("lon", "lat"))

setnames(data_merge, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_merge_melt <- melt(data_merge, measure.vars = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

total_area_land <- data_merge_melt[, .(total_area = sum(area)), .(elev_class, variable)]
total_area_land <- total_area_land[, .(total_area = unique(total_area)), .(elev_class)]

evap_sum_opposing <- data_merge_melt[value == "opposing", .(sum_var = sum(area)), .(variable, elev_class)]
evap_sum_opposing <- merge(evap_sum_opposing, total_area_land, by = "elev_class")
evap_sum_opposing[, fraction := sum_var/total_area]

#### Estimate opposing fraction dataset leftout ----
data_merge_leftout <- merge(evap_mask[, .(lat, lon, elev_class)], 
                            evap_trend_leftout[, .(lon, lat, trend_0_01, trend_0_05, trend_0_1, trend_0_2, trend_all, dataset_leftout)], 
                            by = c("lon", "lat"))

data_merge_leftout  <- grid_cell_area[data_merge_leftout, on = .(lon, lat)]
setnames(data_merge_leftout, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_merge_leftout_melt <- melt(data_merge_leftout, measure.vars = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_merge_leftout_sum_opposing <- data_merge_leftout_melt[value == "opposing", .(sum_leftout = sum(area)), .(variable, dataset_leftout, elev_class)]
data_merge_leftout_sum_opposing <- merge(data_merge_leftout_sum_opposing, total_area_land, by = "elev_class")
data_merge_leftout_sum_opposing[, fraction_leftout := sum_leftout/total_area]

data_opposing <- merge(data_merge_leftout_sum_opposing, evap_sum_opposing, by = c("variable", "elev_class", "total_area"))

data_opposing[, fraction_diff := fraction- fraction_leftout]

data_opposing[, rank_opp := rank(-fraction_diff, ties = "first"), .(variable, elev_class)]

#### Save data ----
saveRDS(data_opposing, paste0(PATH_SAVE_EVAP_TREND, "elevation_classes_datasets_opposing_p_thresholds_bootstrap.rds"))

### Evaporation quantiles ----

data_merge <- merge(evap_mask[, .(lat, lon, evap_quant)], 
                    evap_trend_all[, .(lon, lat, trend_0_01, trend_0_05, trend_0_1, trend_0_2, trend_all, area)], 
                    by = c("lon", "lat"))

setnames(data_merge, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_merge_melt <- melt(data_merge, measure.vars = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

total_area_land <- data_merge_melt[, .(total_area = sum(area)), .(evap_quant, variable)]
total_area_land <- total_area_land[, .(total_area = unique(total_area)), .(evap_quant)]

evap_sum_opposing <- data_merge_melt[value == "opposing", .(sum_var = sum(area)), .(variable, evap_quant)]
evap_sum_opposing <- merge(evap_sum_opposing, total_area_land, by = "evap_quant")
evap_sum_opposing[, fraction := sum_var/total_area]

#### Estimate opposing fraction dataset leftout ----
data_merge_leftout <- merge(evap_mask[, .(lat, lon, evap_quant)], 
                            evap_trend_leftout[, .(lon, lat, trend_0_01, trend_0_05, trend_0_1, trend_0_2, trend_all, dataset_leftout)], 
                            by = c("lon", "lat"))

data_merge_leftout  <- grid_cell_area[data_merge_leftout, on = .(lon, lat)]
setnames(data_merge_leftout, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_merge_leftout_melt <- melt(data_merge_leftout, measure.vars = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_merge_leftout_sum_opposing <- data_merge_leftout_melt[value == "opposing", .(sum_leftout = sum(area)), .(variable, dataset_leftout, evap_quant)]
data_merge_leftout_sum_opposing <- merge(data_merge_leftout_sum_opposing, total_area_land, by = "evap_quant")
data_merge_leftout_sum_opposing[, fraction_leftout := sum_leftout/total_area]

data_opposing <- merge(data_merge_leftout_sum_opposing, evap_sum_opposing, by = c("variable", "evap_quant", "total_area"))

data_opposing[, fraction_diff := fraction- fraction_leftout]

data_opposing[, rank_opp := rank(-fraction_diff, ties = "first"), .(variable, evap_quant)]

#### Save data ----
saveRDS(data_opposing, paste0(PATH_SAVE_EVAP_TREND, "evaporation_quantiles_datasets_opposing_p_thresholds_bootstrap.rds"))

### Koeppen-Geiger classes ----

data_merge <- merge(evap_mask[, .(lat, lon, KG_beck)], 
                    evap_trend_all[, .(lon, lat, trend_0_01, trend_0_05, trend_0_1, trend_0_2, trend_all, area)], 
                    by = c("lon", "lat"))

setnames(data_merge, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_merge_melt <- melt(data_merge, measure.vars = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

total_area_land <- data_merge_melt[, .(total_area = sum(area)), .(KG_beck, variable)]
total_area_land <- total_area_land[, .(total_area = unique(total_area)), .(KG_beck)]

evap_sum_opposing <- data_merge_melt[value == "opposing", .(sum_var = sum(area)), .(variable, KG_beck)]

KG_list <- unique(evap_sum_opposing$KG_beck)
variable_list <- unique(evap_sum_opposing$variable)

fill_data <- data.table(KG_beck = rep(rep(KG_list,5)),
             variable = rep(variable_list, each = length(KG_list)))

fill_data[,sum_var := 0]

evap_sum_fill <- merge(evap_sum_opposing, fill_data, 
                       all = T,
                       by = c('KG_beck', 'variable'))
evap_sum_fill[,sum_var := sum_var.x]
evap_sum_fill[is.na(sum_var.x),sum_var := 0]
evap_sum_fill[,sum_var.x := NULL]
evap_sum_fill[,sum_var.y := NULL]

evap_sum_opposing <- merge(evap_sum_fill, total_area_land, by = "KG_beck")
evap_sum_opposing[, fraction := sum_var/total_area]

#### Estimate opposing fraction dataset leftout ----
data_merge_leftout <- merge(evap_mask[, .(lat, lon, KG_beck)], 
                            evap_trend_leftout[, .(lon, lat, trend_0_01, trend_0_05, trend_0_1, trend_0_2, trend_all, dataset_leftout)], 
                            by = c("lon", "lat"))

data_merge_leftout  <- grid_cell_area[data_merge_leftout, on = .(lon, lat)]
setnames(data_merge_leftout, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_merge_leftout_melt <- melt(data_merge_leftout, measure.vars = c("p <= 0.01", "p <= 0.05", "p <= 0.1", "p <= 0.2", "p <= 1"))

data_merge_leftout_sum_opposing <- data_merge_leftout_melt[value == "opposing", .(sum_leftout = sum(area)), .(variable, dataset_leftout, KG_beck)]

dataset_list <- unique(data_merge_leftout_sum_opposing$dataset)
KG_list <- unique(data_merge_leftout_sum_opposing$KG_beck)
variable_list <- unique(data_merge_leftout_sum_opposing$variable)

fill_data <- 
  data.table(dataset_leftout = rep(rep(dataset_list, each = length(KG_list)), 5), 
             KG_beck = rep(rep(KG_list, length(dataset_list)),5),
             variable = rep(variable_list, 
                            each = length(dataset_list)*length(KG_list)))

fill_data[,sum_leftout := 0]

data_check <- merge(fill_data, data_merge_leftout_sum_opposing,
                    by = c('variable', 'dataset_leftout', 'KG_beck'),
                    all = T)
data_check[, sum_leftout := sum_leftout.y]
data_check[is.na(sum_leftout.y), sum_leftout := 0]
data_check[, sum_leftout.x := NULL]
data_check[, sum_leftout.y := NULL]

data_merge_leftout_sum_opposing <- merge(data_check, total_area_land, by = "KG_beck")
data_merge_leftout_sum_opposing[, fraction_leftout := sum_leftout/total_area]

data_opposing <- merge(data_merge_leftout_sum_opposing, evap_sum_opposing, by = c("variable", "KG_beck", "total_area"))

data_opposing[, fraction_diff := fraction- fraction_leftout]

data_opposing[, rank_opp := rank(-fraction_diff, ties = "first"), .(variable, KG_beck)]

#### Save data ----
saveRDS(data_opposing, paste0(PATH_SAVE_EVAP_TREND, "KG_beck_datasets_opposing_p_thresholds_bootstrap.rds"))


