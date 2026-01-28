# Rank datasets according to signal ----
source('source/evap_trend.R')
source('source/geo_functions.R')

## Data ----
### Input Data generated in projects/partition_evap/04
PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "/partition_evap/")
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

data <-  readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_dataset_opposing_significance.rds"))

## Land use ----
data_merge <- merge(evap_mask[, .(lat, lon, land_cover_short_class)], 
                    data, 
                    by = c("lon", "lat"))

area_opposing_0_01 <- data_merge[, .(area_opposing_0_01 = sum(area)), 
                                 .(land_cover_short_class, opposing_0_01, dataset)]
area_opposing_0_05 <- data_merge[, .(area_opposing_0_05 = sum(area)), 
                                 .(land_cover_short_class, opposing_0_05, dataset)]
area_opposing_0_1 <- data_merge[, .(area_opposing_0_1 = sum(area)), 
                                .(land_cover_short_class, opposing_0_1, dataset)]
area_opposing_0_2 <- data_merge[, .(area_opposing_0_2 = sum(area)), 
                                .(land_cover_short_class, opposing_0_2, dataset)]
area_opposing_all <- data_merge[, .(area_opposing_all = sum(area)), 
                                .(land_cover_short_class, opposing_all, dataset)]

area_opposing <- merge(area_opposing_0_01, area_opposing_0_05, 
                       by.x = c("opposing_0_01", "dataset", "land_cover_short_class"), 
                       by.y = c("opposing_0_05", "dataset", "land_cover_short_class"), all = TRUE)
area_opposing <- merge(area_opposing, area_opposing_0_1, 
                       by.x = c("opposing_0_01", "dataset", "land_cover_short_class"), 
                       by.y = c("opposing_0_1", "dataset", "land_cover_short_class"), all = TRUE)
area_opposing <- merge(area_opposing, area_opposing_0_2, 
                       by.x = c("opposing_0_01", "dataset", "land_cover_short_class"), 
                       by.y = c("opposing_0_2", "dataset", "land_cover_short_class"), all = TRUE)
area_opposing <- merge(area_opposing, area_opposing_all, 
                       by.x = c("opposing_0_01", "dataset", "land_cover_short_class"), 
                       by.y = c("opposing_all", "dataset", "land_cover_short_class"), all = TRUE)

opposing_melt <- melt(area_opposing,
                      measure.vars = c("area_opposing_0_01",
                                       "area_opposing_0_05",
                                       "area_opposing_0_1",
                                       "area_opposing_0_2",
                                       "area_opposing_all"
                      ))


opposing_melt[is.na(value), value := 0]

opposing_melt[, rank_datasets := rank(-value), .(variable, opposing_0_01, land_cover_short_class)]

opposing_melt[variable == "area_opposing_0_01", variable := "p <= 0.01", ]
opposing_melt[variable == "area_opposing_0_05", variable := "p <= 0.05", ]
opposing_melt[variable == "area_opposing_0_1", variable := "p <= 0.1", ]
opposing_melt[variable == "area_opposing_0_2", variable := "p <= 0.2", ]
opposing_melt[variable == "area_opposing_all", variable := "p <= 1", ]

opposing_melt <- opposing_melt[opposing_0_01 == 1]
opposing_melt[, opposing_0_01 := NULL]
saveRDS(opposing_melt, paste0(PATH_SAVE_EVAP_TREND, "land_use_dataset_rank_opposing_significance.rds")) 

## biome ----

data_merge <- merge(evap_mask[, .(lat, lon, biome_short_class)], 
                    data, 
                    by = c("lon", "lat"))

area_opposing_0_01 <- data_merge[, .(area_opposing_0_01 = sum(area)), 
                                 .(biome_short_class, opposing_0_01, dataset)]
area_opposing_0_05 <- data_merge[, .(area_opposing_0_05 = sum(area)), 
                                 .(biome_short_class, opposing_0_05, dataset)]
area_opposing_0_1 <- data_merge[, .(area_opposing_0_1 = sum(area)), 
                                .(biome_short_class, opposing_0_1, dataset)]
area_opposing_0_2 <- data_merge[, .(area_opposing_0_2 = sum(area)), 
                                .(biome_short_class, opposing_0_2, dataset)]
area_opposing_all <- data_merge[, .(area_opposing_all = sum(area)), 
                                .(biome_short_class, opposing_all, dataset)]

area_opposing <- merge(area_opposing_0_01, area_opposing_0_05, 
                       by.x = c("opposing_0_01", "dataset", "biome_short_class"), 
                       by.y = c("opposing_0_05", "dataset", "biome_short_class"), all = TRUE)
area_opposing <- merge(area_opposing, area_opposing_0_1, 
                       by.x = c("opposing_0_01", "dataset", "biome_short_class"), 
                       by.y = c("opposing_0_1", "dataset", "biome_short_class"), all = TRUE)
area_opposing <- merge(area_opposing, area_opposing_0_2, 
                       by.x = c("opposing_0_01", "dataset", "biome_short_class"), 
                       by.y = c("opposing_0_2", "dataset", "biome_short_class"), all = TRUE)
area_opposing <- merge(area_opposing, area_opposing_all, 
                       by.x = c("opposing_0_01", "dataset", "biome_short_class"), 
                       by.y = c("opposing_all", "dataset", "biome_short_class"), all = TRUE)

opposing_melt <- melt(area_opposing,
                      measure.vars = c("area_opposing_0_01",
                                       "area_opposing_0_05",
                                       "area_opposing_0_1",
                                       "area_opposing_0_2",
                                       "area_opposing_all"
                      ))


opposing_melt[is.na(value), value := 0]

opposing_melt[, rank_datasets := rank(-value), .(variable, opposing_0_01, biome_short_class)]

opposing_melt[variable == "area_opposing_0_01", variable := "p <= 0.01", ]
opposing_melt[variable == "area_opposing_0_05", variable := "p <= 0.05", ]
opposing_melt[variable == "area_opposing_0_1", variable := "p <= 0.1", ]
opposing_melt[variable == "area_opposing_0_2", variable := "p <= 0.2", ]
opposing_melt[variable == "area_opposing_all", variable := "p <= 1", ]

opposing_melt <- opposing_melt[opposing_0_01 == 1]
opposing_melt[, opposing_0_01 := NULL]
saveRDS(opposing_melt, paste0(PATH_SAVE_EVAP_TREND, "biome_dataset_rank_opposing_significance.rds")) 

## IPCC ----

data_merge <- merge(evap_mask[, .(lat, lon, IPCC_ref_region)], 
                    data, 
                    by = c("lon", "lat"))

area_opposing_0_01 <- data_merge[, .(area_opposing_0_01 = sum(area)), 
                                 .(IPCC_ref_region, opposing_0_01, dataset)]
area_opposing_0_05 <- data_merge[, .(area_opposing_0_05 = sum(area)), 
                                 .(IPCC_ref_region, opposing_0_05, dataset)]
area_opposing_0_1 <- data_merge[, .(area_opposing_0_1 = sum(area)), 
                                .(IPCC_ref_region, opposing_0_1, dataset)]
area_opposing_0_2 <- data_merge[, .(area_opposing_0_2 = sum(area)), 
                                .(IPCC_ref_region, opposing_0_2, dataset)]
area_opposing_all <- data_merge[, .(area_opposing_all = sum(area)), 
                                .(IPCC_ref_region, opposing_all, dataset)]

area_opposing <- merge(area_opposing_0_01, area_opposing_0_05, 
                       by.x = c("opposing_0_01", "dataset", "IPCC_ref_region"), 
                       by.y = c("opposing_0_05", "dataset", "IPCC_ref_region"), all = TRUE)
area_opposing <- merge(area_opposing, area_opposing_0_1, 
                       by.x = c("opposing_0_01", "dataset", "IPCC_ref_region"), 
                       by.y = c("opposing_0_1", "dataset", "IPCC_ref_region"), all = TRUE)
area_opposing <- merge(area_opposing, area_opposing_0_2, 
                       by.x = c("opposing_0_01", "dataset", "IPCC_ref_region"), 
                       by.y = c("opposing_0_2", "dataset", "IPCC_ref_region"), all = TRUE)
area_opposing <- merge(area_opposing, area_opposing_all, 
                       by.x = c("opposing_0_01", "dataset", "IPCC_ref_region"), 
                       by.y = c("opposing_all", "dataset", "IPCC_ref_region"), all = TRUE)

opposing_melt <- melt(area_opposing,
                      measure.vars = c("area_opposing_0_01",
                                       "area_opposing_0_05",
                                       "area_opposing_0_1",
                                       "area_opposing_0_2",
                                       "area_opposing_all"
                      ))


opposing_melt[is.na(value), value := 0]

opposing_melt[, rank_datasets := rank(-value), .(variable, opposing_0_01, IPCC_ref_region)]

opposing_melt[variable == "area_opposing_0_01", variable := "p <= 0.01", ]
opposing_melt[variable == "area_opposing_0_05", variable := "p <= 0.05", ]
opposing_melt[variable == "area_opposing_0_1", variable := "p <= 0.1", ]
opposing_melt[variable == "area_opposing_0_2", variable := "p <= 0.2", ]
opposing_melt[variable == "area_opposing_all", variable := "p <= 1", ]

opposing_melt <- opposing_melt[opposing_0_01 == 1]
opposing_melt[, opposing_0_01 := NULL]
saveRDS(opposing_melt, paste0(PATH_SAVE_EVAP_TREND, "IPCC_dataset_rank_opposing_significance.rds")) 

## evaporation quantiles ----

data_merge <- merge(evap_mask[, .(lat, lon, evap_quant)], 
                    data, 
                    by = c("lon", "lat"))

area_opposing_0_01 <- data_merge[, .(area_opposing_0_01 = sum(area)), 
                                 .(evap_quant, opposing_0_01, dataset)]
area_opposing_0_05 <- data_merge[, .(area_opposing_0_05 = sum(area)), 
                                 .(evap_quant, opposing_0_05, dataset)]
area_opposing_0_1 <- data_merge[, .(area_opposing_0_1 = sum(area)), 
                                .(evap_quant, opposing_0_1, dataset)]
area_opposing_0_2 <- data_merge[, .(area_opposing_0_2 = sum(area)), 
                                .(evap_quant, opposing_0_2, dataset)]
area_opposing_all <- data_merge[, .(area_opposing_all = sum(area)), 
                                .(evap_quant, opposing_all, dataset)]

area_opposing <- merge(area_opposing_0_01, area_opposing_0_05, 
                       by.x = c("opposing_0_01", "dataset", "evap_quant"), 
                       by.y = c("opposing_0_05", "dataset", "evap_quant"), all = TRUE)
area_opposing <- merge(area_opposing, area_opposing_0_1, 
                       by.x = c("opposing_0_01", "dataset", "evap_quant"), 
                       by.y = c("opposing_0_1", "dataset", "evap_quant"), all = TRUE)
area_opposing <- merge(area_opposing, area_opposing_0_2, 
                       by.x = c("opposing_0_01", "dataset", "evap_quant"), 
                       by.y = c("opposing_0_2", "dataset", "evap_quant"), all = TRUE)
area_opposing <- merge(area_opposing, area_opposing_all, 
                       by.x = c("opposing_0_01", "dataset", "evap_quant"), 
                       by.y = c("opposing_all", "dataset", "evap_quant"), all = TRUE)

opposing_melt <- melt(area_opposing,
                      measure.vars = c("area_opposing_0_01",
                                       "area_opposing_0_05",
                                       "area_opposing_0_1",
                                       "area_opposing_0_2",
                                       "area_opposing_all"
                      ))


opposing_melt[is.na(value), value := 0]

opposing_melt[, rank_datasets := rank(-value), .(variable, opposing_0_01, evap_quant)]

opposing_melt[variable == "area_opposing_0_01", variable := "p <= 0.01", ]
opposing_melt[variable == "area_opposing_0_05", variable := "p <= 0.05", ]
opposing_melt[variable == "area_opposing_0_1", variable := "p <= 0.1", ]
opposing_melt[variable == "area_opposing_0_2", variable := "p <= 0.2", ]
opposing_melt[variable == "area_opposing_all", variable := "p <= 1", ]

opposing_melt <- opposing_melt[opposing_0_01 == 1]
opposing_melt[, opposing_0_01 := NULL]
saveRDS(opposing_melt, paste0(PATH_SAVE_EVAP_TREND, "evaporation_quantiles_dataset_rank_opposing_significance.rds")) 

## elevation classes ----

data_merge <- merge(evap_mask[, .(lat, lon, elev_class)], 
                    data, 
                    by = c("lon", "lat"))

area_opposing_0_01 <- data_merge[, .(area_opposing_0_01 = sum(area)), 
                                 .(elev_class, opposing_0_01, dataset)]
area_opposing_0_05 <- data_merge[, .(area_opposing_0_05 = sum(area)), 
                                 .(elev_class, opposing_0_05, dataset)]
area_opposing_0_1 <- data_merge[, .(area_opposing_0_1 = sum(area)), 
                                .(elev_class, opposing_0_1, dataset)]
area_opposing_0_2 <- data_merge[, .(area_opposing_0_2 = sum(area)), 
                                .(elev_class, opposing_0_2, dataset)]
area_opposing_all <- data_merge[, .(area_opposing_all = sum(area)), 
                                .(elev_class, opposing_all, dataset)]

area_opposing <- merge(area_opposing_0_01, area_opposing_0_05, 
                       by.x = c("opposing_0_01", "dataset", "elev_class"), 
                       by.y = c("opposing_0_05", "dataset", "elev_class"), all = TRUE)
area_opposing <- merge(area_opposing, area_opposing_0_1, 
                       by.x = c("opposing_0_01", "dataset", "elev_class"), 
                       by.y = c("opposing_0_1", "dataset", "elev_class"), all = TRUE)
area_opposing <- merge(area_opposing, area_opposing_0_2, 
                       by.x = c("opposing_0_01", "dataset", "elev_class"), 
                       by.y = c("opposing_0_2", "dataset", "elev_class"), all = TRUE)
area_opposing <- merge(area_opposing, area_opposing_all, 
                       by.x = c("opposing_0_01", "dataset", "elev_class"), 
                       by.y = c("opposing_all", "dataset", "elev_class"), all = TRUE)

opposing_melt <- melt(area_opposing,
                      measure.vars = c("area_opposing_0_01",
                                       "area_opposing_0_05",
                                       "area_opposing_0_1",
                                       "area_opposing_0_2",
                                       "area_opposing_all"
                      ))


opposing_melt[is.na(value), value := 0]

opposing_melt[, rank_datasets := rank(-value), .(variable, opposing_0_01, elev_class)]

opposing_melt[variable == "area_opposing_0_01", variable := "p <= 0.01", ]
opposing_melt[variable == "area_opposing_0_05", variable := "p <= 0.05", ]
opposing_melt[variable == "area_opposing_0_1", variable := "p <= 0.1", ]
opposing_melt[variable == "area_opposing_0_2", variable := "p <= 0.2", ]
opposing_melt[variable == "area_opposing_all", variable := "p <= 1", ]

opposing_melt <- opposing_melt[opposing_0_01 == 1]
opposing_melt[, opposing_0_01 := NULL]
saveRDS(opposing_melt, paste0(PATH_SAVE_EVAP_TREND, "elevation_class_dataset_rank_opposing_significance.rds")) 

## Koeppen-Geiger ----

data_merge <- merge(evap_mask[, .(lat, lon, KG_beck)], 
                    data, 
                    by = c("lon", "lat"))

area_opposing_0_01 <- data_merge[, .(area_opposing_0_01 = sum(area)), 
                                 .(KG_beck, opposing_0_01, dataset)]
area_opposing_0_05 <- data_merge[, .(area_opposing_0_05 = sum(area)), 
                                 .(KG_beck, opposing_0_05, dataset)]
area_opposing_0_1 <- data_merge[, .(area_opposing_0_1 = sum(area)), 
                                .(KG_beck, opposing_0_1, dataset)]
area_opposing_0_2 <- data_merge[, .(area_opposing_0_2 = sum(area)), 
                                .(KG_beck, opposing_0_2, dataset)]
area_opposing_all <- data_merge[, .(area_opposing_all = sum(area)), 
                                .(KG_beck, opposing_all, dataset)]

area_opposing <- merge(area_opposing_0_01, area_opposing_0_05, 
                       by.x = c("opposing_0_01", "dataset", "KG_beck"), 
                       by.y = c("opposing_0_05", "dataset", "KG_beck"), all = TRUE)
area_opposing <- merge(area_opposing, area_opposing_0_1, 
                       by.x = c("opposing_0_01", "dataset", "KG_beck"), 
                       by.y = c("opposing_0_1", "dataset", "KG_beck"), all = TRUE)
area_opposing <- merge(area_opposing, area_opposing_0_2, 
                       by.x = c("opposing_0_01", "dataset", "KG_beck"), 
                       by.y = c("opposing_0_2", "dataset", "KG_beck"), all = TRUE)
area_opposing <- merge(area_opposing, area_opposing_all, 
                       by.x = c("opposing_0_01", "dataset", "KG_beck"), 
                       by.y = c("opposing_all", "dataset", "KG_beck"), all = TRUE)

opposing_melt <- melt(area_opposing,
                      measure.vars = c("area_opposing_0_01",
                                       "area_opposing_0_05",
                                       "area_opposing_0_1",
                                       "area_opposing_0_2",
                                       "area_opposing_all"
                      ))


opposing_melt[is.na(value), value := 0]

opposing_melt[variable == "area_opposing_0_01", variable := "p <= 0.01", ]
opposing_melt[variable == "area_opposing_0_05", variable := "p <= 0.05", ]
opposing_melt[variable == "area_opposing_0_1", variable := "p <= 0.1", ]
opposing_melt[variable == "area_opposing_0_2", variable := "p <= 0.2", ]
opposing_melt[variable == "area_opposing_all", variable := "p <= 1", ]

dataset_list <- unique(opposing_melt$dataset)
KG_list <- unique(opposing_melt$KG_beck)
variable_list <- unique(opposing_melt$variable)

fill_data <- 
  data.table(dataset = rep(rep(dataset_list, each = length(KG_list)), 5), 
             KG_beck = rep(rep(KG_list, length(dataset_list)),5),
             variable = rep(variable_list, 
                            each = length(KG_list)*length(dataset_list)))

fill_data[,value := 0]
opposing_melt <- opposing_melt[opposing_0_01 == 1]
data_check <- merge(opposing_melt, fill_data,
                    by = c('variable', 'dataset', 'KG_beck'), all =T)

data_check[, value := value.x]
data_check[is.na(value.x), value := 0]
data_check[, value.x := NULL]
data_check[, value.y := NULL]

data_check[, rank_datasets := rank(-value), .(variable, opposing_0_01, KG_beck)]
data_check[, opposing_0_01 := NULL]
saveRDS(data_check, paste0(PATH_SAVE_EVAP_TREND, "KG_beck_dataset_rank_opposing_significance.rds")) 

