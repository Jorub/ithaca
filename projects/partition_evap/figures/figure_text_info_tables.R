# Support text with numbers ----
source("source/partition_evap.R")

## Figure 1 ----
### a ----
evap_annual_vol <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "global_annual_means.rds"))
evap_annual_vol[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
evap_annual_vol[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote"]
evap_annual_vol[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydr./LSM model"]
evap_annual_vol[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Ensemble"]
evap_annual_vol <- evap_annual_vol[!(dataset == "etmonitor" & year == 2000), ]

evap_annual_vol[, mean(evap_volume)]
evap_annual_vol[, mean(evap_volume), dataset_type]
evap_annual_vol[, mean(evap_annual_mean), dataset_type]
evap_annual_vol[, mean(evap_annual_mean), dataset]
evap_annual_vol[, mean(evap_annual_mean), dataset]

### b-d ----
dataset_agreement_grid_wise <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP,"dataset_agreement_grid_wise.rds"))
total_area <- dataset_agreement_grid_wise[, sum(area)] 
dataset_agreement_grid_wise[, sum(area), IQR_agreement] 
dataset_agreement_grid_wise[, round(sum(area)/total_area, 2), IQR_agreement] 

dataset_agreement_grid_wise[, sum(area), rel_dataset_agreement ] 
dataset_agreement_grid_wise[, round(sum(area)/total_area, 2), rel_dataset_agreement ] 

dataset_agreement_grid_wise[, sum(area), dist_dataset_agreement ] 
dataset_agreement_grid_wise[, round(sum(area)/total_area, 2), dist_dataset_agreement ] 

dataset_agreement_grid_wise[, round(sum(area)/total_area, 2), 
                            .(dist_dataset_agreement, rel_dataset_agreement)] 

dataset_agreement_grid_wise[dist_dataset_agreement %in% c('High', 'Above average') &
                              rel_dataset_agreement %in% c('High', 'Above average'), 
                            round(sum(area)/total_area, 3), ] 

dataset_agreement_grid_wise[dist_dataset_agreement %in% c('High', 'Above average') &
                            rel_dataset_agreement %in% c('Low', 'Below average'), 
                            round(sum(area)/total_area, 3), ] 

dataset_agreement_grid_wise[rel_dataset_agreement %in% c('High', 'Above average') &
                              dist_dataset_agreement  %in% c('Low', 'Below average'), 
                            round(sum(area)/total_area, 3), ] 

dataset_agreement_grid_wise[rel_dataset_agreement %in% c('Low', 'Below average') &
                              dist_dataset_agreement  %in% c('Low', 'Below average'), 
                            round(sum(area)/total_area, 3), ] 

dataset_agreement_grid_wise[rel_dataset_agreement %in% c('High', 'Above average'), 
                            round(sum(area)/total_area, 3), ] 

## SI tables -----
### S3-5 Thresholds for agreement ----
thresholds_sIQR <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP,'thresholds_sIQR.RDS')) 
thresholds_IQR <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP,'thresholds_IQR.RDS'))
thresholds_dist <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP,'thresholds_distribution_agreement.RDS'))

### S6 and S7 Volume ----
load(paste0(PATH_SAVE_PARTITION_EVAP, "partition_evap.Rdata"))
global_agreement
global_agreement_area <- dataset_agreement_grid_wise[, .(area = sum(area), 
                                                        area_frac = round(sum(area)/total_area, 2)), 
                                                     rel_dataset_agreement ] 

quartile_agreement <- merge(global_agreement, global_agreement_area, by = 'rel_dataset_agreement')
quartile_agreement[,2:5]  <- round(quartile_agreement[,2:5],2) 


load(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_masks.Rdata"))
global_agreement
global_agreement_area <- dataset_agreement_grid_wise[, .(area = sum(area), 
                                                         area_frac = round(sum(area)/total_area, 2)), 
                                                     dist_dataset_agreement ] 

distribution_agreement <- merge(global_agreement, global_agreement_area, by.x = 'agreement_fac', by.y = 'dist_dataset_agreement')
distribution_agreement[,2:5]  <- round(distribution_agreement[,2:5],2) 


### S8-12 area of environments ----
biome <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "biome_area_fraction.rds"))
biome[order(as.character(biome_short_class))]
landcover <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "land_cover_area_fraction.rds"))
landcover[order(as.character(land_cover_short_class))]

evap <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evaporation_quantiles_area_fraction.rds"))

ipcc <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "ipcc_area_fraction.rds"))
elevation <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "elevation_area_fraction.rds"))


