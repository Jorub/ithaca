# Volume and area fractions of agreement indices

source('source/partition_evap.R')

dataset_agreement <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "dataset_agreement_grid_wise.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))

evap_mean <- evap_grid[, .(evap_volume = mean(evap_volume), area = area), .(lat, lon)]
evap_mean <- unique(evap_mean)

global <- merge(dataset_agreement, evap_mean[, .(lon, lat, evap_volume)], by = c("lon", "lat"), all.x = T)

global_agreement <- global[, .(evap_sum = sum(evap_volume), area_sum = sum(area)), .(rel_dataset_agreement,
                                                                                     dist_dataset_agreement)]

global_agreement[, volume_fraction := evap_sum/sum(evap_sum)]  
global_agreement[, area_fraction := area_sum/sum(area_sum)]  
global_agreement[, area_sum := area_sum*M2_TO_KM2]


magnitude_agreement <- global_agreement[,.(evap_sum = sum(evap_sum), area_sum = sum(area_sum),
                                           volume_fraction = sum(volume_fraction), 
                                           area_fraction = sum(area_fraction)), .(rel_dataset_agreement)]

distribution_agreement <- global_agreement[,.(evap_sum = sum(evap_sum), area_sum = sum(area_sum),
                                           volume_fraction = sum(volume_fraction), 
                                           area_fraction = sum(area_fraction)), .(dist_dataset_agreement)]

distribution_agreement <- round(distribution_agreement, 2)

higher <- c("High", "Above average")
lower <- c("Low", "Below average")

global_agreement[rel_dataset_agreement %in% higher & dist_dataset_agreement %in% higher,
                 .(evap_sum = sum(evap_sum), area_sum = sum(area_sum),
                   volume_fraction = sum(volume_fraction),
                   area_fraction = sum(area_fraction))]

global_agreement[rel_dataset_agreement %in% lower & dist_dataset_agreement %in% lower,
                 .(evap_sum = sum(evap_sum), area_sum = sum(area_sum),
                   volume_fraction = sum(volume_fraction),
                   area_fraction = sum(area_fraction))]

global_agreement[rel_dataset_agreement %in% higher & dist_dataset_agreement %in% lower,
                 .(evap_sum = sum(evap_sum), area_sum = sum(area_sum),
                   volume_fraction = sum(volume_fraction),
                   area_fraction = sum(area_fraction))]

global_agreement[rel_dataset_agreement %in% lower & dist_dataset_agreement %in% higher,
                 .(evap_sum = sum(evap_sum), area_sum = sum(area_sum),
                   volume_fraction = sum(volume_fraction),
                   area_fraction = sum(area_fraction))]


global_beck <- global[, .(evap_sum = sum(evap_volume), area_sum = sum(area)), .(KG_beck)]
global_beck[, volume_fraction := evap_sum/sum(evap_sum)]  
global_beck[, area_fraction := round(100*area_sum/sum(area_sum),1)]  
global_beck[, area_sum := area_sum*M2_TO_KM2]
