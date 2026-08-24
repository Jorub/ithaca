
source('source/evap_trend.R')

global_annual_means <- readRDS("~/shared/data_projects/ithaca/evap_trend/global_annual_means.rds")
global_annual_means[, mean(evap_annual_mean), .(dataset)]
evap_annual_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_annual_trend_bootstrap.rds"))  
evap_annual_trend <- evap_annual_trend[,1:5]

evap_global_data <- merge(evap_annual_trend, global_annual_means[, .(evap_mean = mean(evap_annual_mean)), .(dataset)], by = "dataset")

evap_global_data[,2:6] <- round(evap_global_data[,2:6],2)

evap_global_data[, evap_mean := round(evap_mean)]

write.table(evap_global_data, file = paste0(PATH_SAVE_EVAP_TREND, "evap_global_data.csv"),
          col.names = TRUE, row.names = FALSE, sep = " ")
