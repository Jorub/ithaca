# Overview figure agreement across ma basins ----
source('source/partition_evap.R')
source('source/partition_evap_graphics.R')
source('source/graphics.R')
source('source/mask_paths.R')

library(rnaturalearth)
library(dplyr)

## data ----
fname_shape <- list.files(path = PATH_MASKS_MA_BASINS, full.names = TRUE, pattern = "*Boundary_56.shp")
ma_sf <- read_sf(fname_shape[1])
data_ma_time <- read.table(paste0(PATH_MASK, "ma_et_al_water_balance/ETwb_56_ma_et_al.csv"), dec =',', sep =';', header = T)
data_ma_time <- data_ma_time[18:34,]
data_ma_time_long <- melt(as.data.table(data_ma_time), 
                          id.vars = 'Year', value.name = 'evap', variable.name = 'ma_basin')
data_ma_time_long[, ma_basin_chr := as.character(ma_basin)]
data_ma_time_long[, ma_basin := sapply(strsplit(ma_basin_chr, split = '_'), "[[", 2)]
data_ma_time_long[, ma_basin := as.factor(ma_basin)]
data_ma_time_long[, ma_basin_chr := NULL]
data_ma_time_long[, dataset := 'Ma']

data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_ma_basin.rds"))
data[, year := as.numeric(as.character(year))]
data[, environment_volume := NULL]
data[, area_sum := NULL]
data <- data[year < 2017]
data_merged <- merge(data, data_ma_time_long, by.x = c('year', 'ma_basin', 'evap_mean', 'dataset'), by.y = c('Year', 'ma_basin', 'evap', 'dataset'), all = T)

saveRDS(data_merged, paste0(PATH_SAVE_PARTITION_EVAP, "time_series_ma_basins_merged.rds"))

agreement_summary <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "high_agreement_ma_basins_area.rds"))

# Analysis ----
### assign data type
data_merged[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote sensing"]
data_merged[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
data_merged[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydr./LSM model"]
data_merged[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Composite"]
data_merged[dataset %in% 'Ma', dataset_type := "Water balance"]

data_merged[, ma_basin := as.numeric(as.character(ma_basin))]

### assign continent

data_merged[ma_basin < 21, continent := 'North America']
data_merged[ma_basin > 20 & ma_basin < 27, continent := 'S. America']
data_merged[ma_basin > 26 & ma_basin < 33, continent := 'Europe']
data_merged[ma_basin > 32 & ma_basin < 47, continent := 'Asia']
data_merged[ma_basin > 46 & ma_basin < 52, continent := 'Africa']
data_merged[ma_basin > 51, continent := 'Oceania']

data_merged[ma_basin < 21, continent_abr := 'NAm']
data_merged[ma_basin > 20 & ma_basin < 27, continent_abr := 'SAm']
data_merged[ma_basin > 26 & ma_basin < 33, continent_abr := 'Eur']
data_merged[ma_basin > 32 & ma_basin < 47, continent_abr:= 'Asia']
data_merged[ma_basin > 46 & ma_basin < 52, continent_abr := 'Afr']
data_merged[ma_basin > 51, continent_abr := 'Oce']

data_merged[, ma_basin := as.factor(ma_basin)]

## calculate means and compare to water balance ----

data_mean <- data_merged[,.(mean_evap = mean(evap_mean), 
                            dataset_type = first(dataset_type), 
                            continent = first(continent),
                            continent_abr = first(continent_abr)), 
                         .(dataset, ma_basin)]

data_mean[dataset == 'Ma', water_balance := mean_evap, .(ma_basin)]
data_mean[, water_balance := mean(water_balance, na.rm = T), .(ma_basin)]

# Keep Ma as the reference column, not as a member of the product ensemble.
data_mean <- data_mean[dataset != "Ma"]

data_stats <- data_mean[, .(
  Q25_evap = quantile(mean_evap, 0.25),
  Q75_evap = quantile(mean_evap, 0.75),
  ens_mean = mean(mean_evap),
  continent = first(continent)),
  .(ma_basin)]

data_stats[, IQR := Q75_evap - Q25_evap, .(ma_basin)]
data_stats[, sIQR := IQR/ens_mean]

data_stats <- merge(data_stats, agreement_summary, by = "ma_basin")

data_mean[, mean_RE := (mean_evap-water_balance)/water_balance]
data_mean[mean_RE < 0, diff := 'under']
data_mean[mean_RE > 0, diff := 'over']
data_mean <- data_mean[!is.na(mean_RE)]

plot_dt <- merge(data_mean, agreement_summary, by = "ma_basin", all.x = TRUE)

x_scale <- if (max(plot_dt$joint_da_higher, na.rm = TRUE) <= 1) 100 else 1
plot_dt[, joint_high_pct := joint_da_higher * x_scale]
plot_dt <- merge(plot_dt, data_stats[,.(sIQR, ma_basin)], by = "ma_basin")

saveRDS(plot_dt, paste0(PATH_SAVE_PARTITION_EVAP, "ma_basins_comparison.rds"))

basin_sum <- plot_dt[, .(
  mean_RE_min = min(mean_RE, na.rm = TRUE),
  mean_RE_q25 = quantile(mean_RE, 0.25, na.rm = TRUE),
  mean_RE_med = median(mean_RE, na.rm = TRUE),
  mean_RE_mean = mean(mean_RE, na.rm = TRUE),
  mean_RE_q75 = quantile(mean_RE, 0.75, na.rm = TRUE),
  mean_RE_max = max(mean_RE, na.rm = TRUE),
  joint_high_pct = first(joint_high_pct),
  sIQR = first(sIQR)
), by = .(ma_basin)]

basin_sum[, mean_RE_basin := fifelse(
  mean_RE_q75 < 0, "Below WB",
  fifelse(mean_RE_q25 > 0, "Above WB",
          "Crosses WB")
)]

ma_sf <- ma_sf %>%
  mutate(BasinID = as.character(BasinID)) %>%
  left_join(
    basin_sum,
    by = c("BasinID" = "ma_basin")
  ) %>%
  mutate(
    mean_RE = factor(
      mean_RE_basin,
      levels = names(color_ref)
    )
  )

saveRDS(ma_sf, paste0(PATH_SAVE_PARTITION_EVAP, "ma_basins_spatial.rds"))



