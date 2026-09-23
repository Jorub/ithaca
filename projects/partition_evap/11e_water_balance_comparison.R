source('source/partition_evap.R')
source('source/partition_evap_graphics.R')
source('source/geo_functions.R')

diff_pp <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "product_pair_difference_gridwise.rds"))
basin_comparison <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "ma_basins_comparison.rds"))
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

basin_comparison[, threshold_10 := abs(mean_RE) < 0.1]
basin_comparison[, threshold_15 := abs(mean_RE) < 0.15]
basin_comparison[, threshold_20 := abs(mean_RE) < 0.2]

N_kept <- basin_comparison[, .(N10 = sum(threshold_10), N15 = sum(threshold_15), N20 = sum(threshold_20)), .(ma_basin)]

diff_basin <- merge(diff_pp, evap_mask[,.(lon, lat, ma_basin)], by = c("lon", "lat"))

diff_basin <- diff_basin[complete.cases(diff_basin)]

grid_cell_area <- unique(diff_basin[, .(lon, lat)]) %>% grid_area() # m2
diff_basin <- grid_cell_area[diff_basin, on = .(lon, lat)]

diff_basin_av <- diff_basin[, .(mean_basin_diff = sum(difference*area)/sum(area)), .(ma_basin)]

basin_list_10 <- basin_comparison[threshold_10 == TRUE, .(basins_kept = list(dataset)), .(ma_basin)]
basin_list_15 <- basin_comparison[threshold_15 == TRUE, .(basins_kept = list(dataset)), .(ma_basin)]
basin_list_20 <- basin_comparison[threshold_20 == TRUE, .(basins_kept = list(dataset)), .(ma_basin)]

get_datasets <- function(data, id) data[ma_basin == id, basins_kept][[1L]]

diff_basin_kept_10 <- diff_basin[, {
  kept <- get_datasets(basin_list_10, .BY$ma_basin)
  .SD[dataset.x %chin% kept & dataset.y %chin% kept]
}, by = ma_basin]

grid_cell_diff_10 <- diff_basin_kept_10[, .(mean_diff = mean(difference), 
                                            area = first(area),
                                            ma_basin = first(ma_basin)), .(lon, lat)]

grid_cell_diff_10[mean_diff > 0.20, diff_cat := "high"]
grid_cell_diff_10[mean_diff <= 0.20, diff_cat := "average"]
grid_cell_diff_10[mean_diff <= 0.1, diff_cat := "low"]

area_diff_10 <- grid_cell_diff_10[, .(area = sum(area)), .(diff_cat, ma_basin)]
area_diff_10[, area_fraction := area/sum(area), .(ma_basin)]

ggplot(area_diff_10)+
  geom_bar(aes(x = ma_basin, y = area_fraction, fill = diff_cat), stat = "identity")+
  theme_bw()

diff_basin_kept_15 <- diff_basin[, {
  kept <- get_datasets(basin_list_15, .BY$ma_basin)
  .SD[dataset.x %chin% kept & dataset.y %chin% kept]
}, by = ma_basin]

diff_basin_kept_20 <- diff_basin[, {
  kept <- get_datasets(basin_list_20, .BY$ma_basin)
  .SD[dataset.x %chin% kept & dataset.y %chin% kept]
}, by = ma_basin]


diff_basin_av_10 <- diff_basin[, {
  kept <- get_datasets(basin_list_10, .BY$ma_basin)
  use <- dataset.x %chin% kept & dataset.y %chin% kept
  .(mean_basin_diff_10 = if (any(use)) weighted.mean(difference[use], area[use], na.rm = TRUE) else NA_real_)
}, by = ma_basin]

diff_basin_av_15 <- diff_basin[, {
  kept <- get_datasets(basin_list_15, .BY$ma_basin)
  use <- dataset.x %chin% kept & dataset.y %chin% kept
  .(mean_basin_diff_15 = if (any(use)) weighted.mean(difference[use], area[use], na.rm = TRUE) else NA_real_)
}, by = ma_basin]

diff_basin_av_20 <- diff_basin[, {
  kept <- get_datasets(basin_list_20, .BY$ma_basin)
  use <- dataset.x %chin% kept & dataset.y %chin% kept
  .(mean_basin_diff_20 = if (any(use)) weighted.mean(difference[use], area[use], na.rm = TRUE) else NA_real_)
}, by = ma_basin]


diff_basin_avs <- merge(diff_basin_av, diff_basin_av_10, by = "ma_basin")
diff_basin_avs <- merge(diff_basin_avs, diff_basin_av_15, by = "ma_basin")
diff_basin_avs <- merge(diff_basin_avs, diff_basin_av_20, by = "ma_basin")

diff_basin_avs <- merge(diff_basin_avs, N_kept, by = "ma_basin")

ggplot(diff_basin_avs)+
  geom_point(aes(x = ma_basin, y = mean_basin_diff_10-mean_basin_diff, col = "10%"))+
  geom_point(aes(x = ma_basin, y = mean_basin_diff_15-mean_basin_diff, col = "15%"))+
  geom_point(aes(x = ma_basin, y = mean_basin_diff_20-mean_basin_diff, col = "20%"))+
  scale_color_manual(
    values = c(
      "all" = "black",
      "10%" = "darkred",
      "15%" = "darkorange",
      "20%" = "goldenrod"
    )
  )+
  geom_hline(yintercept = 0)+
  theme_bw()


ggplot(diff_basin_avs)+
  geom_point(aes(x = N10-0.1, y = mean_basin_diff_10-mean_basin_diff, col = "10%"))+
  geom_point(aes(x = N15, y = mean_basin_diff_15-mean_basin_diff, col = "15%"))+
  geom_point(aes(x = N20+0.1, y = mean_basin_diff_20-mean_basin_diff, col = "20%"))+
  
  scale_color_manual(
    values = c(
      "all" = "black",
      "10%" = "darkred",
      "15%" = "darkorange",
      "20%" = "goldenrod"
    )
  )+
  labs(
    x = "Number of retained products",
    y = "Change in mean pairwise grid-cell difference",
    colour = "Water-balance threshold"
  )+
  scale_y_continuous(labels = scales::label_percent())+
  geom_hline(yintercept = 0)+
  theme_bw()
  