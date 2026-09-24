source('source/partition_evap.R')
source('source/partition_evap_graphics.R')
source('source/geo_functions.R')

diff_pp <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "product_pair_difference_gridwise.rds"))
basin_comparison <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "ma_basins_comparison.rds"))
agreement_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "dataset_agreement_grid_wise.rds"))


basin_comparison[, threshold_10 := abs(mean_RE) < 0.1]
basin_comparison[, threshold_15 := abs(mean_RE) < 0.15]
basin_comparison[, threshold_20 := abs(mean_RE) < 0.2]

N_kept <- basin_comparison[, .(N10 = sum(threshold_10), N15 = sum(threshold_15), N20 = sum(threshold_20)), .(ma_basin)]

diff_basin <- merge(diff_pp, agreement_grid[,.(lon, lat, ma_basin, rel_dataset_agreement, dist_dataset_agreement)], by = c("lon", "lat"))

diff_basin <- diff_basin[complete.cases(diff_basin)]

grid_cell_area <- unique(diff_basin[, .(lon, lat)]) %>% grid_area() # m2
diff_basin <- grid_cell_area[diff_basin, on = .(lon, lat)]

diff_basin_av <- diff_basin[, .(mean_basin_diff = sum(difference*area)/sum(area)), .(ma_basin)]

# Area fraction of magnitude agreement classes ----
diff_basin_rda <- diff_basin[, .(area = sum(area)), .(ma_basin, rel_dataset_agreement)]
diff_basin_rda[, area_fraction := area/sum(area), (ma_basin)]

# Area fraction of grid-scale differences classes ----
grid_cell_diff <- diff_basin[, .(mean_diff = median(difference), 
                                            area = first(area),
                                            ma_basin = first(ma_basin)), .(lon, lat)]

grid_cell_diff[mean_diff > 0.20, diff_cat := "High"]
grid_cell_diff[mean_diff <= 0.20, diff_cat := "Moderate"]
grid_cell_diff[mean_diff <= 0.1, diff_cat := "Low"]

grid_cell_diff[, diff_cat := factor(diff_cat, levels = c("Low", "Moderate", "High"))]

area_diff <- grid_cell_diff[, .(area = sum(area)), .(diff_cat, ma_basin)]
area_diff[, area_fraction := area/sum(area), .(ma_basin)]

# Area fraction of grid-scale differences classes with error different thresholds ----
basin_list_10 <- basin_comparison[threshold_10 == TRUE, .(basins_kept = list(dataset)), .(ma_basin)]
basin_list_15 <- basin_comparison[threshold_15 == TRUE, .(basins_kept = list(dataset)), .(ma_basin)]
basin_list_20 <- basin_comparison[threshold_20 == TRUE, .(basins_kept = list(dataset)), .(ma_basin)]

get_datasets <- function(data, id) data[ma_basin == id, basins_kept][[1L]]

diff_basin_kept_10 <- diff_basin[, {
  kept <- get_datasets(basin_list_10, .BY$ma_basin)
  .SD[dataset.x %chin% kept & dataset.y %chin% kept]
}, by = ma_basin]
grid_cell_diff_10[mean_diff > 0.20, diff_cat := "High"]
grid_cell_diff_10[mean_diff <= 0.20, diff_cat := "Moderate"]
grid_cell_diff_10[mean_diff <= 0.1, diff_cat := "Low"]

grid_cell_diff_10[, diff_cat := factor(diff_cat, levels = c("Low", "Moderate", "High"))]

area_diff_10 <- grid_cell_diff_10[, .(area = sum(area)), .(diff_cat, ma_basin)]
area_diff_10[, area_fraction := area/sum(area), .(ma_basin)]

all_basins <- sort(unique(area_diff$ma_basin))
missing_basins <- setdiff(all_basins, area_diff_10$ma_basin)

missing_dt <-   data.table(
  diff_cat = as.factor("One retained dataset"),
  ma_basin = as.factor(missing_basins),
  area = 1,
  area_fraction = 1)

area_diff_10_merge <- rbind(
  area_diff_10,
  missing_dt
)

grid_cell_diff_10 <- diff_basin_kept_10[, .(mean_diff = median(difference), 
                                            area = first(area),
                                            ma_basin = first(ma_basin)), .(lon, lat)]


diff_basin_kept_15 <- diff_basin[, {
  kept <- get_datasets(basin_list_15, .BY$ma_basin)
  .SD[dataset.x %chin% kept & dataset.y %chin% kept]
}, by = ma_basin]

grid_cell_diff_15 <- diff_basin_kept_15[, .(mean_diff = median(difference), 
                                            area = first(area),
                                            ma_basin = first(ma_basin)), .(lon, lat)]

grid_cell_diff_15[mean_diff > 0.20, diff_cat := "High"]
grid_cell_diff_15[mean_diff <= 0.20, diff_cat := "Moderate"]
grid_cell_diff_15[mean_diff <= 0.1, diff_cat := "Low"]

grid_cell_diff_15[, diff_cat := factor(diff_cat, levels = c("Low", "Moderate", "High"))]

area_diff_15 <- grid_cell_diff_15[, .(area = sum(area)), .(diff_cat, ma_basin)]
area_diff_15[, area_fraction := area/sum(area), .(ma_basin)]

diff_basin_kept_20 <- diff_basin[, {
  kept <- get_datasets(basin_list_20, .BY$ma_basin)
  .SD[dataset.x %chin% kept & dataset.y %chin% kept]
}, by = ma_basin]

# Grid scale difference change for 10 % threshold
grid_cell_comparison <- merge(grid_cell_diff, grid_cell_diff_10, by = c("lon", "lat", "area", "ma_basin")) 
grid_cell_comparison[, diff := mean_diff.y-mean_diff.x]
grid_cell_comparison[, diff_cat := cut(diff, breaks = c(-1, -0.1, -0.05, 0.05, 0.1, 1 ))]

area_comparison <- grid_cell_comparison[, .(area = sum(area)), .(ma_basin, diff_cat)]
area_comparison[, area_fraction := area/ sum(area), .(ma_basin)]

area_comparison_merge <- rbind(
  area_comparison,
  missing_dt
)

# Basin area-averaged differences ----

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

# figures ----
basin_levels <- area_diff_10_merge[
  diff_cat %in% c("High", "One retained dataset"),
  ma_basin[order(area_fraction)]
]

diff_basin_rda[, basin_plot := factor(ma_basin, levels = basin_levels)]
area_diff_10_merge[, basin_plot := factor(ma_basin, levels = basin_levels)]
area_comparison_merge[, basin_plot := factor(ma_basin, levels = basin_levels)]
N_kept[, basin_plot := factor(ma_basin, levels = basin_levels)]
basin_comparison[, basin_plot := factor(ma_basin, levels = basin_levels)]

fig_agreement <- ggplot(diff_basin_rda)+
  geom_bar(aes(x = basin_plot, y = area_fraction, fill = rel_dataset_agreement), 
           stat = "identity")+
  labs(y = "Area fraction",
       x = "Basin ID",
       fill = "Dataset agreement")+
  scale_fill_manual(values = color_agreement)+
  theme_bw()


change_cols <- c(
  "(-1,-0.1]"    = "#2166AC",
  "(-0.1,-0.05]" = "#67A9CF",
  "(-0.05,0.05]"    = "gray90",
  "(0.05,0.1]"   = "#EF8A62",
  "(0.1,1]"      = "#B2182B",
  "One retained dataset" = "gray20"
)
fig_area_diff <- ggplot(area_comparison_merge)+
  geom_bar(aes(x = basin_plot, y = area_fraction, fill = diff_cat), 
           stat = "identity")+
  labs(y = "Area fraction",
       x = "Basin ID",
       fill = "Difference classes")+
  scale_fill_manual(values = change_cols)+
  theme_bw()


fig_area_diff_10 <- ggplot(area_diff_10_merge)+
  geom_bar(aes(x = basin_plot, y = area_fraction, fill = diff_cat), 
           stat = "identity")+
  scale_fill_manual(
    values = c("Low" = "#4D648D",
                "Moderate" = "#F4CC70",
                "High" = "#A63A3A",
               "One retained dataset" = "gray20")
  )+
  labs(y = "Area fraction",
       x = "Basin ID",
       fill = "Difference classes")+
  theme_bw()


fig_N_kept <- ggplot(N_kept)+
  geom_bar(aes(x = basin_plot, y = N10, fill = "10%"), 
           stat = "identity")+
  scale_fill_manual(
    values = c(
      "all" = "black",
      "10%" = "darkred",
      "15%" = "darkorange",
      "20%" = "goldenrod"
    )
  )+
  labs(y = "Number of datasets",
       x = "Basin ID",
       fill = "Filter")+
  theme_bw()
  
dataset_order <- basin_comparison[
  , .(n_true = sum(threshold_10 == TRUE, na.rm = TRUE)), by = dataset
][order(-n_true, dataset), dataset]

basin_comparison[
  , dataset_plot := factor(dataset, levels = rev(dataset_order))
]

fig_datasets <- ggplot(basin_comparison)+
  geom_tile(aes(x = basin_plot, y = dataset_plot, fill = threshold_10))+
  scale_fill_manual(values = c("white", "gray20"))+
  labs(x = "Basin ID", y = "Datasets")+
  facet_grid(dataset_type ~ ., scales = "free_y", space = "free_y") +
  theme_bw()+
  theme(
    strip.text.y = element_text(angle = 0),
    panel.grid = element_blank()
  )

aligned <- cowplot::align_plots(
  fig_agreement,
  fig_area_diff_10,
  fig_area_diff,
  fig_N_kept,
  fig_datasets,
  align = "v",
  axis = "lr"
)

fig_combined <- ggarrange(
  plotlist = aligned,
  ncol = 1,
  nrow = 5,
  heights = c(1, 1, 1, 0.7, 2.4)
)

fig_combined

