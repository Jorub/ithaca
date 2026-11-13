# Godavari: GLDAS-VIC and TerraClimate pairwise ET differences ----
library(data.table)
library(sf)

source("source/partition_evap.R")
source("source/partition_evap_graphics.R")

# Data ----
diff_pp <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "product_pair_difference_gridwise.rds"
)))
agreement_grid <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "dataset_agreement_grid_wise.rds"
)))
basin_comparison <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "ma_basins_comparison.rds"
)))
ma_sf <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "ma_basins_spatial.rds"))

stopifnot(identical(attr(diff_pp, "sym_rel_diff_definition"),
                    "cellwise_pair_mean"))

# Godavari and product pair ----
ma_sf$BasinID <- as.character(ma_sf$BasinID)
godavari <- ma_sf[grepl("godavari", ma_sf$Name, ignore.case = TRUE), ]
stopifnot(nrow(godavari) == 1)

grid <- agreement_grid[
  as.character(ma_basin) == godavari$BasinID & is.finite(area) & area > 0,
  .(lon, lat, area)
]
pair <- diff_pp[
  dataset.x == "GLDAS-VIC" & dataset.y == "TerraClimate",
  .(lon, lat, sym_rel_diff, et_delta, pair_mean_et)
]
pair <- merge(pair, grid, by = c("lon", "lat"))
stopifnot(nrow(pair) > 0, !anyDuplicated(pair[, .(lon, lat)]))

# Diagnostics ----
# et_delta = GLDAS-VIC - TerraClimate; pair_mean_et = their cellwise mean.
valid <- pair[is.finite(et_delta) & is.finite(pair_mean_et)]
stopifnot(nrow(valid) > 0)

pair_summary <- valid[, .(
  median_relative_difference = weighted_median(sym_rel_diff, area),
  gross_difference = weighted.mean(abs(et_delta), area),
  net_difference = weighted.mean(et_delta, area),
  positive_area_fraction = weighted.mean(et_delta > 0, area),
  negative_area_fraction = weighted.mean(et_delta < 0, area),
  vic_mean = weighted.mean(pair_mean_et + et_delta / 2, area),
  terraclimate_mean = weighted.mean(pair_mean_et - et_delta / 2, area),
  valid_area_fraction = sum(area) / sum(grid$area)
)]
pair_summary[, compensation := fifelse(
  gross_difference > 0,
  1 - abs(net_difference) / gross_difference, NA_real_
)]

basin_comparison[, ma_basin := as.character(ma_basin)]
wb_summary <- basin_comparison[
  ma_basin == godavari$BasinID &
    dataset %in% c("GLDAS-VIC", "TerraClimate"),
  .(dataset, mean_evap, water_balance,
    mean_RE = (mean_evap - water_balance) / water_balance,
    retained = abs((mean_evap - water_balance) / water_balance) <= 0.1)
]

cat("\nGodavari: pairwise diagnostics\n")
print(pair_summary)
cat("\nGodavari: water-balance filtering inputs\n")
print(wb_summary)
cat("\nReconstructed minus filtering basin means\n")
print(data.table(
  dataset = c("GLDAS-VIC", "TerraClimate"),
  reconstructed = c(pair_summary$vic_mean, pair_summary$terraclimate_mean)
)[wb_summary, on = "dataset"][, .(
  dataset, reconstructed, filtering_mean = mean_evap,
  difference = reconstructed - mean_evap
)])

# Maps ----
map_theme <- theme_map_fig5 + theme(legend.position = "right")

fig_relative <- ggplot() +
  geom_tile(data = pair[is.finite(sym_rel_diff)],
            aes(lon, lat, fill = sym_rel_diff),
            width = 0.25, height = 0.25) +
  geom_sf(data = godavari, fill = NA, colour = "gray20", linewidth = 0.5) +
  scale_fill_gradient(
    low = "#F7FBFF", high = "#08306B",
    labels = scales::label_percent(accuracy = 1),
    name = "Symmetric relative\ndifference"
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "a  Symmetric relative difference", x = NULL, y = NULL) +
  map_theme

fig_signed <- ggplot() +
  geom_tile(data = pair[is.finite(et_delta)],
            aes(lon, lat, fill = et_delta),
            width = 0.25, height = 0.25) +
  geom_sf(data = godavari, fill = NA, colour = "gray20", linewidth = 0.5) +
  scale_fill_gradient2(
    low = fig5_col_low, mid = fig5_col_mid, high = fig5_col_high,
    midpoint = 0, name = "ET difference\n(GLDAS-VIC − TerraClimate)"
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "b  Signed ET difference", x = NULL, y = NULL) +
  map_theme

fig_godavari <- ggarrange(fig_relative, fig_signed, ncol = 1, nrow = 2,
                          align = "v")

# Save ----
ggsave(
  paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
         "main/godavari_gldas_vic_terraclimate_difference.pdf"),
  fig_godavari, width = 17, height = 23, units = "cm",
  device = cairo_pdf, bg = "white"
)
ggsave(
  paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
         "main/godavari_gldas_vic_terraclimate_difference.png"),
  fig_godavari, width = 17, height = 23, units = "cm",
  dpi = 300, bg = "white"
)
