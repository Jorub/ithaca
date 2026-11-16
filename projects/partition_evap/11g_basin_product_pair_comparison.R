# Product-pair differences and spatial compensation within selected basins ----
library(data.table)
library(sf)

source("source/partition_evap.R")

# Configuration ----
case_basin_ids <- character(0) # All basins; specify IDs here to select a subset.
wb_tolerance <- 0.10
large_difference_threshold <- 0.20

# Data ----
basin_comparison <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "ma_basins_comparison.rds"
)))
agreement_grid <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "dataset_agreement_grid_wise.rds"
)))
ma_sf <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "ma_basins_spatial.rds"))
basin_names <- as.data.table(st_drop_geometry(ma_sf))[
  , .(ma_basin = as.character(BasinID), basin_name = Name)
]
if (!length(case_basin_ids)) case_basin_ids <- basin_names$ma_basin
stopifnot(all(case_basin_ids %in% basin_names$ma_basin))

basin_comparison[, ma_basin := as.character(ma_basin)]
agreement_grid[, ma_basin := as.character(ma_basin)]
product_status <- basin_comparison[
  ma_basin %in% case_basin_ids & dataset != "Ma" &
    is.finite(mean_evap) & is.finite(water_balance) & water_balance > 0,
  .(ma_basin, dataset = as.character(dataset), dataset_type,
    mean_evap, water_balance)
]
product_status[, mean_RE := (mean_evap - water_balance) / water_balance]
product_status[, retained := abs(mean_RE) <= wb_tolerance]
product_status <- merge(product_status, basin_names, by = "ma_basin",
                        all.x = TRUE)
stopifnot(
  !anyDuplicated(product_status[, .(ma_basin, dataset)]),
  all(case_basin_ids %in% product_status$ma_basin),
  !anyDuplicated(agreement_grid[, .(lon, lat)])
)
grid_lookup <- agreement_grid[
  ma_basin %in% case_basin_ids & is.finite(area) & area > 0,
  .(lon, lat, ma_basin, area)
]
basin_area <- grid_lookup[, .(basin_area = sum(area)), .(ma_basin)]

# Restrict the pairwise data to the selected basins before summarising ----
diff_pp <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "product_pair_difference_gridwise.rds"
)))
if (!identical(attr(diff_pp, "sym_rel_diff_definition"), "cellwise_pair_mean") ||
    !all(c("evap_mean.x", "evap_mean.y") %in% names(diff_pp))) {
  stop("Regenerate product_pair_difference_gridwise.rds with the current 07f script.")
}
pair_grid <- diff_pp[grid_lookup, on = .(lon, lat), nomatch = 0, .(
  lon, lat, ma_basin = i.ma_basin, area = i.area,
  dataset.x, dataset.y, evap_mean.x, evap_mean.y,
  sym_rel_diff, et_delta, abs_et_delta, pair_mean_et
)]
rm(diff_pp)
stopifnot(!anyDuplicated(pair_grid[
  , .(ma_basin, lon, lat, dataset.x, dataset.y)
]))

retention <- product_status[, .(ma_basin, dataset, retained)]
pair_grid <- merge(
  pair_grid, retention,
  by.x = c("ma_basin", "dataset.x"), by.y = c("ma_basin", "dataset")
)
setnames(pair_grid, "retained", "retained_x")
pair_grid <- merge(
  pair_grid, retention,
  by.x = c("ma_basin", "dataset.y"), by.y = c("ma_basin", "dataset")
)
setnames(pair_grid, "retained", "retained_y")

# Every pair remains separate; do not take a median across product pairs.
# Grid means use the full available period; water-balance retention uses the
# existing 2000-2016 comparison, with incomplete ETmonitor 2000 excluded.
pair_summary <- pair_grid[
  is.finite(evap_mean.x) & is.finite(evap_mean.y) &
    is.finite(et_delta) & is.finite(abs_et_delta) & is.finite(pair_mean_et),
  .(
    retained_x = first(retained_x), retained_y = first(retained_y),
    median_sym_rel_diff = weighted_median(sym_rel_diff, area),
    # Denominator is the area with a defined relative difference for this pair.
    large_difference_fraction = if (any(is.finite(sym_rel_diff))) {
      weighted.mean(sym_rel_diff[is.finite(sym_rel_diff)] >
                      large_difference_threshold, area[is.finite(sym_rel_diff)])
    } else NA_real_,
    gross_difference = weighted.mean(abs_et_delta, area),
    signed_net_difference = weighted.mean(et_delta, area),
    pair_mean_et = weighted.mean(pair_mean_et, area),
    mean_et_x = weighted.mean(evap_mean.x, area),
    mean_et_y = weighted.mean(evap_mean.y, area),
    positive_area_fraction = weighted.mean(et_delta > 0, area),
    negative_area_fraction = weighted.mean(et_delta < 0, area),
    pair_area = sum(area),
    difference_area = sum(area[is.finite(sym_rel_diff)]), N_cells = .N
  ), .(ma_basin, dataset.x, dataset.y)
]
pair_summary[, net_difference := abs(signed_net_difference)]
pair_summary[, compensation := fifelse(
  gross_difference > 0, 1 - net_difference / gross_difference, NA_real_
)]
pair_summary[, gross_relative_difference := fifelse(
  pair_mean_et > 0, gross_difference / pair_mean_et, NA_real_
)]
pair_summary <- merge(pair_summary, basin_area, by = "ma_basin", all.x = TRUE)
pair_summary <- merge(pair_summary, basin_names, by = "ma_basin", all.x = TRUE)
pair_summary[, `:=`(
  both_retained = retained_x & retained_y,
  pair_area_fraction = pair_area / basin_area,
  difference_area_fraction = difference_area / basin_area
)]
setorder(pair_summary, ma_basin, dataset.x, dataset.y)
attr(pair_summary, "wb_tolerance") <- wb_tolerance
attr(pair_summary, "large_difference_threshold") <- large_difference_threshold
attr(product_status, "wb_tolerance") <- wb_tolerance

# Save compact tables; plotting does not reload the global pairwise data ----
saveRDS(pair_summary, paste0(PATH_SAVE_PARTITION_EVAP,
                             "basin_product_pair_summary.rds"))
saveRDS(product_status, paste0(PATH_SAVE_PARTITION_EVAP,
                               "basin_pair_product_status.rds"))
