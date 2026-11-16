# Select retained pairs with a large local-versus-basin difference gap ----
library(data.table)

source("source/partition_evap.R")

# Configuration ----
wb_tolerance <- 0.10
gap_threshold_pp <- 10
compensation_threshold <- 0.50

# Data ----
# Use the existing 11g summaries; neither grid data nor KS tests are rerun.
pair_summary <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "basin_product_pair_summary.rds"
)))
product_status <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "basin_pair_product_status.rds"
)))
stopifnot(
  isTRUE(all.equal(attr(pair_summary, "wb_tolerance"), wb_tolerance)),
  isTRUE(all.equal(attr(product_status, "wb_tolerance"), wb_tolerance))
)
pair_summary[, ma_basin := as.character(ma_basin)]
product_status[, ma_basin := as.character(ma_basin)]
stopifnot(
  !anyDuplicated(pair_summary[, .(ma_basin, dataset.x, dataset.y)]),
  !anyDuplicated(product_status[, .(ma_basin, dataset)])
)

# Compare the two difference metrics on the same support as compensation ----
pair_summary[, aggregated_sym_rel_diff := fifelse(
  is.finite(net_difference) & is.finite(pair_mean_et) & pair_mean_et > 0,
  net_difference / pair_mean_et, NA_real_
)]
pair_summary[, difference_gap_pp :=
               100 * (median_sym_rel_diff - aggregated_sym_rel_diff)]

# The gap is positive: local differences exceed the basin difference.
# Allow only machine-scale subtraction error at the inclusive 10 pp boundary.
gap_tolerance_pp <- 100 * .Machine$double.eps
selected_pairs <- pair_summary[
  both_retained == TRUE &
    is.finite(median_sym_rel_diff) & is.finite(aggregated_sym_rel_diff) &
    is.finite(difference_gap_pp) &
    difference_gap_pp >= gap_threshold_pp - gap_tolerance_pp &
    is.finite(compensation) & compensation > compensation_threshold
]

# Keep the individual water-balance errors visible in the selection table.
errors_x <- product_status[, .(
  ma_basin, dataset.x = dataset, wb_mean_et_x = mean_evap,
  water_balance, wb_relative_error_x = mean_RE
)]
errors_y <- product_status[, .(
  ma_basin, dataset.y = dataset, wb_mean_et_y = mean_evap,
  wb_relative_error_y = mean_RE
)]
selected_pairs <- merge(selected_pairs, errors_x,
                        by = c("ma_basin", "dataset.x"), all.x = TRUE)
selected_pairs <- merge(selected_pairs, errors_y,
                        by = c("ma_basin", "dataset.y"), all.x = TRUE)
stopifnot(
  all(is.finite(selected_pairs$wb_relative_error_x)),
  all(is.finite(selected_pairs$wb_relative_error_y)),
  all(abs(selected_pairs$wb_relative_error_x) <= wb_tolerance),
  all(abs(selected_pairs$wb_relative_error_y) <= wb_tolerance)
)
setorder(selected_pairs, -difference_gap_pp, -compensation,
         ma_basin, dataset.x, dataset.y)
attr(selected_pairs, "selection_thresholds") <- list(
  wb_tolerance = wb_tolerance, gap_pp = gap_threshold_pp,
  compensation = compensation_threshold
)

# Save every qualifying pair, preserving magnitude and coverage diagnostics ----
saveRDS(selected_pairs, paste0(PATH_SAVE_PARTITION_EVAP,
                               "basin_compensating_pairs_by_gap.rds"))
dir.create(PATH_SAVE_PARTITION_EVAP_TABLES, recursive = TRUE, showWarnings = FALSE)
fwrite(selected_pairs, paste0(PATH_SAVE_PARTITION_EVAP_TABLES,
                              "basin_compensating_pairs_by_gap.csv"))
print(selected_pairs[, .(N_pairs = .N), .(ma_basin, basin_name)])
