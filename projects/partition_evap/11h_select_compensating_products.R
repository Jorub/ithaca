# Select retained product pairs with large grid-cell differences ----
library(data.table)

source("source/partition_evap.R")

# Configuration ----
wb_tolerance <- 0.10
difference_threshold <- 0.20
compensation_threshold <- 0

# Data ----
# These compact tables are produced by 11g; do not reload grid-cell data.
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

# Select all qualifying pairs within every prepared basin ----
# Difference is the area-weighted median of cellwise symmetric relative
# differences. Both cutoffs are strict: difference >20% and C >0.
compensating_pairs <- pair_summary[
  both_retained == TRUE &
    is.finite(median_sym_rel_diff) & median_sym_rel_diff > difference_threshold &
    is.finite(compensation) & compensation > compensation_threshold
]

# Include the individual basin errors so the retention criterion is visible.
errors_x <- product_status[, .(
  ma_basin, dataset.x = dataset, wb_mean_et_x = mean_evap,
  water_balance, wb_relative_error_x = mean_RE
)]
errors_y <- product_status[, .(
  ma_basin, dataset.y = dataset, wb_mean_et_y = mean_evap,
  wb_relative_error_y = mean_RE
)]
compensating_pairs <- merge(compensating_pairs, errors_x,
                            by = c("ma_basin", "dataset.x"), all.x = TRUE)
compensating_pairs <- merge(compensating_pairs, errors_y,
                            by = c("ma_basin", "dataset.y"), all.x = TRUE)
stopifnot(
  all(is.finite(compensating_pairs$wb_relative_error_x)),
  all(is.finite(compensating_pairs$wb_relative_error_y)),
  all(abs(compensating_pairs$wb_relative_error_x) <= wb_tolerance),
  all(abs(compensating_pairs$wb_relative_error_y) <= wb_tolerance)
)
setorder(compensating_pairs, ma_basin, -median_sym_rel_diff, -compensation,
         dataset.x, dataset.y)
attr(compensating_pairs, "selection_thresholds") <- list(
  wb_tolerance = wb_tolerance, difference = difference_threshold,
  compensation = compensation_threshold
)

# Save all selected rows, including gross/net differences and coverage ----
saveRDS(compensating_pairs, paste0(PATH_SAVE_PARTITION_EVAP,
                                   "basin_compensating_product_pairs.rds"))
dir.create(PATH_SAVE_PARTITION_EVAP_TABLES, recursive = TRUE, showWarnings = FALSE)
fwrite(compensating_pairs, paste0(PATH_SAVE_PARTITION_EVAP_TABLES,
                                  "basin_compensating_product_pairs.csv"))

print(compensating_pairs[, .(N_pairs = .N), .(ma_basin, basin_name)])
