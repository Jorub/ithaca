# Pairwise difference metrics ----
library(data.table)

source("source/partition_evap.R")

# Data ----
evap_mean_datasets <- as.data.table(readRDS(paste0(
  PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"
)))
evap_mean_datasets <- evap_mean_datasets[, .(lon, lat, dataset, evap_mean)]
evap_mean_datasets[, dataset := as.character(dataset)]
stopifnot(
  !anyNA(evap_mean_datasets$dataset),
  !anyDuplicated(evap_mean_datasets[, .(lon, lat, dataset)])
)

dataset_list <- evap_mean_datasets[, unique(dataset)]
n_datasets <- length(dataset_list)
stopifnot(n_datasets >= 2)

# Split and key once, instead of repeatedly scanning the full input.
dataset_data <- split(evap_mean_datasets, by = "dataset", keep.by = FALSE)
for (dataset_name in names(dataset_data)) {
  setkeyv(dataset_data[[dataset_name]], c("lon", "lat"))
}

# Calculate each product pair ----
n_pairs <- choose(n_datasets, 2)
pair_results <- vector("list", n_pairs)
pair_index <- 0L

for (i in seq_len(n_datasets - 1L)) {
  dummy1 <- dataset_data[[dataset_list[i]]]
  for (j in seq.int(i + 1L, n_datasets)) {
    pair_index <- pair_index + 1L
    message("Pair ", pair_index, "/", n_pairs, ": ",
            dataset_list[i], " vs ", dataset_list[j])
    dummy2 <- dataset_data[[dataset_list[j]]]
    dummy_data <- merge(
      dummy1, dummy2, by = c("lon", "lat"), sort = FALSE
    )
    dummy_result <- dummy_data[, .(
      lon, lat,
      sym_rel_diff = abs(evap_mean.x - evap_mean.y) /
        ((evap_mean.x + evap_mean.y) / 2),
      pair_mean_et = (evap_mean.x + evap_mean.y) / 2,
      et_delta = evap_mean.x - evap_mean.y,
      abs_et_delta = abs(evap_mean.x - evap_mean.y)
    )]
    dummy_result[, dataset.x := dataset_list[i]]
    dummy_result[, dataset.y := dataset_list[j]]
    pair_results[[pair_index]] <- dummy_result
  }
}

# Stack disjoint product-pair results once; no growing full-table merges.
message("Combining ", n_pairs, " product pairs")
data_out <- rbindlist(pair_results, use.names = TRUE)
rm(pair_results, dataset_data)

attr(data_out, "sym_rel_diff_definition") <- "cellwise_pair_mean"

# Save data ----
message("Writing ", nrow(data_out), " rows to the pairwise RDS file")
saveRDS(data_out, paste0(
  PATH_SAVE_PARTITION_EVAP, "product_pair_difference_gridwise.rds"
))
