# Create dataset coverage information ----
source("source/partition_evap.R")

PATH_SAVE_PARTITION_EVAP_DATASETS_RDS <- file.path(
  PATH_SAVE_PARTITION_EVAP,
  "datasets_rds"
)
PATH_SAVE_PARTITION_EVAP_COVERAGE_RDS <- file.path(
  PATH_SAVE_PARTITION_EVAP,
  "dataset_coverage_rds"
)

save_rds_atomically <- function(object, output_file) {
  temporary_output <- paste0(output_file, ".part")
  on.exit(unlink(temporary_output), add = TRUE)
  saveRDS(object, temporary_output)

  if (!file.rename(temporary_output, output_file)) {
    stop("Could not move the completed RDS file to: ", output_file)
  }
}

create_dataset_coverage <- function(dataset_file,
                                    output_folder =
                                      PATH_SAVE_PARTITION_EVAP_COVERAGE_RDS) {
  output_file <- file.path(output_folder, basename(dataset_file))

  if (file.exists(output_file)) {
    message("Coverage already created, skipping: ", basename(dataset_file))
    return(FALSE)
  }

  message("Creating coverage: ", basename(dataset_file))
  evap_dataset <- readRDS(dataset_file)

  evap_dataset[, year_count := .N, .(dataset, lon, lat)]
  evap_dataset <- evap_dataset[year_count == 20]
  evap_dataset <- evap_dataset[
    !(dataset == "etmonitor" & year == "2000")
  ]
  evap_dataset[dataset == "etsynthesis", dataset := "synthesizedet"]

  dataset_coverage <- unique(
    evap_dataset[, .(lon, lat, dataset)]
  )

  save_rds_atomically(dataset_coverage, output_file)
  rm(evap_dataset, dataset_coverage)
  gc()
  TRUE
}

dir.create(PATH_SAVE_PARTITION_EVAP_COVERAGE_RDS,
           recursive = TRUE,
           showWarnings = FALSE)

if (identical(RUN_LOCATION, "local")) {
  dataset_files <- list.files(
    PATH_SAVE_PARTITION_EVAP_DATASETS_RDS,
    pattern = "[.]rds$",
    full.names = TRUE
  )

  if (!length(dataset_files)) {
    stop("No per-dataset RDS files found in: ",
         PATH_SAVE_PARTITION_EVAP_DATASETS_RDS)
  }

  coverage_created <- vapply(
    dataset_files,
    create_dataset_coverage,
    logical(1)
  )

  coverage_files <- list.files(
    PATH_SAVE_PARTITION_EVAP_COVERAGE_RDS,
    pattern = "[.]rds$",
    full.names = TRUE
  )
  evap_data_count_coverage <- data.table::rbindlist(
    lapply(coverage_files, readRDS),
    use.names = TRUE
  )

  message(
    "Coverage creation complete: ", sum(coverage_created), " created, ",
    sum(!coverage_created), " already present."
  )
} else {
  evap_datasets <- readRDS(
    paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets.rds")
  )

  evap_datasets[, year_count := .N, .(dataset, lon, lat)]
  evap_datasets <- evap_datasets[year_count == 20]
  evap_datasets <- evap_datasets[
    !(dataset == "etmonitor" & year == "2000")
  ]
  evap_datasets[dataset == "etsynthesis", dataset := "synthesizedet"]

  evap_data_count_coverage <- unique(
    evap_datasets[, .(lon, lat, dataset)]
  )
  rm(evap_datasets)
  gc()
}

evap_data_count_coverage[, data_count := .N, .(lon, lat)]
data.table::setcolorder(
  evap_data_count_coverage,
  c("lon", "lat", "data_count", "dataset")
)
data.table::setorder(evap_data_count_coverage, dataset, lat, lon)

evap_data_count <- unique(
  evap_data_count_coverage[, .(lon, lat, data_count)]
)
data.table::setorder(evap_data_count, lat, lon)

save_rds_atomically(
  evap_data_count,
  file.path(PATH_SAVE_PARTITION_EVAP, "evap_data_count.rds")
)

save_rds_atomically(
  evap_data_count_coverage,
  file.path(PATH_SAVE_PARTITION_EVAP, "evap_data_count_coverage.rds")
)
