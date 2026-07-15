# Clean evaporation datasets ----
# Retain complete time series at locations covered by at least 13 datasets.
source("source/partition_evap.R")
source("source/geo_functions.R")

## path ----

PATH_SAVE_PARTITION_EVAP_DATASETS_RDS <- file.path(
  PATH_SAVE_PARTITION_EVAP,
  "datasets_rds"
)
PATH_SAVE_PARTITION_EVAP_DATASETS_CLEAN_RDS <- file.path(
  PATH_SAVE_PARTITION_EVAP,
  "datasets_clean_rds"
)

## fucntions -----
save_rds_atomically <- function(object, output_file) {
  temporary_output <- paste0(output_file, ".part")
  on.exit(unlink(temporary_output), add = TRUE)
  saveRDS(object, temporary_output)

  if (!file.rename(temporary_output, output_file)) {
    stop("Could not move the completed RDS file to: ", output_file)
  }
}

clean_local_dataset <- function(dataset_file,
                                eligible_locations,
                                grid_cell_area,
                                output_folder =
                                  PATH_SAVE_PARTITION_EVAP_DATASETS_CLEAN_RDS) {
  dataset_name <- tools::file_path_sans_ext(basename(dataset_file))
  output_name <- if (dataset_name == "etsynthesis") {
    "synthesizedet"
  } else {
    dataset_name
  }
  output_file <- file.path(output_folder, paste0(output_name, ".rds"))

  if (file.exists(output_file)) {
    message("Clean dataset already present, skipping: ", output_name)
    return(FALSE)
  }

  message("Cleaning: ", dataset_name)
  evap_dataset <- readRDS(dataset_file)

  evap_dataset[, year_count := .N, .(dataset, lon, lat)]
  evap_dataset <- evap_dataset[year_count == 20]
  evap_dataset <- evap_dataset[
    eligible_locations,
    on = .(lon, lat),
    nomatch = 0
  ]
  evap_dataset <- evap_dataset[
    !(dataset == "etmonitor" & year == "2000")
  ]
  evap_dataset[, year_count := NULL]

  evap_dataset <- grid_cell_area[
    evap_dataset,
    on = .(lon, lat),
    nomatch = 0
  ]
  evap_dataset[, evap_volume := evap * M2_TO_KM2 * MM_TO_KM * area]
  evap_dataset[dataset == "etsynthesis", dataset := "synthesizedet"]

  save_rds_atomically(evap_dataset, output_file)
  rm(evap_dataset)
  gc()
  TRUE
}

## clean data -----
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

  coverage_file <- file.path(PATH_SAVE_PARTITION_EVAP, "evap_data_count.rds")
  if (!file.exists(coverage_file)) {
    stop("Coverage data are missing. Run 01_d_dataset_coverage.R first.")
  }

  evap_data_count <- readRDS(coverage_file)
  eligible_locations <- evap_data_count[
    data_count >= MIN_N_DATASETS,
    .(lon, lat)
  ]
  grid_cell_area <- grid_area(data.table::copy(eligible_locations))

  dir.create(PATH_SAVE_PARTITION_EVAP_DATASETS_CLEAN_RDS,
             recursive = TRUE,
             showWarnings = FALSE)

  cleaned <- vapply(
    dataset_files,
    clean_local_dataset,
    logical(1),
    eligible_locations = eligible_locations,
    grid_cell_area = grid_cell_area
  )

  message(
    "Local cleaning complete: ", sum(cleaned), " cleaned, ",
    sum(!cleaned), " already present."
  )
} else {
  # Original server workflow: retain one combined clean dataset and summaries.
  load("~/shared/data_projects/ithaca/misc/evap_fnames_2000_2019_full_record.Rdata")

  evap_datasets <- readRDS(
    paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets.rds")
  )
  evap_datasets[, year_count := .N, .(dataset, lon, lat)]
  evap_datasets <- evap_datasets[year_count == 20]

  evap_datasets[, data_count := .N, .(lon, lat, year)]
  evap_datasets <- evap_datasets[data_count >= 13]
  evap_datasets <- evap_datasets[
    !(dataset == "etmonitor" & year == "2000")
  ]

  evap_datasets[, data_count := NULL]
  evap_datasets[, year_count := NULL]
  grid_cell_area <- unique(evap_datasets[, .(lon, lat)]) |>
    grid_area()
  evap_datasets <- grid_cell_area[evap_datasets, on = .(lon, lat)]
  evap_datasets[, evap_volume := evap * M2_TO_KM2 * MM_TO_KM * area]
  evap_datasets[dataset == "etsynthesis", dataset := "synthesizedet"]
  evap_datasets <- evap_datasets[order(dataset)]
  saveRDS(
    evap_datasets,
    paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_clean.rds")
  )

  evap_datasets[, area_weights := area / sum(area), .(dataset, year)]
  evap_datasets[, all_area_weights := sum(area_weights), .(dataset, year)]

  evap_datasets_global_annual_mean <- evap_datasets[
    ,
    .(evap_annual_mean = sum(area_weights * evap)),
    .(dataset, year)
  ]
  evap_datasets_global_annual_mean[
    ,
    year := as.numeric(as.character(year))
  ]

  total_area <- evap_datasets[
    ,
    .(total_area = sum(area)),
    .(dataset, year)
  ]
  total_area[, year := as.numeric(as.character(year))]
  evap_datasets_global_annual_mean <- total_area[
    evap_datasets_global_annual_mean,
    on = .(dataset, year)
  ]
  evap_datasets_global_annual_mean[
    ,
    evap_volume := evap_annual_mean * M2_TO_KM2 * MM_TO_KM * total_area
  ]

  saveRDS(
    evap_datasets_global_annual_mean,
    paste0(PATH_SAVE_PARTITION_EVAP, "global_annual_means.rds")
  )
  write.table(
    evap_datasets_global_annual_mean,
    paste0(
      PATH_SAVE_PARTITION_EVAP_TABLES,
      "interannual_variance_global.csv"
    ),
    row.names = FALSE,
    sep = ","
  )

  evap_datasets_grid_mean <- evap_datasets[
    ,
    .(evap_mean = mean(evap), evap_sd = sd(evap)),
    .(lon, lat, dataset)
  ]
  evap_datasets_grid_mean <- grid_cell_area[
    evap_datasets_grid_mean,
    on = .(lon, lat)
  ]
  evap_datasets_grid_mean[
    ,
    evap_volume := evap_mean * M2_TO_KM2 * MM_TO_KM * area
  ]
  saveRDS(
    evap_datasets_grid_mean,
    paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds")
  )
}
