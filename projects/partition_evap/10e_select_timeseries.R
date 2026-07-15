# Select the example time series used in Figure 1 ----
source("source/partition_evap.R")

save_rds_atomically <- function(object, output_file) {
  temporary_output <- paste0(output_file, ".part")
  on.exit(unlink(temporary_output), add = TRUE)
  saveRDS(object, temporary_output)

  if (!file.rename(temporary_output, output_file)) {
    stop("Could not move the completed RDS file to: ", output_file)
  }
}

agreement <- readRDS(
  file.path(PATH_SAVE_PARTITION_EVAP, "dataset_agreement_grid_wise.rds")
)

magnitude_high_candidates <- agreement[
  quant_range > 190 &
    quant_range < 210 &
    rel_dataset_agreement == "High"
]
magnitude_low_candidates <- agreement[
  quant_range > 160 &
    quant_range < 210 &
    rel_dataset_agreement == "Low" &
    dist_dataset_agreement == "High" &
    std_quant_range < 1
]
distribution_high_candidates <- agreement[
  dist_dataset_agreement == "High" &
    rel_dataset_agreement == "High" &
    evap_quant == "0.5-0.6"
]
distribution_low_candidates <- agreement[
  dist_dataset_agreement == "Low" &
    rel_dataset_agreement == "Low" &
    evap_quant == "0.2-0.3"
]

if (nrow(magnitude_high_candidates) < 599 ||
    nrow(magnitude_low_candidates) < 1 ||
    nrow(distribution_high_candidates) < 1 ||
    nrow(distribution_low_candidates) < 2) {
  stop("The Figure 1 example-location selections are no longer available.")
}

select_location <- function(data, row, example_label, selection_type) {
  location <- data.table::copy(data[row])
  location[, `:=`(
    example_label = example_label,
    selection_type = selection_type
  )]
  location
}

selected_locations <- data.table::rbindlist(
  list(
    select_location(magnitude_high_candidates, 599, "M1", "magnitude"),
    select_location(magnitude_low_candidates, 1, "M2", "magnitude"),
    select_location(distribution_high_candidates, 1, "D1", "distribution"),
    select_location(distribution_low_candidates, 2, "D2", "distribution")
  ),
  use.names = TRUE
)

if (uniqueN(selected_locations[, .(lon, lat)]) != 4) {
  stop("Figure 1 must contain four unique example locations.")
}

selected_locations <- selected_locations[
  ,
  .(
    lon,
    lat,
    example_label,
    selection_type,
    rel_dataset_agreement,
    dist_dataset_agreement,
    std_quant_range,
    quant_range,
    ens_mean_q25,
    ens_mean_mean,
    ens_mean_q75,
    evap_quant,
    match_ratio
  )
]

if (identical(RUN_LOCATION, "local")) {
  clean_data_path <- file.path(
    PATH_SAVE_PARTITION_EVAP,
    "datasets_clean_rds"
  )
  clean_data_files <- list.files(
    clean_data_path,
    pattern = "[.]rds$",
    full.names = TRUE
  )

  if (!length(clean_data_files)) {
    stop("No clean per-dataset files found. Run 01c_clean_data.R first.")
  }
} else {
  clean_data_files <- file.path(
    PATH_SAVE_PARTITION_EVAP,
    "evap_datasets_clean.rds"
  )
}

selected_coordinates <- selected_locations[, .(lon, lat)]
figure1_timeseries <- data.table::rbindlist(
  lapply(
    clean_data_files,
    function(clean_data_file) {
      clean_data <- readRDS(clean_data_file)
      clean_data[
        selected_coordinates,
        on = .(lon, lat),
        nomatch = 0
      ]
    }
  ),
  use.names = TRUE
)

figure1_timeseries[, year := as.numeric(as.character(year))]
figure1_timeseries <- merge(
  figure1_timeseries,
  selected_locations,
  by = c("lon", "lat"),
  all.x = TRUE
)
data.table::setorder(figure1_timeseries, example_label, dataset, year)

missing_locations <- setdiff(
  selected_locations$example_label,
  unique(figure1_timeseries$example_label)
)
if (length(missing_locations)) {
  stop(
    "No clean time series found for Figure 1 location(s): ",
    paste(missing_locations, collapse = ", ")
  )
}

save_rds_atomically(
  selected_locations,
  file.path(PATH_SAVE_PARTITION_EVAP, "figure1_selected_locations.rds")
)
save_rds_atomically(
  figure1_timeseries,
  file.path(PATH_SAVE_PARTITION_EVAP, "figure1_timeseries.rds")
)
