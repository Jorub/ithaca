# Convert each NetCDF dataset to a separate data table ----
source("source/partition_evap.R")

PATH_SAVE_PARTITION_EVAP_DATASETS_RDS <- file.path(
  PATH_SAVE_PARTITION_EVAP,
  "datasets_rds"
)

netcdf_to_rds <- function(netcdf_file,
                          output_folder = PATH_SAVE_PARTITION_EVAP_DATASETS_RDS) {
  dataset <- sub("_e_mm_.*$", "", basename(netcdf_file))
  output_file <- file.path(output_folder, paste0(dataset, ".rds"))

  if (file.exists(output_file)) {
    message("Already converted, skipping: ", dataset)
    return(FALSE)
  }

  message("Converting: ", dataset)
  evap_brick <- raster::brick(netcdf_file)
  layer_dates <- raster::getZ(evap_brick)

  if (is.null(layer_dates) || length(layer_dates) != raster::nlayers(evap_brick)) {
    stop("Missing or invalid time metadata in: ", netcdf_file)
  }

  layer_years <- as.integer(format(as.Date(layer_dates), "%Y"))
  layer_tables <- vector("list", raster::nlayers(evap_brick))

  for (layer_index in seq_len(raster::nlayers(evap_brick))) {
    layer_table <- data.table::as.data.table(
      as.data.frame(
        evap_brick[[layer_index]],
        xy = TRUE,
        na.rm = TRUE
      )
    )

    data.table::setnames(
      layer_table,
      names(layer_table)[1:3],
      c("lon", "lat", "evap")
    )
    layer_table[, year := layer_years[[layer_index]]]
    layer_tables[[layer_index]] <- layer_table
  }

  evap_dataset <- data.table::rbindlist(layer_tables, use.names = TRUE)
  rm(layer_tables)

  evap_dataset[, dataset := dataset]
  evap_dataset[, year := factor(year)]
  data.table::setcolorder(
    evap_dataset,
    c("lon", "lat", "year", "dataset", "evap")
  )

  temporary_output <- paste0(output_file, ".part")
  on.exit(unlink(temporary_output), add = TRUE)
  saveRDS(evap_dataset, temporary_output)

  if (!file.rename(temporary_output, output_file)) {
    stop("Could not move the completed RDS file to: ", output_file)
  }

  rm(evap_dataset, evap_brick)
  gc()
  TRUE
}

if (identical(RUN_LOCATION, "local")) {
  dir.create(PATH_SAVE_PARTITION_EVAP_DATASETS_RDS,
             recursive = TRUE,
             showWarnings = FALSE)

  netcdf_files <- list.files(
    PATH_SAVE_PARTITION_EVAP_RAW,
    pattern = "[.]nc$",
    full.names = TRUE
  )

  if (!length(netcdf_files)) {
    stop("No NetCDF files found in: ", PATH_SAVE_PARTITION_EVAP_RAW)
  }

  converted <- vapply(netcdf_files, netcdf_to_rds, logical(1))

  message(
    "Conversion complete: ", sum(converted), " converted, ",
    sum(!converted), " already present."
  )
} else {
  # Original server workflow: create one combined data table.
  source("source/geo_functions.R")
  library(doParallel)

  load("~/shared/data_projects/ithaca/misc/evap_fnames_2000_2019_full_record.Rdata")
  evap_2000_2019 <- lapply(EVAP_FNAMES_2000_2019, raster::brick)
  names(evap_2000_2019) <- EVAP_FNAMES_SHORT_2000_2019

  registerDoParallel(cores = N_CORES - 1)
  evap_datasets <- foreach(
    dataset_count = seq_len(n_datasets_2000_2019),
    .combine = rbind) %do% {
    dummy <- raster_to_dt(evap_2000_2019[[dataset_count]])
    dummy$dataset <- names(evap_2000_2019)[[dataset_count]]
    dummy
  }

  evap_datasets <- evap_datasets[, .(
    lon = lon,
    lat = lat,
    year = factor(lubridate::year(date)),
    dataset,
    evap = value
  )]

  saveRDS(
    evap_datasets,
    paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets.rds")
  )
}
