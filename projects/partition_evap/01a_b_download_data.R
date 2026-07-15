# Download annual data from Zenodo ----
source("source/partition_evap.R")

ZENODO_RECORD_URL <- "https://zenodo.org/records/18150992/files/"

EVAP_DATASETS_ZENODO <- c(
  "bess",
  "camele",
  "era5-land",
  "etmonitor",
  "etsynthesis",
  "fldas",
  "gldas-clsm",
  "gldas-noah",
  "gldas-vic",
  "gleam",
  "jra55",
  "merra2",
  "mod16a",
  "terraclimate"
)

ZENODO_FILE_NAMES <- paste0(
  EVAP_DATASETS_ZENODO,
  "_e_mm_land_200001_201912_025_yearly.nc"
)

download_zenodo_file <- function(file_name,
                                 folder_path = PATH_SAVE_PARTITION_EVAP_RAW) {
  destination <- file.path(folder_path, file_name)

  if (file.exists(destination)) {
    message("Already present, skipping: ", file_name)
    return(FALSE)
  }

  temporary_destination <- paste0(destination, ".part")
  file_url <- paste0(ZENODO_RECORD_URL, file_name, "?download=1")

  on.exit(unlink(temporary_destination), add = TRUE)
  message("Downloading: ", file_name)
  download.file(file_url, temporary_destination, mode = "wb")

  if (!file.rename(temporary_destination, destination)) {
    stop("Could not move the completed download to: ", destination)
  }

  TRUE
}

if (identical(RUN_LOCATION, "local")) {
  dir.create(PATH_SAVE_PARTITION_EVAP_RAW,
             recursive = TRUE,
             showWarnings = FALSE)

  downloaded <- vapply(ZENODO_FILE_NAMES,
                       download_zenodo_file,
                       logical(1))

  message(
    "Download complete: ", sum(downloaded), " downloaded, ",
    sum(!downloaded), " already present."
  )
} else {
  message("Server run: Zenodo downloads skipped.")
}
