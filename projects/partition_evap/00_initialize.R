# Creates project paths -----
RUN_LOCATION <- Sys.getenv("ITHACA_RUN_LOCATION")
if (!nzchar(RUN_LOCATION)) {
  RUN_LOCATION <- if (dir.exists(path.expand("~/shared"))) "server" else "local"
}

switch(
  RUN_LOCATION,
  local = source("source/running_local.R"),
  server = {
    source("source/main.R")
    source("database/07_dataset_fnames_evap.R")

    ## Paths
    ### Output
    PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "/partition_evap/")
    PATH_SAVE_PARTITION_EVAP_RAW <- paste0(PATH_SAVE, "/partition_evap/raw/")
    PATH_SAVE_PARTITION_EVAP_SPATIAL <- paste0(PATH_SAVE, "/partition_evap/spatial/")
    PATH_SAVE_PARTITION_EVAP_FIGURES <- paste0(PATH_SAVE, "/partition_evap/figures/")
    PATH_SAVE_PARTITION_EVAP_TABLES <- paste0(PATH_SAVE, "/partition_evap/tables/")

    dir.create(PATH_SAVE_PARTITION_EVAP, showWarnings = FALSE)
    dir.create(PATH_SAVE_PARTITION_EVAP_RAW, showWarnings = FALSE)
    dir.create(PATH_SAVE_PARTITION_EVAP_SPATIAL, showWarnings = FALSE)
    dir.create(PATH_SAVE_PARTITION_EVAP_FIGURES, showWarnings = FALSE)
    dir.create(PATH_SAVE_PARTITION_EVAP_TABLES, showWarnings = FALSE)

    save(PATH_SAVE_PARTITION_EVAP,
         PATH_SAVE_PARTITION_EVAP_RAW,
         PATH_SAVE_PARTITION_EVAP_SPATIAL,
         PATH_SAVE_PARTITION_EVAP_FIGURES,
         PATH_SAVE_PARTITION_EVAP_TABLES,
         file = paste0(PATH_SAVE_PARTITION_EVAP, "paths.Rdata"))
  },
  stop("Unknown run location: ", RUN_LOCATION,
       ". Use 'local' or 'server'.")
)
