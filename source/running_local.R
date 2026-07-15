PATH_SAVE_PARTITION_EVAP <- paste0("data/partition_evap/")
PATH_SAVE_PARTITION_EVAP_RAW <- paste0("data/partition_evap/raw/")
PATH_SAVE_PARTITION_EVAP_SPATIAL <- paste0("data/partition_evap/spatial/")
PATH_SAVE_PARTITION_EVAP_FIGURES <- paste0("data/partition_evap/figures/")
PATH_SAVE_PARTITION_EVAP_TABLES <- paste0("data/partition_evap/tables/")

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
