source("source/main.R")

# Evaporation

## Period 2000-2019
EVAP_FNAMES_SHORT_2000_2019_FULL_RECORD <- c("bess","camele","era5-land","etmonitor","etsynthesis","fldas", "gldas-clsm","gldas-noah", 
                                             "gldas-vic","gleam", "jra55", "merra2","mod16a","terraclimate")

EVAP_FNAMES_2000_2019_FULL_RECORD <- c(list.files(path = PATH_EVAP_SIM, full.names = TRUE),
                                       list.files(path = PATH_EVAP_OBS, full.names = TRUE))

EVAP_FNAMES_2000_2019_FULL_RECORD <- unique(grep(paste(EVAP_FNAMES_SHORT_2000_2019_FULL_RECORD, collapse = "|"), 
                                                 EVAP_FNAMES_2000_2019_FULL_RECORD, value = TRUE))
EVAP_FNAMES_2000_2019_FULL_RECORD <- grep("land", EVAP_FNAMES_2000_2019_FULL_RECORD, value = TRUE)
EVAP_FNAMES_2000_2019_FULL_RECORD <- grep("yearly", EVAP_FNAMES_2000_2019_FULL_RECORD, value = TRUE)
EVAP_FNAMES_2000_2019_FULL_RECORD <- grep("194801_201412", EVAP_FNAMES_2000_2019_FULL_RECORD, value = TRUE, invert = TRUE)
EVAP_FNAMES_2000_2019_FULL_RECORD <- grep("e_mm", EVAP_FNAMES_2000_2019_FULL_RECORD, value = TRUE)

# order for alphabetical file name only
dummy <- strsplit(EVAP_FNAMES_2000_2019_FULL_RECORD, split = '//')
dummy <- sapply(dummy, "[[", 1)
dummy <- strsplit(dummy, split = '_')
dummy <- sapply(dummy, "[[", 1)
dummy <- strsplit(dummy, split = '/')
dummy <- sapply(dummy, "[[", 8)

EVAP_FNAMES_SHORT_2000_2019_FULL_RECORD_ordered_id <- sort(dummy, index.return = T)[[2]]

EVAP_FNAMES_2000_2019_FULL_RECORD <- EVAP_FNAMES_2000_2019_FULL_RECORD[EVAP_FNAMES_SHORT_2000_2019_FULL_RECORD_ordered_id]

save(EVAP_FNAMES_2000_2019_FULL_RECORD, file = "~/shared/data_projects/ithaca/misc/evap_fnames_2000_2019_full_record.Rdata")
