RUN_LOCATION <- Sys.getenv("ITHACA_RUN_LOCATION")
if (!nzchar(RUN_LOCATION)) {
  RUN_LOCATION <- if (dir.exists(path.expand("~/shared"))) "server" else "local"
}

switch(
  RUN_LOCATION,
  local = {
    suppressPackageStartupMessages({
      library(data.table)
      library(lubridate)
      library(ggplot2)
      library(raster)
      library(ncdf4)
      library(sp)
      library(sf)
      library(stars)
    })

    N_CORES <- parallel::detectCores() - 2
    PATH_SAVE <- "data"

    M2_TO_KM2 <- 1e-6
    MM_TO_M <- 1e-3
    MM_TO_KM <- 1e-6
  },
  server = source("source/main.R"),
  stop("Unknown run location: ", RUN_LOCATION,
       ". Use 'local' or 'server'.")
)

## Paths

### Output
PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "/partition_evap/")
PATH_SAVE_PARTITION_EVAP_RAW <- paste0(PATH_SAVE, "/partition_evap/raw/")
PATH_SAVE_PARTITION_EVAP_SPATIAL <- paste0(PATH_SAVE, "/partition_evap/spatial/")
PATH_SAVE_PARTITION_EVAP_FIGURES <- paste0(PATH_SAVE, "/partition_evap/figures/")
PATH_SAVE_PARTITION_EVAP_TABLES <- paste0(PATH_SAVE, "/partition_evap/tables/")

### Project data
EVAP_FNAMES_2000_2019 <-  list.files(path = PATH_SAVE_PARTITION_EVAP_RAW, full.names = TRUE)
EVAP_FNAMES_SHORT_2000_2019 <- sub("_.*", "", basename(EVAP_FNAMES_2000_2019))

## Variables
MIN_N_DATASETS <- 13
#n_datasets_2000_2019 <- length(EVAP_FNAMES_SHORT_2000_2019)
n_datasets_2000_2019 <- 14

## Specify start/end for the period of analysis 
period_start <- as.Date("2000-01-01") 
period_end <- as.Date("2019-12-31")
period_months <- lubridate::interval(period_start, period_end) %/%
  lubridate::period(months = 1) + 1

#global space
global_area_evap <- 125803654946773

## colors

cols_data <- c("bess" = "#228B22",
               "camele" = "red2",
               "era5-land" = "orange1",
               "etmonitor" = "#708238",
               "synthesizedet" = "#940308",#"#B81B1A",
               "fldas" = "darkslategray3",
               "gldas-clsm" = "#2E64FE",
               "gldas-noah" = "#2A3F9F",
               "gldas-vic" = "#4A90E2",
               "gleam" = "#004F00",
               "jra55" = "orange3",
               "merra2" = "#4a3009",
               "mod16a" = "chartreuse3",
               "terraclimate" = "#011a59"
)

## IPCC -----
IPCC_Africa <- c("CAF", "ESAF", "MDG", "NEAF", "SAH", "SEAF", "WAF", "WSAF")
IPCC_Asia <-   c("ARP", "EAS", "ECA", "ESB",  "RFE", "RAR",  "SAS", "SEA",  "TIB", "WCA", "WSB")
IPCC_Australasia <- c("CAU", "EAU", "NAU", "NZ", "PAC", "SAU")
IPCC_Europe <- c("EEU", "GIC","MED", "NEU", "WCE")
IPCC_Namerica <- c("CAR", "CNA", "ENA", "NCA","NEN", "NWN", "SCA", "WNA")
IPCC_Samerica <- c("NES","NSA","NWS","SAM","SES", "SSA","SWS")

#EVAP_DATASETS_OBS <- c()
EVAP_DATASETS_REANAL <- c("era5-land", "jra55", "merra2")
EVAP_DATASETS_REMOTE <- c("bess", "etmonitor", "gleam","mod16a")
EVAP_DATASETS_HYDROL <- c("fldas", "gldas-clsm", "gldas-noah", "gldas-vic", "terraclimate")
EVAP_DATASETS_ENSEMB <- c("camele", "etsynthesis", "synthesizedet")
