source("source/main.R")
#source("source/partition_evap.R")

# parallel
library(doParallel)

# Input ---- ####
PATH_IPCC_data <- paste0(PATH_DATA,"/geodata/ipcc_v4/")

# Output ---- ####
PATH_SAVE_EVAP_TREND <- paste0(PATH_SAVE, "/evap_trend/")
load(paste0(PATH_SAVE_EVAP_TREND, "paths.Rdata"))

# Colors---- ####
## Color for datasets ####

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

cols_data_c <- c("GLDAS-VIC" = "#4A90E2",    "GLDAS-NOAH" = "#2A3F9F",   "GLDAS-CLSM"  = "#2E64FE",  
                 "MOD16A"  = "chartreuse3",      "CAMELE"   = "red2",   "ETMonitor"   = "#708238",   
               "SynthesizedET" = "#940308", "GLEAM" = "#004F00", "MERRA-2"  = "#4a3009", "JRA-55" = "orange3",   
               "TerraClimate" = "#011a59", "FLDAS"  = "darkslategray3", "ERA5-land" = "orange1", "BESS"= "#228B22")   

# KG classes
source('source/mask_paths.R')

kg_meta <- data.table(read.table(paste0(PATH_MASKS_BECK_KOEPPEN,"/KG_legend.csv"), sep =';',
                      col.names = c('KG', 'R', 'G', 'B')))

kg_meta[, hex := rgb(R,G,B, maxColorValue = 255)]

cols_kg <- kg_meta$hex
names(cols_kg) <- kg_meta$KG


# IPCC ####
IPCC_Africa <- c("CAF", "ESAF", "MDG", "NEAF", "SAH", "SEAF", "WAF", "WSAF")
IPCC_Asia <-   c("ARP", "EAS", "ECA", "ESB",  "RFE", "RAR",  "SAS", "SEA",  "TIB", "WCA", "WSB")
IPCC_Australasia <- c("CAU", "EAU", "NAU", "NZ", "PAC", "SAU")
IPCC_Europe <- c("EEU", "GIC","MED", "NEU", "WCE")
IPCC_Namerica <- c("CAR", "CNA", "ENA", "NCA","NEN", "NWN", "SCA", "WNA")
IPCC_Samerica <- c("NES","NSA","NWS","SAM","SES", "SSA","SWS")


n_datasets_2000_2019 <- 14

# Functions ---- ####
## Calculate theil sen slope for each grid in parallel 
evap_trends <- function(x) {
  no_cores <- detectCores() - 1
  if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  registerDoParallel(cores = no_cores)
  if (length(unique(x$lon)) > length(unique(x$lat))) {
    x <- split(x, by = "lon")
  } else {
    x <- split(x, by = "lat")
  }

  dummie <- foreach (idx = 1:length(x), .combine = rbind, .packages = c("Kendall", "RobustLinearReg")) %dopar% {
    dummie_row <- x[[idx]]
    dummie_row <- dummie_row[, .(  kendall_tau = Kendall(evap, year)$tau,
                                   kendall_p_value = Kendall(evap, year)$sl,
                                   theil_sen_slope = theil_sen_regression(evap~year)$coefficients[2], 
                                   theil_sen_p_value = summary(theil_sen_regression(evap~year))$coefficients[8]
    ), 
    .(lon, lat, dataset)]
  }
  return(dummie)
}


evap_trends_lon_lat <- function(x) {
  no_cores <- detectCores() - 1
  if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  registerDoParallel(cores = no_cores)
  if (length(unique(x$lon)) > length(unique(x$lat))) {
    x <- split(x, by = "lon")
  } else {
    x <- split(x, by = "lat")
  }
  
  dummie <- foreach (idx = 1:length(x), .combine = rbind, .packages = c("Kendall", "RobustLinearReg")) %dopar% {
    dummie_row <- x[[idx]]
    dummie_row <- dummie_row[, .(  kendall_tau = Kendall(evap, year)$tau,
                                   kendall_p_value = Kendall(evap, year)$sl,
                                   theil_sen_slope = theil_sen_regression(evap~year)$coefficients[2], 
                                   theil_sen_p_value = summary(theil_sen_regression(evap~year))$coefficients[8]
    ), 
    .(lon, lat)]
  }
  return(dummie)
}


evap_trends_lon_lat_boot <- function(x) {
  no_cores <- detectCores() - 1
  if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  registerDoParallel(cores = no_cores)
  if (length(unique(x$lon)) > length(unique(x$lat))) {
    x <- split(x, by = "lon")
  } else {
    x <- split(x, by = "lat")
  }
  
  dummie <- foreach (idx = 1:length(x), .combine = rbind, .packages = c("openair")) %dopar% {
    dummie_row <- x[[idx]]
    dummie_row <- dummie_row[, TheilSen(.SD, pollutant = "evap", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)]
    , 
    .(lon, lat)]
  }
  return(dummie)
}


evap_trends_boot <- function(x) {
  no_cores <- detectCores() - 1
  if (no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  registerDoParallel(cores = no_cores)
  if (length(unique(x$lon)) > length(unique(x$lat))) {
    x <- split(x, by = "lon")
  } else {
    x <- split(x, by = "lat")
  }
  
  dummie <- foreach (idx = 1:length(x), .combine = rbind, .packages = c("openair")) %dopar% {
    dummie_row <- x[[idx]]
    dummie_row <- dummie_row[, TheilSen(.SD, pollutant = "evap", autocor = TRUE, plot = F, silent = T)$data$main.data[1,c(10,12,16,17)]
                             , 
                             .(lon, lat, dataset)]
  }
  return(dummie)
}

