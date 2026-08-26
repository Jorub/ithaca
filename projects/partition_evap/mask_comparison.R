
source("source/partition_evap.R")

library(data.table)
library(ggplot2)
library(pRecipe)
library(jsonlite)


options(timeout = max(600, getOption("timeout")))


precipe_masks <- as.data.table(pRecipe_masks())
precipe_masks <- precipe_masks[elev_class != "",]

ggplot(precipe_masks)+
  geom_tile(aes(x = lon, y = lat, fill = elev_class))+
  theme_bw()

doi <- "10.5281/zenodo.18151961"

record_id <- sub(".*zenodo\\.", "", doi)

api_url <- paste0("https://zenodo.org/api/records/", record_id)

record <- fromJSON(api_url,
                   simplifyVector = FALSE)

files <- record$files

masks <- files[[1]]
download_url <- masks$links$self 
out_path <- file.path(PATH_SAVE_PARTITION_EVAP, masks$key)
download.file(url = download_url, destfile = out_path, mode = "wb", quiet = FALSE ) 

mask_data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

mask_data <- read.csv(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.csv"))

mask_data <- as.data.table(mask_data)

mask_common <- merge(mask_data, precipe_masks, by = c("lon", "lat", "elev_class"), all.x = T)


