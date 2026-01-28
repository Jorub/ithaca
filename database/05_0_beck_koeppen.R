source('source/main.R')
source('source/geo_functions.R')
source('source/mask_paths.R')
library(terra)
# Load tiff
kg_raster <- rast(paste0(PATH_MASKS_BECK_KOEPPEN,"/Beck_KG_V1_present_0p083.tif"))
target <- rast(paste0(PATH_PREC_SIM, "/era5_tp_mm_land_195901_202112_025_yearly.nc"))

crs(kg_raster) <- crs(target)

KG_col_tab <- coltab(kg_raster)[[1]]

kg_meta <- read.table(paste0(PATH_MASKS_BECK_KOEPPEN,"/KG_legend.csv"), sep =';',
                      col.names = c('KG', 'R', 'G', 'B'))


# reproject


terraOptions(gdal = FALSE)
kg_resampled <- project(kg_raster, target, method="near")
terraOptions(gdal = TRUE)


plot(kg_resampled)

crs(kg_resampled)
crs(target)

res(kg_resampled)
res(target)

ext(kg_resampled)
ext(target)

nrow(kg_resampled); ncol(kg_resampled)
nrow(target); ncol(target)

writeCDF(kg_resampled, 
         filename = paste0(PATH_MASKS_BECK_KOEPPEN,"/kg_resampled_0.25deg.nc"),
         varname  = "KoppenGeiger",
         overwrite = TRUE)
