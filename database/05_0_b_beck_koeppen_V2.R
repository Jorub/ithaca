source('source/main.R')
source('source/geo_functions.R')
source('source/mask_paths.R')
library(terra)
# Load tiff
kg_raster <- rast(paste0(PATH_MASKS_BECK_KOEPPEN_v2,"/koppen_geiger_0p00833333.tif"))
target <- rast(paste0(PATH_PREC_SIM, "/era5_tp_mm_land_195901_202112_025_yearly.nc"))

crs(kg_raster) <- crs(target)


KG_col_tab <- coltab(kg_raster)[[1]]

kg_meta <- read.table(paste0(PATH_MASKS_BECK_KOEPPEN_v2,"/KG_legend.csv"), sep =';',
                      col.names = c('KG', 'R', 'G', 'B'))


# reproject
terraOptions(gdal = FALSE)

kg_modal <- aggregate(
  kg_raster,
  fact = 30,
  fun = "modal",
  na.rm = TRUE
)

if (!compareGeom(
  kg_modal,
  target,
  crs = TRUE,
  ext = TRUE,
  rowcol = TRUE,
  res = TRUE,
  stopOnError = FALSE
)) {
  kg_modal <- resample(
    kg_modal,
    target,
    method = "near"
  )
}

compareGeom(
  kg_modal,
  target,
  crs = TRUE,
  ext = TRUE,
  rowcol = TRUE,
  res = TRUE,
  stopOnError = TRUE
)

terraOptions(gdal = TRUE)


plot(kg_modal)

crs(kg_modal)
crs(target)

res(kg_modal)
res(target)

ext(kg_modal)
ext(target)

nrow(kg_modal); ncol(kg_modal)
nrow(target); ncol(target)

writeCDF(kg_modal, 
         filename = paste0(PATH_MASKS_BECK_KOEPPEN_v2,"/kg_modal_0.25deg_v2.nc"),
         varname  = "KoppenGeiger",
         overwrite = TRUE)
