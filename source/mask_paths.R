# Paths of masks for spatial cropping

PATH_MASK <- paste0("~/shared/data/geodata/masks/final/")

## Raster data
PATH_MASKS_LAND_OCEAN <- paste0(PATH_MASK, "land_ocean") #IMERG
PATH_MASKS_ELEVATION <- paste0(PATH_MASK, "elevation_zones") 
PATH_MASKS_LAND_USE <- paste0(PATH_MASK, "landcover")
PATH_MASKS_LITHOLOGY <- paste0(PATH_MASK, "lithology")

## Shape files data
PATH_MASKS_KOPPEN <- paste0(PATH_MASK, "climate") # Koeppen-Geiger Beck et al. 2022
PATH_MASKS_COUNTRY <- paste0(PATH_MASK, "country_borders") #library(maptools)
PATH_MASKS_BIOME <- paste0(PATH_MASK, "biomes")
PATH_MASKS_SEDIMENT <- paste0(PATH_MASK, "sediment")
PATH_MASKS_MA_BASINS <- paste0(PATH_MASK, "ma_et_al_water_balance")
PATH_MASKS_BECK_KOEPPEN <- paste0(PATH_MASK, "Beck_KG_V1")

# Spatial Polygon

PATH_MASKS_IPCC <- paste0(PATH_MASK, "IPCC_reference_regions_v4")
