
library(bcmaps)
library(sf)
library(raster)
library(mapview)
library(tidyverse)
library(ggspatial)

setwd('C:/Users/hgleason/Dropbox/Git_Snow_MODIS/Data/MODIS/Raw/Annual_Snow_Metrics')


files <- list.files(path = ".", full.names = TRUE, recursive = TRUE)

shp = bc_bound()
shp = st_transform(shp, crs = 4326)


for (file in files)
{
  outname <- paste0(substr(file,0,16),"_clp.tif")
  
  tiff <- brick(file)
   
  ras_crop = raster::crop(tiff, shp)
   
  ras_mask = raster::mask(ras_crop, shp)
   
  writeRaster(x = ras_mask, filename = outname, overwrite=TRUE)

}
