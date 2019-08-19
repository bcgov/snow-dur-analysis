
library(tidyverse)
library(ggspatial)
library(raster)
library(sf)
library(mapview)

setwd("C:/Users/bevington/Desktop")

# Points
df = read.csv("points.csv", sep = " ")
df_sf = st_as_sf(df, coords = c("lon", "lat"), crs = 4326)

# SD Raster
sd = brick("C:/Users/bevington/Dropbox/FLNRO_p1/Research_Cryosphere/Project_Snow/Git_Snow_MODIS/Data/MODIS/Derived/Annual_Snow_Metrics/MD10A1_SD_2017_clp.tif")

# Pick a site, 5 km buffer
df_sf_buff = df_sf %>% 
  st_transform(crs = 3005) %>% 
  filter(name == "robson") %>%
  st_buffer(100000)
mapview(df_sf_buff)

# Clip raster to buffer
sd_mask = mask(sd, df_sf_buff %>% st_transform(crs = 4326))
sd_mask = crop(sd_mask, df_sf_buff %>% st_transform(crs = 4326))

# Plot
ggplot() + 
  layer_spatial(sd_mask[[1]], interpolate = F) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 80, na.value = "white") +
  coord_sf(crs = 3005) + 
  theme_minimal() + theme(panel.grid.major = element_blank())
