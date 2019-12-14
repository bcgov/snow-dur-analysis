#Import dependencies
library(tidyverse)
library(bcmaps)
library(sf)

# NR Districts
nrd <- nr_districts()

# Bevington et al., 2018 M*D10A1 Snow Duration Samples 
sd_samp_pts<-st_read('C:/Users/hgleason/Dropbox/FLNRO/Projects/snow-dur-analysis/R/snow_summaries_note/SD_Random_Samples.gpkg')

#Join the SD sample to the NR Districts via spatial intersection 
sd_by_nrd <- nrd %>%
  st_join(sd_samp_pts, join = st_intersects)

#Create Box-Plots for SDon, SDoff and SDdur by NR District
box_sdon <- ggplot(sd_by_nrd, aes(x=DISTRICT_NAME, y=sdon)) + geom_boxplot() + coord_flip() + labs(y="M*D10A1 Snow On Date (Days Since 1-Sep)", x = "")
box_sdon

box_sdoff <- ggplot(sd_by_nrd, aes(x=DISTRICT_NAME, y=sdoff)) + geom_boxplot() + coord_flip() + labs(y="M*D10A1 Snow Off Date (Days Since 1-Sep)", x = "")
box_sdoff

box_sddur <- ggplot(sd_by_nrd, aes(x=DISTRICT_NAME, y=sddur)) + geom_boxplot() + coord_flip() + labs(y="M*D10A1 Snow Duration (Days)", x = "")
box_sddur



#For later
sd_sumry <- sd_by_nrd %>%
   group_by(DISTRICT_NAME) %>%
  summarise(mean_sdon = mean(sdon, na.rm = TRUE))
   #summarise(mean_sdon = mean(sdon, na.rm = TRUE), sd_sdon = sd(sdon, na.rm = TRUE),mean_sdoff = mean(sdoff, na.rm = TRUE), sd_sdoff = sd(sdoff, na.rm = TRUE),mean_sddur = mean(sddur, na.rm = TRUE), sd_sddur = sd(sddur, na.rm = TRUE))


hm <- ggplot() + geom_sf(data = sd_sumry)
hm
