
rm(list = ls())

library(reshape2)
library(tidyverse)

setwd("G:/Dropbox/FLNRO_p1/Research_Cryosphere/Project_Snow/Git_Snow_MODIS/")

df = read.csv("Data/MODIS/Derived/MD10A1_ASWS_Sampls.csv")

df %>% 
  filter(station == unique(df$station)[2]) %>% 
  # filter(year == 2005) %>% 
  melt(id.vars = c("station","year","lat","lon")) %>% 
  mutate(day = sub(pattern = "d", replacement = "", x = variable)) %>% 
  filter(value != -9) %>% 
  # mutate(dayofyear = as.Date(format(as.Date(paste0(year,"-09-01"))+as.numeric(day)-1,"2000-%m-%d"))) %>% 
  ggplot(aes(as.numeric(day), value)) + 
    geom_hline(yintercept = 0, linetype = 1, color = "black") +
    geom_smooth(method = "loess", span = 0.2) +
    geom_line(aes(color = "MODIS NDSI"))  +
    geom_point(shape = 21, fill = "white", aes(color = "MODIS NDSI")) + 
    geom_smooth(method = "loess", span = 0.2, aes(colour = "LOWESS (bw 0.2)"), se = F) +
    geom_hline(linetype = 2, aes(colour = "NDSI Threshold",yintercept = 30)) +
    facet_wrap(~year) + 
    theme_bw(base_size = 15) +
    theme(panel.grid = element_blank(), legend.direction = "vertical", legend.position = "bottom") +
    scale_colour_manual(values = c("blue","black","red")) +
    scale_linetype_manual(values = c(2)) +
    scale_y_continuous(limits = c(0,100), breaks = c(0,30,60,90)) +
    scale_x_continuous(breaks = c(0,122,242,365), labels = c(format(as.Date("2000-09-01")+0,"%b %d"),
                                                         format(as.Date("2000-09-01")+122,"%b %d"),
                                                         format(as.Date("2000-09-01")+242,"%b %d"),
                                                         format(as.Date("2000-09-01")+365,"%b %d")), expand = c(0.1,0.1)) +
    labs(x = "Day of Year", y = "NDSI", title = paste("ASWS station:",unique(df$station)[2], "MCBRIDE (UPPER) [53°18' N, 120°19' W, 1608 m]"), 
         colour = "")

  ggsave(filename = "Results/Figures/MODIS_timeseries_sz20.pdf")


