# Copyright 2018 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


## plots for paper publication

library(lubridate)
library(patchwork)

## figure 4 ONI time series plot
dfo_plot1 <- dfo_ts %>%
  gather(key = month, value = ONI, -year) %>%
  mutate(monyear = as.Date(paste(year, "-", month, "-01", sep = ""), format = "%Y-%b-%d")) %>%
  ggplot(aes(monyear, ONI)) +
  geom_bar(stat = "identity", aes(fill = ONI), colour = "black") +
  labs(x = "", y = "Monthly ONI") +
  geom_hline(yintercept = 0) +
  scale_fill_distiller(palette = "Spectral") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  theme_bw(base_size = 25, base_family = "Calibri") +
  theme(legend.position = "none", aspect.ratio = 0.3,
        panel.grid.major = element_line(linetype = 2, size = .4, color = "dark grey"),
        panel.grid.minor = element_line(linetype = 2, size = .4, color = "dark grey"))
dfo_plot1

dfo_ts2 <- dfo_ts %>%
  gather(key = month, value = ONI, -year) %>%
  mutate(monyear = as.Date(paste(year, "-", month, "-01", sep = ""), format = "%Y-%b-%d"),
  season = case_when(month == "Dec" | month == "Jan" | month == "Feb" ~ "Winter",
                     month == "Mar" | month == "Apr" | month == "May" ~ "Spring",
                     month == "Jun" | month == "Jul" | month == "Aug" ~ "Summer",
                     month == "Sep" | month == "Oct" | month == "Nov" ~ "Fall"))

## because the seasonal average is calculated by year, and winter season uses
## December from previous year, propagating December into next year
dfo_ts2[dfo_ts2$month == "Dec", "year"] <- dfo_ts2[dfo_ts2$month == "Dec", "year"] + 1

dfo_plot2 <- dfo_ts2 %>%
  group_by(year, season) %>%
  mutate(seasonal_ONI = mean(ONI, na.rm = TRUE)) %>%
  filter(month == "Mar" | month == "Jun" | month == "Sep" | month == "Dec") %>%
  ggplot(aes(monyear, seasonal_ONI)) +
  geom_bar(stat = "identity", aes(fill = seasonal_ONI), colour = "black") +
  labs(x = "", y = "Seasonal ONI") +
  geom_hline(yintercept = 0) +
  scale_fill_distiller(palette = "Spectral") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  theme_bw(base_size = 25) +
  theme(legend.position = "none", aspect.ratio = 0.3,
        panel.grid.major = element_line(linetype = 2, size = .4, color = "dark grey"),
        panel.grid.minor = element_line(linetype = 2, size = .4, color = "dark grey"))
dfo_plot2

dfo_plot <- dfo_plot1 + dfo_plot2 + plot_layout(ncol = 1)
dfo_plot

## figure 5 snow cover over time
reg_plot <- df_cal_long %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(year, doy)) + # raw doy data unprocessed
  geom_smooth(method = "lm", colour = "#2b8cbe") +
  labs(x = "", y = "") + # showing doy for start and end but duration for INT and SCI
  facet_wrap("variable", scales = "free_y") +
  theme_bw(base_size = 25) +
  theme(legend.position = "none", strip.background = element_rect(fill = "transparent", colour = "grey"),
        panel.grid.major = element_line(linetype = 2, size = .4, color = "dark grey"),
        panel.grid.minor = element_line(linetype = 2, size = .4, color = "dark grey"))
reg_plot

## figure 6 ONI correlation map
SCI_map <- df_oni_prov %>%
  filter(measurements == "SCI") %>%
  rename("geometry" = "SHAPE") %>%
  ggplot() +
  geom_sf(aes(fill = cor_seasonal)) +
  labs(title = "SCI") +
  scale_fill_viridis_c(name = "Cor") +
  facet_wrap("season") +
  theme_void() +
  theme(legend.key.size = unit(0.5, "cm"), plot.title = element_text(hjust = 0.5),
        panel.grid = element_line(colour = "transparent"), text = element_text(size = 20),
        strip.text = element_text(size = 20), legend.text = element_text(size = 20))

INT_map <- df_oni_prov %>%
  filter(measurements == "INT") %>%
  rename("geometry" = "SHAPE") %>%
  ggplot() +
  geom_sf(aes(fill = cor_seasonal)) +
  labs(title = "INT") +
  scale_fill_viridis_c(name = "Cor") +
  facet_wrap("season") +
  theme_void() +
  theme(legend.key.size = unit(0.5, "cm"), plot.title = element_text(hjust = 0.5),
        panel.grid = element_line(colour = "transparent"), text = element_text(size = 20),
        strip.text = element_text(size = 20), legend.text = element_text(size = 20))

INTs_map <- df_oni_prov %>%
  filter(measurements == "INTs") %>%
  rename("geometry" = "SHAPE") %>%
  ggplot() +
  geom_sf(aes(fill = cor_seasonal)) +
  labs(title = "INTs") +
  scale_fill_viridis_c(name = "Cor") +
  facet_wrap("season") +
  theme_void() +
  theme(legend.key.size = unit(0.5, "cm"), plot.title = element_text(hjust = 0.5),
        panel.grid = element_line(colour = "transparent"), text = element_text(size = 20),
        strip.text = element_text(size = 20), legend.text = element_text(size = 20))

INTe_map <- df_oni_prov %>%
  filter(measurements == "INTe") %>%
  rename("geometry" = "SHAPE") %>%
  ggplot() +
  geom_sf(aes(fill = cor_seasonal)) +
  labs(title = "INTe") +
  scale_fill_viridis_c(name = "Cor") +
  facet_wrap("season") +
  theme_void() +
  theme(legend.key.size = unit(0.5, "cm"), plot.title = element_text(hjust = 0.5),
        panel.grid = element_line(colour = "transparent"), text = element_text(size = 20),
        strip.text = element_text(size = 20), legend.text = element_text(size = 20))

ONI_map <- SCI_map + INT_map + INTs_map + INTe_map
ONI_map


## figure 7 topo effects - snow measurements against elevation
## should not keep only unique records of correlation, even though every elevation has
## the same correlation?
topo_plot <- df_oni %>%
  ggplot(aes(cor_seasonal, mean_z, colour = season)) +
  geom_point(aes(fill = season), color = "black", size = 3, shape = 21) +
  geom_smooth(method = "lm", se = F) +
  labs(x = "Correlation", y = "Elevation (m)") +
  scale_color_brewer(palette = "Set1", name = "") +
  facet_wrap("measurements") +
  theme_bw() +
  theme(legend.key.size = unit(0.5, "cm"), plot.title = element_text(hjust = 0.5),
        strip.background = element_rect(fill = "transparent", colour = "grey"),
        panel.grid = element_line(colour = "transparent"), text = element_text(size = 20),
        strip.text = element_text(size = 20), legend.text = element_text(size = 20)) +
  geom_vline(xintercept = 0)
topo_plot

## figure 8 forest cover change plot
tc_plot <- df_full %>%
  select(loss, TC2000, grep("SCI_20", colnames(df_full)), grep("INT_20", colnames(df_full)),
         grep("INTs_20", colnames(df_full)), grep("INTe_20", colnames(df_full))) %>%
  gather(key = "measurements", value = "value", -loss, -TC2000) %>%
  mutate(measurements = sub("_.*", "", measurements),
         tree_c = case_when(TC2000 > 20 & loss == 0 ~ "stable",
                            TC2000 > 20 & loss == 1 ~ "loss",
                            TC2000 < 20 ~ "none"),
         tree_c = factor(as.factor(tree_c), c("stable", "loss", "none"))) %>%
  filter(complete.cases(tree_c)) %>%
  ggplot(aes(measurements, value, fill = tree_c)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "YlGn", name = "Forest Cover", direction = -1) +
  theme_bw(base_size = 25, base_family = "Calibri") +
  theme(panel.grid.major = element_line(linetype = 2, size = .4, color = "dark grey"),
        panel.grid.minor = element_line(linetype = 2, size = .4, color = "dark grey"),
        aspect.ratio = 0.6, legend.position = "bottom",
        legend.text = element_text(size = 20), legend.title = element_text(size = 20))
tc_plot

## graphic output
png("../snow_docs/plots/F4_dfo_plot.png", 800, 550, "px")
dfo_plot
dev.off()

png("../snow_docs/plots/F5_reg_plot.png", 700, 550, "px")
reg_plot
dev.off()

png("../snow_docs/plots/F6_oni_map.png", 1000, 600, "px")
ONI_map
dev.off()

png("../snow_docs/plots/F7_topo_plot.png", 700, 550, "px")
topo_plot
dev.off()

png("../snow_docs/plots/F8_tc_plot.png", 800, 600, "px")
tc_plot
dev.off()
