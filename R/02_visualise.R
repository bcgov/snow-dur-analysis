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


library(RColorBrewer)
library(patchwork)

pal <- brewer.pal(9, "Blues")

## Snow Cover Index (SCI) by ecoprovince
prov_plot <- ggplot() +
  geom_sf(data = df_prov, aes(fill = SCI_avg)) +
  scale_fill_gradientn(colours = pal, name = "Average SCI",
                        limits = c(57, 230), na.value = "#001d49") + # tweaking SAL
  facet_wrap(facets = vars(variable)) +
  theme_void() +
  theme(panel.grid = element_line(colour = "transparent"), text = element_text(size = 12),
        strip.text = element_text(size = 12), legend.text = element_text(size = 12))
prov_plot

## Oceanic Nino Index (ONI) correlation plot
oni_plot <- ggplot() +
  geom_sf(data = df_oni_prov, aes(fill = cor)) +
  scale_fill_viridis_c(name = "Correlation") +
  facet_wrap(c("season", "measurements")) +
  theme_void() +
  theme(panel.grid = element_line(colour = "transparent"), text = element_text(size = 12),
        strip.text = element_text(size = 12), legend.text = element_text(size = 12))
oni_plot

## dot plot showing average snow amount each ecosection, equal unit
dot_plot <- ggplot(df_dots_map) +
  geom_sf(aes(size = SCI_sum), show.legend = "point", colour = "#4292c6") +
  labs(title = "Interpolated Sum of SCI by Ecosections (2002-2017)") +
  scale_size_area(name = "SCI sum") +
  theme_void() +
  theme(panel.grid = element_line(colour = "transparent"), text = element_text(size = 14),
        plot.title = element_text(size = 24, hjust = 0.5, margin = margin(10, 0, 0, 0)))
dot_plot

## static plots
## calendar plot for snow start and end dates
cal_plot <- df_cal_long %>%
  filter(variable == "Start Date" | variable == "End Date") %>%
  ggplot(aes(day, year, fill = n)) +
  geom_tile(colour = "grey", width = 0.8) +
  labs(x = "", y = "") +
  scale_fill_viridis_c(name = "Number\nof Sites") +
  scale_x_discrete(breaks = c("2018-01-07", "2018-03-12", "2018-06-15", "2018-09-17", "2018-12-30"),
                   labels = c("Jan", "Mar", "Jun", "Sep", "Dec")) +
  # scale_x_date(date_labels = "%b %d") +
  facet_grid(variable ~ .) +
  theme_light() +
  theme(panel.grid = element_blank(), strip.background = element_rect(fill = "transparent", colour = "grey"),
        strip.text = element_text(colour = "black", size = 12), axis.ticks = element_blank(), text = element_text(size = 12),
        axis.text = element_text(size = 12), legend.text = element_text(size = 12))
cal_plot

## snow duration plot
dur_plot <- df_full %>%
  select(ID, ECOPROVINCE_CODE, grep("INTs_20", colnames(df_full)), grep("INTe_20", colnames(df_full)),
         grep("INT_20", colnames(df_full))) %>% # for ordering
  gather(key = variable, value = doy, -ID, -ECOPROVINCE_CODE) %>%
  mutate(year = sub(".*_", "", variable), variable = sub("_.*", "", variable)) %>%
  spread(variable, doy) %>%
  gather(key = variable, value = doy, -ID, -ECOPROVINCE_CODE, -INT, -year) %>%
  group_by(variable, ECOPROVINCE_CODE, year) %>%
  dplyr::summarise(doy = mean(doy, na.rm =  TRUE), INT = mean(INT, na.rm = TRUE)) %>%
  ggplot(aes(reorder(ECOPROVINCE_CODE, -INT), doy, colour = year)) +
  geom_point(position = position_dodge(width = 0.8)) +
  geom_line(position = position_dodge(width = 0.8), size = 0.7) +
  labs(x = "", y = "Date of Year") +
  scale_color_viridis_d(direction = -1, name = "") +
  theme_light() +
  theme(panel.grid = element_blank(), axis.ticks = element_blank(), text = element_text(size = 12),
        axis.text = element_text(size = 12), legend.text = element_text(size = 12))
dur_plot

## boxplot of SCI averaged by pixel, coloured by SCI averaged by ecoprovince
SCI_plot <- df_full %>%
  select(ECOPROVINCE_CODE, SCI_mean) %>%
  group_by(ECOPROVINCE_CODE) %>%
  mutate(SCI_eco = mean(SCI_mean, na.rm = TRUE)) %>%
  ggplot(aes(ECOPROVINCE_CODE, SCI_mean, fill = SCI_eco)) +
  geom_boxplot() +
  labs(x = "", y = "SCI averaged by pixel") +
  scale_fill_gradientn(colours = pal, name = "SCI by\nEcoprovince") +
  theme_light() +
  theme(panel.grid = element_blank(), axis.ticks = element_blank(), text = element_text(size = 12),
        axis.text = element_text(size = 12), legend.text = element_text(size = 12))
SCI_plot

## for hydrozones
SCI_plot2 <- df_full %>%
  select(HYDROLOGICZONE_NAME, SCI_mean) %>%
  group_by(HYDROLOGICZONE_NAME) %>%
  mutate(SCI_eco = mean(SCI_mean, na.rm = TRUE)) %>%
  ggplot(aes(HYDROLOGICZONE_NAME, SCI_mean, fill = SCI_eco)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Mean SCI per Pixel among Hydrozones (2002-2017)", x = "", y = "SCI averaged by pixel") +
  scale_fill_gradientn(colours = pal, name = "SCI by\nHydrozone") +
  theme_light() +
  theme(panel.grid = element_blank(), axis.ticks = element_blank(), text = element_text(size = 14),
        plot.title = element_text(size = 24, hjust = 0.5, margin = margin(10, 0, 15, 0)))
SCI_plot2

## SCI against elevation, using mean SCI and elevation per observation point data
elev_plot <- ggplot(df_full, aes(SCI_mean, z)) +
  geom_hex() +
  labs(x = "Average SCI", y = "Elevation") +
  scale_fill_viridis_c() +
  facet_wrap("ECOPROVINCE_CODE") +
  theme_light() +
  theme(panel.grid = element_blank(), strip.background = element_rect(fill = "transparent", colour = "grey"),
        strip.text = element_text(colour = "black", size = 12), axis.ticks = element_blank(), text = element_text(size = 12),
        axis.text = element_text(size = 12), legend.text = element_text(size = 12))
elev_plot

## ONI and SCI time series plot
ts_SCI <- df_oni_long %>%
  filter(measurements == "SCI") %>%
  group_by(ECOPROVINCE_CODE, year) %>%
  dplyr::summarise(SCI = mean(value, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(mapping = aes(year, SCI, colour = ECOPROVINCE_CODE), size = 0.7) +
  labs(x = "", y = "Annual mean SCI") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_brewer(palette = "Paired", name = "") +
  theme_light() +
  theme(panel.grid = element_blank(), axis.ticks = element_blank(), text = element_text(size = 12),
       axis.text.x = element_text(angle = -35, hjust = -0.2),
       axis.text = element_text(size = 12), legend.text = element_text(size = 12))
ts_SCI

ts_ONI <- df_oni_long %>%
  group_by(monyear) %>%
  dplyr::summarise(ONI = mean(ONI, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(mapping = aes(monyear, ONI)) +
  labs(x = "", y = "Monthly Mean ONI") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = as.Date(c("2002-01-01", "2017-01-01"))) +
  theme_light() +
  theme(panel.grid = element_blank(), axis.ticks = element_blank(), text = element_text(size = 12),
        axis.text.x = element_text(angle = -35, hjust = -0.2),
        axis.text = element_text(size = 12), legend.text = element_text(size = 12))
ts_ONI

ts_plot <- ts_SCI + ts_ONI + plot_layout(ncol = 1, heights = c(3, 1))
ts_plot

## looping over seasonal ONI correlation with different snow measurements plot
for (i in 1:length(unique(df_oni$measurements))) {

  oni_cor_plot <- df_oni %>%
    filter(measurements == unique(df_oni$measurements)[i]) %>%
    ggplot(mapping = aes(ECOPROVINCE_CODE, cor_seasonal, fill = p_value_seasonal)) +
    geom_errorbar(aes(ymin = cor_min, ymax = cor_max), width = 0.4) +
    geom_point(size = 2, shape = 21, alpha = 0.5) +
    scale_fill_viridis_c(direction = -1, name = "p-value", breaks = c(0.05, seq(0, 0.8, 0.2))) +
    labs(x = "", y = paste("ONI Correlation with", unique(df_oni$measurements)[i])) +
    coord_flip() +
    facet_wrap("season") +
    theme_light() +
    theme(panel.grid = element_blank(), strip.background = element_rect(fill = "transparent", colour = "grey"),
          strip.text = element_text(colour = "black", size = 11), axis.ticks = element_blank(), text = element_text(size = 11),
          axis.text = element_text(size = 11), legend.text = element_text(size = 11), legend.title = element_text(size = 12))

  ggsave(paste("../snow_docs/plots/ONI_cor_", unique(df_oni$measurements)[i], ".png"), oni_cor_plot,
         width = 5.5, height = 4)
}


## saving plots
png("../snow_docs/plots/ecoprov_map.png", 750, 450, "px")
prov_plot
dev.off()
png("../snow_docs/plots/ONI_map.png", 800, 500, "px")
oni_plot
dev.off()
png("../snow_docs/plots/dot_map.png", 1000, 600, "px")
dot_plot
dev.off()
png("../snow_docs/plots/calendar_plot.png", 750, 450, "px")
cal_plot
png("../snow_docs/plots/duration_plot.png", 700, 550, "px")
dur_plot
dev.off()
dev.off()
png("../snow_docs/plots/SCI_boxplot.png", 500, 300, "px")
SCI_plot
dev.off()
png("../snow_docs/plots/SCI_boxplot_hydro.png", 800, 600, "px")
SCI_plot2
dev.off()
png("../snow_docs/plots/elevation_plot.png", 550, 350, "px")
elev_plot
dev.off()
png("../snow_docs/plots/time-series_plot.png", 550, 400, "px")
ts_plot
dev.off()
