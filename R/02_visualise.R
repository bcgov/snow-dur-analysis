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


library(rcartocolor) # palette
library(Cairo) # graphics device

## plotting
## bubble plot
# bbl_plot <- ggplot() +
#   geom_sf(data = df_bbl, aes(size = value), shape = 1, show.legend = "point") +
#   facet_wrap("variable") +
#   theme_void() +
#   theme(panel.grid = element_line(colour = "transparent"))
# bbl_plot

## Snow Cover Index (SCI) by ecoprovince
prov_plot <- ggplot() +
  geom_sf(data = df_prov, aes(fill = SCI_avg), lwd = 0.4) +
  scale_fill_carto_c(palette = "ArmyRose") +
  facet_wrap(facets = vars(variable)) +
  theme_void() +
  theme(panel.grid = element_line(colour = "transparent"))
prov_plot

## Oceanic Nino Index (ONI) correlation plot
oni_plot <- ggplot() +
  geom_sf(data = df_oni_prov, aes(fill = cor), lwd = 0.4) +
  scale_fill_viridis_c(name = "Correlation") +
  facet_wrap(c("season", "measurements")) +
  theme_void() +
  theme(panel.grid = element_line(colour = "transparent"))
oni_plot

## calendar plot for snow start and end dates
cal_plot <- ggplot(df_cal_long, aes(day, year, fill = n)) +
  geom_tile(colour = "grey", width = 0.8) +
  labs(x = "", y = "") +
  scale_fill_viridis_c(name = "Number\nof Sites") +
  scale_x_discrete(breaks = c("2018-03-12", "2018-06-15", "2018-09-17", "2018-12-14"),
                   labels = c("Mar", "Jun", "Sep", "Dec")) +
  # scale_x_date(date_labels = "%b %d") +
  facet_grid(variable ~ .) +
  theme_light() +
  theme(panel.grid = element_blank(), strip.background = element_rect(fill = "transparent", colour = "grey"),
        strip.text = element_text(colour = "black"), axis.ticks = element_blank())
cal_plot

## SCI against elevation
elev_plot <- ggplot(df_full, aes(SCI_mean, z)) +
  geom_hex() +
  labs(x = "Average SCI", y = "Elevation") +
  scale_fill_viridis_c() +
  facet_wrap("ECOPROVINCE_NAME") +
  theme_light() +
  theme(panel.grid = element_blank(), strip.background = element_rect(fill = "transparent", colour = "grey"),
        strip.text = element_text(colour = "black"), axis.ticks = element_blank())
elev_plot

## seasonal ONI correlation with measurements static plot
oni_cor <- df_oni %>%
  filter(measurements == "SCI") %>%
  ggplot(mapping = aes(ECOPROVINCE_NAME, cor_seasonal, fill = p_value_seasonal)) +
  geom_errorbar(aes(ymin = cor_min, ymax = cor_max), width = 0.5) +
  geom_point(size = 5, shape = 21, alpha = 0.5) +
  scale_fill_viridis_c(direction = -1, name = "p-value") +
  labs(x = "", y = "Correlation") +
  coord_flip() +
  facet_wrap("season") +
  theme_light() +
  theme(panel.grid = element_blank(), strip.background = element_rect(fill = "transparent", colour = "grey"),
        strip.text = element_text(colour = "black"), axis.ticks = element_blank())
plot(oni_cor)

## saving plots
# CairoPNG("../plots/bubble_map.png", 1200, 800)
# bbl_plot
# dev.off()
CairoPNG("../plots/ecoprov_map.png", 1200, 600)
prov_plot
dev.off()
CairoPNG("../plots/ONI_map.png", 1200, 600)
oni_plot
dev.off()
CairoPNG("../plots/calendar_plot.png", width = 1200, height = 600)
cal_plot
dev.off()
