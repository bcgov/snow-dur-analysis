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
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
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
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_bw(base_size = 25) +
  theme(legend.position = "none", aspect.ratio = 0.3,
        panel.grid.major = element_line(linetype = 2, size = .4, color = "dark grey"),
        panel.grid.minor = element_line(linetype = 2, size = .4, color = "dark grey"))
dfo_plot2

dfo_plot <- dfo_plot1 + dfo_plot2 + plot_layout(ncol = 1)
dfo_plot

## figure 5 snow cover over time
df_cal_long %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(year, doy)) + # raw doy data unprocessed
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "", y = "") + # showing doy for start and end but duration for INT and SCI
  facet_wrap(.~variable) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none", strip.background = element_rect(fill = "transparent", colour = "grey"),
        panel.grid.major = element_line(linetype = 2, size = .4, color = "dark grey"),
        panel.grid.minor = element_line(linetype = 2, size = .4, color = "dark grey"))

## figure 6 ONI correlation map
SCI_map <- df_oni_prov %>%
  filter(measurements == "SCI") %>%
  rename("geometry" = "SHAPE") %>%
  ggplot() +
  geom_sf(aes(fill = cor)) +
  labs(title = "SCI") +
  scale_fill_viridis_c(name = "Cor") +
  facet_wrap("season") +
  theme_void() +
  theme(legend.key.size = unit(0.5, "cm"), plot.title = element_text(hjust = 0.5),
        panel.grid = element_line(colour = "transparent"), text = element_text(size = 12),
        strip.text = element_text(size = 12), legend.text = element_text(size = 12))

INT_map <- df_oni_prov %>%
  filter(measurements == "INT") %>%
  rename("geometry" = "SHAPE") %>%
  ggplot() +
  geom_sf(aes(fill = cor)) +
  labs(title = "INT") +
  scale_fill_viridis_c(name = "Cor") +
  facet_wrap("season") +
  theme_void() +
  theme(legend.key.size = unit(0.5, "cm"), plot.title = element_text(hjust = 0.5),
        panel.grid = element_line(colour = "transparent"), text = element_text(size = 12),
        strip.text = element_text(size = 12), legend.text = element_text(size = 12))

INTs_map <- df_oni_prov %>%
  filter(measurements == "INTs") %>%
  rename("geometry" = "SHAPE") %>%
  ggplot() +
  geom_sf(aes(fill = cor)) +
  labs(title = "INTs") +
  scale_fill_viridis_c(name = "Cor") +
  facet_wrap("season") +
  theme_void() +
  theme(legend.key.size = unit(0.5, "cm"), plot.title = element_text(hjust = 0.5),
        panel.grid = element_line(colour = "transparent"), text = element_text(size = 12),
        strip.text = element_text(size = 12), legend.text = element_text(size = 12))

INTe_map <- df_oni_prov %>%
  filter(measurements == "INTe") %>%
  rename("geometry" = "SHAPE") %>%
  ggplot() +
  geom_sf(aes(fill = cor)) +
  labs(title = "INTe") +
  scale_fill_viridis_c(name = "Cor") +
  facet_wrap("season") +
  theme_void() +
  theme(legend.key.size = unit(0.5, "cm"), plot.title = element_text(hjust = 0.5),
        panel.grid = element_line(colour = "transparent"), text = element_text(size = 12),
        strip.text = element_text(size = 12), legend.text = element_text(size = 12))

ONI_map <- SCI_map + INT_map + INTs_map + INTe_map
ONI_map

## figure 7 topo effects - snow measurements against elevation
## by ECOPROVINCE
df_full %>%
  select(ECOPROVINCE_CODE, z, grep("SCI_20", colnames(df_full)), grep("INT_20", colnames(df_full)),
         grep("INTs_20", colnames(df_full)), grep("INTe_20", colnames(df_full))) %>%
  gather(key = "measurements", value = "value", -ECOPROVINCE_CODE, -z) %>%
  mutate(year = sub(".*_", "", measurements),
         measurements = sub("_.*", "", measurements),
         season = case_when(month == "Dec" | month == "Jan" | month == "Feb" ~ "Winter",
                            month == "Mar" | month == "Apr" | month == "May" ~ "Spring",
                            month == "Jun" | month == "Jul" | month == "Aug" ~ "Summer",
                            month == "Sep" | month == "Oct" | month == "Nov" ~ "Fall")) %>%
  ggplot(aes(value, z, colour = ECOPROVINCE_CODE)) +
  geom_point() +
  facet_wrap("measurements")

## by season
df_cal_long %>%
  left_join(df_full) %>%
  select(ECOPROVINCE_CODE, variable, doy, z, date) %>%
  mutate(month = month(date),
         season = case_when(month == 12 | month == 1 | month == 2 ~ "Winter",
                            month == 3 | month == 4 | month == 5 ~ "Spring",
                            month == 6 | month == 7 | month == 8 ~ "Summer",
                            month == 9 | month == 10 | month == 11 ~ "Fall")) %>%
  ggplot(aes(doy, z, colour = season)) +
  geom_point() +
  facet_wrap("variable")
