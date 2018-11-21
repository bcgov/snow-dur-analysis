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
  theme_bw(base_size = 25, base_family = "Calibri") +
  theme(legend.position = "none", aspect.ratio = 0.3,
        panel.grid.major = element_line(linetype = 2, size = .4, color = "dark grey"),
        panel.grid.minor = element_line(linetype = 2, size = .4, color = "dark grey"))
dfo_plot2

dfo_plot <- dfo_plot1 + dfo_plot2 + plot_layout(ncol = 1)
dfo_plot
