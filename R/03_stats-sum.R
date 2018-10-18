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

## code for viewing the statistical summaries of various measurements to use for the paper report


## mean snow duration summaries by ecoprovince
sum_boxplot <- df_full %>%
  select(ECOPROVINCE_CODE, SCI_mean) %>%
  group_by(ECOPROVINCE_CODE) %>%
  dplyr::summarise(SCI_eco = mean(SCI_mean, na.rm = TRUE),
         SCI_median = median(SCI_eco, na.rm = TRUE),
         SCI_min = min(SCI_mean, na.rm = TRUE),
         SCI_max = max(SCI_mean, na.rm = TRUE))

## calendar plot dates for snow start and end by year
sum_cal <- df_cal_long %>%
  group_by(ECOPROVINCE_CODE, variable, year) %>%
  dplyr::summarise(earliest = min(date, na.rm = TRUE),
                   latest = max(date, na.rm = TRUE))

## largest number of sites that started and ended snowing each year
for (i in 2002:2018) {
  df <- subset(df_cal_long[df_cal_long$year == i & df_cal_long$variable == "Start Date", ])
  print(unique(df[df$n == max(df$n, na.rm = TRUE), "date"]))
}

for (i in 2002:2018) {
  df <- subset(df_cal_long[df_cal_long$year == i & df_cal_long$variable == "End Date", ])
  print(unique(df[df$n == max(df$n, na.rm = TRUE), "date"]))
}

## snow duration by year by ecoprovince
sum_dur <- df_full %>%
  select(ID, ECOPROVINCE_CODE, grep("INTs_20", colnames(df_full)), grep("INTe_20", colnames(df_full)),
         grep("INT_20", colnames(df_full))) %>% # for ordering
  gather(key = variable, value = doy, -ID, -ECOPROVINCE_CODE) %>%
  mutate(year = sub(".*_", "", variable), variable = sub("_.*", "", variable)) %>%
  group_by(ECOPROVINCE_CODE, variable, year) %>%
  summarise(doy = mean(doy, na.rm = TRUE))

## Seasonal ONI summaries
sum_oni <- df_oni_long %>%
  group_by(year, season) %>%
  summarise(seasonal_ONI = mean(ONI, na.rm = TRUE))
