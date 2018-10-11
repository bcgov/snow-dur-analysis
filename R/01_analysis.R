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

library(plyr) # ddply function for correlation test
library(tidyverse) # data analysis and viz
library(rmapshaper) # simplify shapefiles for viz
library(bcmaps)
library(sf) # geospatial data processing

## importing cleaned data
## SCI - Snow Cover Index; INTe, INTs, INT: end/start/interval (since Sept 1) of snow season;
## COR_**** Correlation with tested variables; p_Season_Time: p-value
df_full <- read.csv("../data/snow/export_df.csv")

## removing NA Ecoprovince names
df_full <- df_full[complete.cases(df_full$ECOPROVINCE_NAME), ]

## reading in Oceanic Nino Index (ONI) data from 2002-2018 from
## http://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php
## column names indicate averaged results for three months
dfo <- read.csv("../data/snow/original_ONI.csv")

## extracting relevant columns and calculating average snow cover index for each ecoprovinces
df_prov <- df_full %>%
  select(ECOPROVINCE_NAME, grep("SCI_20", colnames(df_full))) %>%
  gather(key = variable, value = value, -ECOPROVINCE_NAME) %>%
  group_by(ECOPROVINCE_NAME, variable) %>%
  dplyr::summarise(SCI_avg = mean(value, na.rm = TRUE))

## for hydrozones
df_hydro <- df_full %>%
  select(HYDROLOGICZONE_NAME, grep("SCI_20", colnames(df_full))) %>%
  gather(key = variable, value = value, -HYDROLOGICZONE_NAME) %>%
  group_by(HYDROLOGICZONE_NAME, variable) %>%
  dplyr::summarise(SCI_avg = mean(value, na.rm = TRUE))

## snow start-end calender dataframe
df_cal <- select(df_full, ID, grep("INTs_20", colnames(df_full)), grep("INTe_20", colnames(df_full)))
df_cal_long <- df_cal %>%
  gather(key = variable, value = doy, -ID) %>%
  mutate(year = as.character(substr(variable, 6, 10)),
         variable = substr(variable, 1,4),
         date = as.Date(doy, origin = paste0(year, '-09-13')),
         day = substr(date, 6, 10),
         year = substr(date, 1, 4)) %>% # outputting correct year after setting origin date
  add_count(date) ## count distinguishes year+month+day

df_cal_long$variable <- factor(df_cal_long$variable, levels = c("INTs", "INTe"), labels = c("Start Date", "End Date"))

## no more year specification for x axis and as character for invidual tiles on plot
df_cal_long$day <- as.Date(df_cal_long$day, "%m-%d")
df_cal_long$day <- as.character(df_cal_long$day)

## remove NA
df_cal_long <- df_cal_long[complete.cases(df_cal_long$date), ]


## dfo analysis ####
## renaming columns with the middle month
colnames(dfo) <- c("year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

## replacing Jan - May ONI with consecutive year's data (hydrological year Sept - Jun,
## but summer season ends in May. Decision is made for complete seasonal summaries)
for (i in 1:(nrow(dfo)-1)) {
  dfo[i, c("Jan", "Feb", "Mar", "Apr", "May")] <- map(dfo[c("Jan", "Feb", "Mar", "Apr", "May")], i + 1)
}
dfo <- dfo[-nrow(dfo), ]
dfo$year <- as.character(dfo$year) # for joining dataframes later

## preparing dataframe for correlation test of ONI months with each snow measurement
df_oni_long <- df_full %>%
  select(ECOPROVINCE_CODE, grep("SCI_20", colnames(df_full)), grep("INT_20", colnames(df_full)),
         grep("INTs_20", colnames(df_full)), grep("INTe_20", colnames(df_full))) %>%
  gather(key = measurements, value = value, -ECOPROVINCE_CODE) %>%
  mutate(year = sub(".*_", "", measurements),
         measurements = sub("_.*", "", measurements)) %>%
  left_join(dfo) %>%
  gather(key = "month", value = "ONI", Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec) %>%
  mutate(season = case_when(month == "Dec" | month == "Jan" | month == "Feb" ~ "Winter",
                       month == "Mar" | month == "Apr" | month == "May" ~ "Spring",
                       month == "Jun" | month == "Jul" | month == "Aug" ~ "Summer",
                       month == "Sep" | month == "Oct" | month == "Nov" ~ "Fall"))

## formatting month-year and year columns as date class for ONI and SCI summaries
df_oni_long$monyear <- as.Date(paste(df_oni_long$year, "-", df_oni_long$month, "-01", sep = ""), format = "%Y-%b-%d")
df_oni_long$year <- as.Date(paste(df_oni_long$year, "-01-01", sep = ""), format = "%Y-%m-%d")

## Pearson correlation test by each ecoprovince
df_oni <- df_oni_long %>%
  ddply(.(ECOPROVINCE_CODE, measurements, month), mutate,
        "cor" = cor.test(value, ONI, method = "pearson")$estimate,
        "p_value" = cor.test(value, ONI, method = "pearson")$p.value) %>%
  ddply(.(ECOPROVINCE_CODE, measurements, season), mutate,
        "cor_seasonal" = cor.test(value, ONI, method = "pearson")$estimate,
        "p_value_seasonal" = cor.test(value, ONI, method = "pearson")$p.value) %>%
  group_by(ECOPROVINCE_CODE, season, measurements) %>%
  mutate(cor_min = min(cor), cor_max = max(cor))

## keeping only unique correlation records
df_oni <- subset(df_oni, !duplicated(df_oni$cor))

## dot map showing sum of snow cover index
df_dots <- df_full %>%
  select(c(ECOSECTION_NAME, grep("SCI_20", colnames(df_full)))) %>%
  gather(key = variable, value = value, -ECOSECTION_NAME) %>%
  group_by(ECOSECTION_NAME, variable) %>%
  dplyr::summarise(SCI_sum = sum(value, na.rm = TRUE))


## geospatial processing
## converting csv to sf
# df_bbl <- st_as_sf(df_bbl, coords = c("lon", "lat"))
# st_crs(df_bbl) <- st_crs(bc_bound())

## preparing for map viz
ecoprov <- st_intersection(ecoprovinces(), bc_bound())
ecoprov <- ms_simplify(ecoprov, keep = 0.02, keep_shapes = TRUE)
ecosec <- st_intersection(ecosections(), bc_bound())
ecosec <- ms_simplify(ecosec, keep = 0.02, keep_shapes = TRUE)
hydro <- st_intersection(hydrozones(), bc_bound())
hydro <- ms_simplify(hydro, keep = 0.02, keep_shapes = TRUE)

## joining spatial and tabular dataframes
df_prov <- left_join(ecoprov, df_prov, by = "ECOPROVINCE_NAME")
df_hydro <- left_join(hydro, df_hydro, by = "HYDROLOGICZONE_NAME")

df_dots <- left_join(ecosec, df_dots, by = "ECOSECTION_NAME")
df_dots <- df_dots %>%
  select(SCI_sum, SHAPE) %>%
  st_buffer(0) # multipolygon

## for ONI
df_oni_prov <- left_join(ecoprov, df_oni, by = "ECOPROVINCE_CODE")

## creating equal interval grids to combine with spatial dataframe
## dictates the density of dots, does not change grouped values
df_grid <- st_make_grid(df_dots, n= 50)

## interpolating and returning centroid for each grid cell
df_dots_map <- df_dots %>%
  st_interpolate_aw(to = df_grid, extensive = FALSE) %>%
  st_centroid()
