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


library(dplyr) # data analysis
library(tidyr) # gather function
library(purrr) # extracting single elements from dataframe
library(reshape2) # melt function
library(rmapshaper) # simplify shapefiles for viz
library(bcmaps)
library(sf) # geospatial data processing

## importing cleaned data
## SCI - Snow Cover Index; INTe, INTs, INT: end/start/interval (since Sept 1) of snow season;
## COR_**** Correlation with tested variables; p_Season_Time: p-value
df_full <- read.csv("../data/export_df.csv")

## reading in Oceanic Nino Index (ONI) data from 2002-2018 from
## http://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php
## column names indicate averaged results for three months
dfo <- read.csv("../data/original_ONI.csv")

## renaming columns with the middle month
# colnames(dfo)[(grep("^[A-Z]{3}$", colnames(dfo)))] <- substr(colnames(dfo)[(grep("^[A-Z]{3}$", colnames(dfo)))], 2, 2)
colnames(dfo) <- c("year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

## replacing Jan - May ONI with consecutive year's data (hydrological year Sept - Jun,
## but summer season ends in May. Decision is made for complete seasonal summaries)
for (i in 1:(nrow(dfo)-1)) {
  dfo[i, c("Jan", "Feb", "Mar", "Apr", "May")] <- map(dfo[c("Jan", "Feb", "Mar", "Apr", "May")], i + 1)
}

## correlation with ONI by point
df_bbl <- df_full[, c("lon", "lat", "COR_Sprg_Start", "COR_Sum_Start", "COR_Fall_Start", "COR_Wint_Start")]
df_bbl <- melt(df_bbl, id.vars = c("lon", "lat"))

## snow start-end calender dataframe
df_cal <- select(df_full, ID, grep("INTs_20", colnames(df_full)), grep("INTe_20", colnames(df_full)))
df_cal_long <- df_cal %>%
  gather(key = variable, value = doy, -ID) %>%
  mutate(year = as.character(substr(variable, 6, 10)),
         variable = substr(variable, 1,4),
         date = as.Date(doy, origin = paste0(year, '-09-01')),
         day = substr(date, 6, 10)) %>%
  add_count(date) ## count distinguishes year+month+day

df_cal_long$variable <- factor(df_cal_long$variable, levels = c("INTs", "INTe"), labels = c("Start Date", "End Date"))

## no more year specification for x axis and as character for invidual tiles on plot
df_cal_long$day <- as.Date(df_cal_long$day, "%m-%d")
df_cal_long$day <- as.character(df_cal_long$day)

## remove NA
df_cal_long <- df_cal_long[complete.cases(df_cal_long$date), ]

## extracting hydrological season data from original dataframe
dfo$season <- NA
dfo[dfo$ONI == "DJF", "season"] <- "Winter"
dfo[dfo$ONI == "MAM", "season"] <- "Spring"
dfo[dfo$ONI == "JJA", "season"] <- "Summer"
dfo[dfo$ONI == "SON", "season"] <- "Fall"

df_oni <- dfo[!is.na(dfo$season), ]


## geospatial processing
## converting csv to sf
df_bbl <- st_as_sf(df_bbl, coords = c("lon", "lat"))
st_crs(df_bbl) <- st_crs(bc_bound())

## preparing for map viz
ecoprov <- st_intersection(ecoprovinces(), bc_bound())
ecoprov <- ms_simplify(ecoprov, keep = 0.02, keep_shapes = TRUE)

## extracting relevant columns and calculating average snow cover index for each ecoprovinces
df_prov <- df_full %>%
  select(c(ID, grep("SCI_20", colnames(df_full)), ECOPROVINCE_NAME)) %>%
  gather(key = variable, value = value, grep("SCI_20", colnames(df_full))) %>%
  group_by(ECOPROVINCE_NAME, variable) %>%
  summarise(SCI_avg = mean(value, na.rm = TRUE))

df_prov <- df_prov[!is.na(df_prov$ECOPROVINCE_NAME), ]

## joining spatial and tabular dataframes
df_prov <- left_join(ecoprov, df_prov, by = "ECOPROVINCE_NAME")

## for ONI
df_oni <- left_join(ecoprov, df_oni, by = "ECOPROVINCE_NAME")

