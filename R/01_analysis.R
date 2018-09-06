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
library(reshape2) # melt function
library(rmapshaper) # simplify shapefiles for viz
library(bcmaps)
library(sf) # geospatial data processing

## importing cleaned data
## SCI - Snow Cover Index; INTe, INTs, INT: end/start/interval (since Sept 1) of snow season;
## COR_**** Correlation with tested variables; p_Season_Time: p-value
df_full <- read.csv("../data/export_df.csv")

## removing NA Ecoprovince names
df_full <- df_full[complete.cases(df_full$ECOPROVINCE_NAME), ]

## reading in Oceanic Nino Index (ONI) data from 2002-2018 from
## http://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php
## column names indicate averaged results for three months
dfo <- read.csv("../data/original_ONI.csv")

## df_full analysis ####
## correlation with ONI by point
# df_bbl <- df_full[, c("lon", "lat", "COR_Sprg_Start", "COR_Sum_Start", "COR_Fall_Start", "COR_Wint_Start")]
# df_bbl <- melt(df_bbl, id.vars = c("lon", "lat"))

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


## dfo analysis ####
## renaming columns with the middle month
colnames(dfo) <- c("year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

## replacing Jan - May ONI with consecutive year's data (hydrological year Sept - Jun,
## but summer season ends in May. Decision is made for complete seasonal summaries)
for (i in 1:(nrow(dfo)-1)) {
  dfo[i, c("Jan", "Feb", "Mar", "Apr", "May")] <- map(dfo[c("Jan", "Feb", "Mar", "Apr", "May")], i + 1)
}
dfo <- dfo[-nrow(dfo), ]

## preparing dataframe for correlation test of ONI months with each snow measurement
df_oni_long <- df_full %>%
  select(ECOPROVINCE_NAME, grep("SCI_20", colnames(df_full)), grep("INT_20", colnames(df_full)),
         grep("INTs_20", colnames(df_full)), grep("INTe_20", colnames(df_full))) %>%
  gather(key = measurements, value = value, -ECOPROVINCE_NAME)

## splitting measurement name and year
df_oni_long[, c("measurements", "year")] <- str_split_fixed(df_oni_long$measurements, "_", 2)

## merging with ONI dataframe and melting by month names
df_oni_long <- merge(df_oni_long, dfo)
df_oni_long <- melt(df_oni_long, id.vars = c("year", "ECOPROVINCE_NAME", "measurements", "value"),
                    variable.name = "month", value.name = "ONI")

## extracting hydrological season data from original dataframe
df_oni_long$season <- NA
df_oni_long[df_oni_long$month == "Dec" | df_oni_long$month == "Jan" | df_oni_long$month == "Feb", "season"] <- "Winter"
df_oni_long[df_oni_long$month == "Mar" | df_oni_long$month == "Apr" | df_oni_long$month == "May", "season"] <- "Spring"
df_oni_long[df_oni_long$month == "Jun" | df_oni_long$month == "Jul" | df_oni_long$month == "Aug", "season"] <- "Summer"
df_oni_long[df_oni_long$month == "Sep" | df_oni_long$month == "Oct" | df_oni_long$month == "Nov", "season"] <- "Fall"

## Pearson correlation test by each ecoprovince,
df_oni <- df_oni_long %>%
  ddply(.(ECOPROVINCE_NAME, measurements, month), mutate,
        "cor" = cor.test(value, ONI, method = "pearson")$estimate,
        "p_value" = cor.test(value, ONI, method = "pearson")$p.value) %>%
  ddply(.(ECOPROVINCE_NAME, measurements, season), mutate,
        "cor_seasonal" = cor.test(value, ONI, method = "pearson")$estimate,
        "p_value_seasonal" = cor.test(value, ONI, method = "pearson")$p.value)

## keeping only unique correlation records
df_oni <- subset(df_oni, !duplicated(df_oni$cor))

## seasonal ONI and snow measurement correlation, adding to original dataframe
df_oni_long <- df_oni_long %>%
  ddply(.(ECOPROVINCE_NAME, measurements, season), mutate,
        "cor" = cor.test(value, ONI, method = "pearson")$estimate,
        "p_value" = cor.test(value, ONI, method = "pearson")$p.value)


## geospatial processing
## converting csv to sf
# df_bbl <- st_as_sf(df_bbl, coords = c("lon", "lat"))
# st_crs(df_bbl) <- st_crs(bc_bound())

## preparing for map viz
ecoprov <- st_intersection(ecoprovinces(), bc_bound())
ecoprov <- ms_simplify(ecoprov, keep = 0.02, keep_shapes = TRUE)

## extracting relevant columns and calculating average snow cover index for each ecoprovinces
df_prov <- df_full %>%
  select(c(ECOPROVINCE_NAME, grep("SCI_20", colnames(df_full)))) %>%
  gather(key = variable, value = value, -ECOPROVINCE_NAME) %>%
  group_by(ECOPROVINCE_NAME, variable) %>%
  dplyr::summarise(SCI_avg = mean(value, na.rm = TRUE))

df_prov <- df_prov[!is.na(df_prov$ECOPROVINCE_NAME), ]

## joining spatial and tabular dataframes
df_prov <- left_join(ecoprov, df_prov, by = "ECOPROVINCE_NAME")

## for ONI
df_oni <- left_join(ecoprov, df_oni, by = "ECOPROVINCE_NAME")
