# Copyright 2019 Province of British Columbia
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

# library(bcmaps)
# install.packages('bcmapsdata', repos='https://bcgov.github.io/drat/', lib = "C:/Program Files/R/R-3.4.4/library")



#### 2: CLEAN  ####

  # Filter Elevation and Water Mask, Rename snow measurements, and group Z, asp, slp, and XY, also add unique ID

    df = dat %>%
      dplyr::filter(elev > 0) %>%
      dplyr::filter(!is.na(elev)) %>%
      dplyr::filter(!is.na(water_mask_mode)) %>%
      dplyr::filter(!is.na(aspect)) %>%
      dplyr::filter(!is.na(slope)) %>%
      dplyr::filter(!is.na(eastness)) %>%
      dplyr::filter(!is.na(northness)) %>%
      dplyr::filter(!is.na(lat)) %>%
      dplyr::filter(!is.na(lon)) %>%
      dplyr::filter(!is.na(water_mask_mode)) %>%
      dplyr::filter(water_mask_mode == 0) %>%
      dplyr::filter(obs > 0.3*max(obs)) %>%
      dplyr::rename(SD_ON = start_day) %>% #int_start
      dplyr::rename(SD_OFF = end_day) %>% #int_end
      dplyr::rename(SD_INT = snow_dur) %>% #intp_dur
      dplyr::filter(SD_ON != -9) %>% #intp_dur
      dplyr::filter(SD_OFF != -9) %>% #intp_dur
      dplyr::filter(SD_INT != -9) %>% #intp_dur
      mutate(grZ   = as.numeric(as.character(cut(elev, seq(0, 5000, 500), labels = seq(0, 5000-500, 500)))),
             grA   = as.numeric(as.character(cut(aspect, c(-1,45,90,135,180,225,270,315,361), labels = seq(0, 360-45, 45)))),
             grS   = as.numeric(as.character(cut(slope, c(-1, 10,20,30,40,50,60), labels = seq(0, 60-10, 10)))),
             grN   = as.numeric(as.character(cut(northness, seq(-1.4, 1.4, 0.4), labels = seq(-1, 1.4, 0.4)))),
             grE   = as.numeric(as.character(cut(eastness, seq(-1.4, 1.4, 0.4), labels = seq(-1, 1.4, 0.4)))),
             grLAT = as.numeric(as.character(cut(lat, seq(45, 62, 2), labels = seq(45, 62-2, 2)))),
             grLON = as.numeric(as.character(cut(lon, seq(-150, -110, 5), labels = seq(-150, -110-5, 5)))),
             UID   = round(abs((lat*1000) * (lon*1000)), digits = 3),
             typeS = as.character(cut(SD_INT, c(0, 0.07*365, 0.30*365, 0.90*365, 365),
                                      labels = c("nosnow","intermit","seasonal","permanent"))))

  # write.csv(df, paste(getwd(),"/5_Draft/Figures/", "dat01_df","_", format(x = now(), format = "%Y%m%d%H%M%S.csv"), sep = ""))


    summary(df)
    unique(df$grS)
  # Optional code to change SD_ON and SD_OFF from "days since 1-Sep to Julian days
    # SD_ON  = as.numeric(format(as.Date(paste(year, "-09-01", sep = ""))+SD_ON, "%j")),
    # SD_OFF = as.numeric(format(as.Date(paste(year, "-09-01", sep = ""))+SD_OFF, "%j")))

  # Counters
    n_iterations = 10000 #1000
    n = dim(df)[1]
    ns = paste(n,n_iterations, sep="_")

#### 3: BCMAPS ####

  # Load BC POLY
    bc = as(st_geometry(get_layer("bc_bound", class = "sf")), "Spatial")
    bc = transform_bc_albers(bc)

  # Transform DF to SpatialPointsDataFrame and project as ALBERS
    spdf = transform_bc_albers(SpatialPointsDataFrame(coords = df[,c("lon","lat")], data = df, proj4string = CRS("+init=epsg:4326")))

  # Clip DF to BC
    spdf = spdf[!is.na(over(spdf,bc)),]

  # Load HYDROZONES
    zones      = hydrozones()
    zone_name  = "HYDROLOGICZONE_NAME" #c("grZ","grLAT","grLON") #
    zone_exp   = paste("HZ",ns, sep="_")

  # Transform zones
    zones_alb  = transform_bc_albers(as(zones, "Spatial"))

  # Extract EcoregionCode by point
    zones_sel <- over(spdf, zones_alb[,zone_name])

  # flip back to DF
    df = as.data.frame(spdf)
    df = cbind(df, zones_sel)
    df = df %>% dplyr::filter(!is.na(zone_name))

  # CLEAN ENVIRONMENT
    remove(zones_sel, spdf, zones_alb, bc)

#### 4: TELECONNECTIONS ####

  # Conversions
    tel_list = c("oni","pdo") #,"oni_plus_pdo") #,"oni_event","pdo_event","oni_event_plus_pdo_event","oni_event_plus_pdo_event_both")
    tel_name = c("ONI","PDO") #,"ONI+PDO") #,"ONIe","PDOe","ONIe+PDOe","ONIe+PDOe_b")

  # Conversions
    msm_list = c("SD_ON","SD_OFF","SD_INT")#,"SD_SCI")
    msm_name = c("SD[ON]","SD[OFF]","SD[DUR]")#,"SD[SCI]")

  # Mean TEL by year
    tel_by_year = tel %>%
      dplyr::group_by(year) %>%
      dplyr::select(tel_list) %>%
      dplyr::filter(year >= 2002) %>%
      dplyr::summarise_all(mean, na.rm = T)

    # summary(lm(oni ~ year, tel_by_year))
    # summary(lm(pdo ~ year, tel_by_year))

    # Mean TEL by year, season
    tel_by_seas = tel %>%
      dplyr::group_by(year, season) %>%
      dplyr::select(tel_list) %>%
      dplyr::filter(year >= 2002) %>%
      dplyr::summarise_all(mean, na.rm = T)

  # Order Seasons
    tel_by_seas$season =
      ordered(tel_by_seas$season, levels = c("Spring","Summer","Fall","Winter"))

  # Summarise by zone
    df_zone = df %>%
      dplyr::group_by(HYDROLOGICZONE_NAME, year) %>%
      dplyr::summarise(
        SD_ON     = mean(SD_ON, na.rm = T),
        SD_OFF    = mean(SD_OFF, na.rm = T),
        SD_INT    = mean(SD_INT, na.rm = T))

  # Summarise by zone and elevation
    df_zoneZ = df %>%
      dplyr::group_by(HYDROLOGICZONE_NAME, grZ, year) %>%
      dplyr::summarise(
        SD_ON     = mean(SD_ON, na.rm = T),
        SD_OFF    = mean(SD_OFF, na.rm = T),
        SD_INT    = mean(SD_INT, na.rm = T))

    # MERGE tel - AND - dfM - BY - year
    tel_by_year_all   = merge(df, tel_by_year, by="year")
    tel_by_year_zone  = merge(df_zone, tel_by_year, by="year")
    tel_by_year_zoneZ  = merge(df_zoneZ, tel_by_year, by="year")

  # MERGE tel_seas - AND - dfM - BY - year
    tel_by_seas_all   = merge(df, tel_by_seas, by=c("year"))
    tel_by_seas_zone  = merge(df_zone, tel_by_seas, by="year")
    tel_by_seas_zoneZ = merge(df_zoneZ, tel_by_seas, by="year")

  # DFs to DATs
    df_all_tel_m       = data.table(melt(df,               measure.vars = msm_list, variable.name = "measurement", value.name = "days"))
    df_tel_year_all_m  = data.table(melt(tel_by_year_all,  measure.vars = msm_list, variable.name = "measurement", value.name = "days"))
    df_tel_seas_all_m  = data.table(melt(tel_by_seas_all,  measure.vars = msm_list, variable.name = "measurement", value.name = "days"))
    df_tel_year_zone_m = data.table(melt(tel_by_year_zone, measure.vars = msm_list, variable.name = "measurement", value.name = "days"))
    df_tel_seas_zone_m = data.table(melt(tel_by_seas_zone, measure.vars = msm_list, variable.name = "measurement", value.name = "days"))
    df_tel_year_zoneZ_m = data.table(melt(tel_by_year_zoneZ, measure.vars = msm_list, variable.name = "measurement", value.name = "days"))
    df_tel_seas_zoneZ_m = data.table(melt(tel_by_seas_zoneZ, measure.vars = msm_list, variable.name = "measurement", value.name = "days"))

  # ORDER FACTORS
    df_all_tel_m$measurement       = ordered(df_all_tel_m$measurement, levels = msm_list, labels = msm_name)
    df_tel_year_all_m$measurement  = ordered(df_tel_year_all_m$measurement, levels = msm_list, labels = msm_name)
    df_tel_seas_all_m$measurement  = ordered(df_tel_seas_all_m$measurement, levels = msm_list, labels = msm_name)
    df_tel_year_zone_m$measurement = ordered(df_tel_year_zone_m$measurement, levels = msm_list, labels = msm_name)
    df_tel_seas_zone_m$measurement = ordered(df_tel_seas_zone_m$measurement, levels = msm_list, labels = msm_name)
    df_tel_year_zoneZ_m$measurement = ordered(df_tel_year_zoneZ_m$measurement, levels = msm_list, labels = msm_name)
    df_tel_seas_zoneZ_m$measurement = ordered(df_tel_seas_zoneZ_m$measurement, levels = msm_list, labels = msm_name)


  # SUMMARISE MEAN VALUES PER YEAR

    df_BC_year_mean  = df_tel_year_all_m %>%
      group_by(year, measurement) %>%
      select(c("days", tel_list)) %>%
      summarise_all(mean, na.rm = T) %>%
      gather(key = tel, value = index, tel_list)

    df_BC_year_mean$tel = ordered(df_BC_year_mean$tel, levels = tel_list, labels = tel_name)

  # SUMMARISE MEAN VALUES PER YEAR+SEASON

    df_BC_seas_mean = df_tel_seas_all_m %>%
      group_by(year, measurement, season) %>%
      select( c("days", tel_list)) %>%
      summarise_all(mean, na.rm = T) %>%
      gather(key = tel, value = index, tel_list)

    df_BC_seas_mean$tel = ordered(df_BC_seas_mean$tel, levels = tel_list, labels = tel_name)

  # SUMMARISE MEAN VALUES PER YEAR

    df_zone_year_mean = df_tel_year_zone_m %>%
      group_by(year, measurement, HYDROLOGICZONE_NAME) %>%
      summarise_all(mean, na.rm = T) %>%
      gather(key = tel, value = index, tel_list)

    df_zone_year_mean$tel = ordered(df_zone_year_mean$tel, levels = tel_list, labels = tel_name)

  # SUMMARISE MEAN VALUES PER YEAR + SEASON

    df_zone_seas_mean = df_tel_seas_zone_m %>%
      group_by(year, season, measurement, HYDROLOGICZONE_NAME) %>%
      summarise_all(mean, na.rm = T) %>%
      gather(key = tel, value = index, tel_list)

    df_zone_seas_mean$tel = ordered(df_zone_seas_mean$tel, levels = tel_list, labels = tel_name)

  # SUMMARISE MEAN VALUES PER YEAR

    df_zoneZ_year_mean = df_tel_year_zoneZ_m %>%
      group_by(year, measurement, HYDROLOGICZONE_NAME, grZ) %>%
      summarise_all(mean, na.rm = T) %>%
      gather(key = tel, value = index, tel_list)

    df_zoneZ_year_mean$tel = ordered(df_zoneZ_year_mean$tel, levels = tel_list, labels = tel_name)

  # SUMMARISE MEAN VALUES PER YEAR + SEASON

    df_zoneZ_seas_mean = df_tel_seas_zoneZ_m %>%
      group_by(year, season, measurement, HYDROLOGICZONE_NAME, grZ) %>%
      summarise_all(mean, na.rm = T) %>%
      gather(key = tel, value = index, tel_list)

    df_zoneZ_seas_mean$tel = ordered(df_zoneZ_seas_mean$tel, levels = tel_list, labels = tel_name)

