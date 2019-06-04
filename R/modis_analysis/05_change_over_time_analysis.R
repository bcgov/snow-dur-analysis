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

# files = list.files("G:/Dropbox/FLNRO_p1/Research_Cryosphere/Project_Snow/Paper_2018_snow_modis/Results/Tables", pattern = "csv")
# for(file in files){print(file)
#   out = paste(strsplit(file,"_")[[1]][1],strsplit(file,"_")[[1]][2],strsplit(file,"_")[[1]][3],strsplit(file,"_")[[1]][4],strsplit(file,"_")[[1]][5], sep = "_")
#   assign(out, read.csv(paste0("G:/Dropbox/FLNRO_p1/Research_Cryosphere/Project_Snow/Paper_2018_snow_modis/Results/Tables/",file)))}

# Function to test time series, and adjusted annual and seasonal time series

mod_lm_zone_year_mean = read.csv(list.files(path = "Results/Tables", pattern = "sup05", full.names = T))
mod_lm_zone_year_mean$measurement = ordered(mod_lm_zone_year_mean$measurement, levels = msm_name, labels = msm_name)


mod_lm_zoneZ_year_mean = read.csv(list.files(path = "Results/Tables", pattern = "sup09", full.names = T))
mod_lm_zoneZ_year_mean$measurement = ordered(mod_lm_zoneZ_year_mean$measurement, levels = msm_name, labels = msm_name)

mod_lm_zoneZ_seas_mean = read.csv(list.files(path = "Results/Tables", pattern = "sup11", full.names = T))
mod_lm_zoneZ_seas_mean$measurement = ordered(mod_lm_zoneZ_seas_mean$measurement, levels = msm_name, labels = msm_name)
mod_lm_zoneZ_seas_mean$season = ordered(mod_lm_zoneZ_seas_mean$season, levels = season_name, labels = season_name)


adjuster = function(raw_df, raw_lm, groups) {

  # raw_df = df_zone_year_mean
  # raw_lm = mod_lm_zone_year_mean %>% select(-year, -days, -index) %>% filter(!is.na(HYDROLOGICZONE_NAME))
  # groups = c(zone_name,"measurement")

# Linear regression of change over time for original data
  ts_original =  raw_df  %>%
    dplyr::group_by_(.dots = groups) %>%
    do(broom::tidy(lm(days ~ year, data = .))) %>%
    dplyr::filter(term == "year")

    print("ts_original")
    print(head(ts_original))

# Correct data using teleconnection relationships
  corrected_df = full_join(x = raw_lm, y = raw_df) %>% dplyr::mutate(days_corrected = days+(estimate*index))
  print("corrected_df")
  print(head(corrected_df))

  ts_corrected = corrected_df %>%
    dplyr::group_by_(.dots = c(groups,"tel")) %>%
    do(broom::tidy(lm(days_corrected ~ year, data = ., na.action = na.exclude))) %>%
    # do(broom::glance(lm(days_corrected ~ year, data = ., na.action = na.omit))) %>%
    dplyr::filter(term == "year");
  print("ts_corrected")
  print(head(ts_corrected))

  out = full_join(x = ts_original %>% mutate(tel = "RAW"), y = ts_corrected)
  print("out")
  print(head(out))

  return(out)
  }

give.n <- function(x){
  return(c(y = min(x)-0.1, label = length(x)))
  }

# Linear model for RAW and ADJUSTED data


  mod_lm_BC_year_mean = read.csv(list.files(path = "Results/Tables", pattern = "sup01", full.names = T))
  mod_lm_BC_year_mean$measurement = ordered(mod_lm_BC_year_mean$measurement, levels = msm_name, labels = msm_name)
  
  # All BC annual
  ts_BC_year = adjuster(raw_df = df_BC_year_mean,
                        raw_lm = mod_lm_BC_year_mean,
                        groups = c("measurement"))
                        # write.csv(ts_BC_year, paste(getwd(),"/Results/Tables/", "sup13_ts_BC_year", ns, "_", format(x = now(), format = "%Y%m%d%H%M%S.csv"), sep = ""))
  # All BC seasonal
  ts_BC_seas = adjuster(raw_df = df_BC_seas_mean,
                        raw_lm = mod_lm_BC_seas_mean,
                        groups = c("measurement","season"))
                        # write.csv(ts_BC_seas, paste(getwd(),"/Results/Tables/", "sup14_ts_BC_seas", ns, "_", format(x = now(), format = "%Y%m%d%H%M%S.csv"), sep = ""))
  # Hydrozones annual
  ts_HZ_year = adjuster(raw_df = df_zone_year_mean %>% filter(!is.na(HYDROLOGICZONE_NAME)),
                        raw_lm = mod_lm_zone_year_mean %>% filter(!is.na(HYDROLOGICZONE_NAME)),
                        groups = c(zone_name,"measurement"))
                        # write.csv(ts_HZ_year, paste(getwd(),"/Results/Tables/", "sup15_ts_HZ_year", ns, "_", format(x = now(), format = "%Y%m%d%H%M%S.csv"), sep = ""))
  # Hydrozones seasonal
  ts_HZ_seas = adjuster(raw_df = df_zone_seas_mean %>% filter(!is.na(HYDROLOGICZONE_NAME)),
                        raw_lm = mod_lm_zone_seas_mean %>% filter(!is.na(HYDROLOGICZONE_NAME)),
                        groups = c(zone_name,"measurement","season"))
                        # write.csv(ts_HZ_seas, paste(getwd(),"/Results/Tables/", "sup16_ts_HZ_seas", ns, "_", format(x = now(), format = "%Y%m%d%H%M%S.csv"), sep = ""))
  # Hydrozones / elevation annual
  ts_HZgrZ_year = adjuster(raw_df = df_zoneZ_year_mean %>% filter(!is.na(HYDROLOGICZONE_NAME)),
                           raw_lm = mod_lm_zoneZ_year_mean %>% filter(!is.na(HYDROLOGICZONE_NAME)),
                           groups = c(zone_name,"grZ","measurement"))
                           # write.csv(ts_HZgrZ_year, paste(getwd(),"/Results/Tables/", "sup17_ts_HZgrZ_year", ns, "_", format(x = now(), format = "%Y%m%d%H%M%S.csv"), sep = ""))
  # Hydrozones / elevation seasonal
  ts_HZgrZ_seas = adjuster(raw_df = df_zoneZ_seas_mean %>% filter(!is.na(HYDROLOGICZONE_NAME)),
                           raw_lm = mod_lm_zoneZ_seas_mean %>% filter(!is.na(HYDROLOGICZONE_NAME)),
                           groups = c(zone_name,"grZ","measurement","season"))
                           write.csv(ts_HZgrZ_seas, paste(getwd(),"/Results/Tables/", "sup18_ts_HZgrZ_seas", ns, "_", format(x = now(), format = "%Y%m%d%H%M%S.csv"), sep = ""))

# Plotting


  # PLOT RAW DATA
  zone_plot(ts_HZ_year %>% filter(tel == "RAW"), -2,2,0.5) +
    facet_grid(~measurement, labeller = labeller(measurement = label_parsed))   +
    labs(title = "ts_HZ_seas_raw", x = "", y = "", fill = expression(paste("LLS (d °",yr^-1,")", sep = "")))

  # PLOT CORRECTED ANNUAL HYDROZONE
  zone_plot(ts_HZ_year %>% filter(tel != "RAW"), -2,2,0.5) +
    facet_grid(~measurement, labeller = labeller(measurement = label_parsed))   +
    labs(title = "ts_HZ_seas_raw", x = "", y = "", fill = expression(paste("LLS (d °",yr^-1,")", sep = "")))

  # PLOT CORRECTED SEASONAL HYDROZONE
  zone_plot(ts_HZ_seas %>% filter(tel != "RAW"), -2,2,0.5) +
    facet_grid(tel+season~measurement, labeller = labeller(measurement = label_parsed))   +
    labs(title = "mod_lm_zone_seas_mean", x = "", y = "", fill = expression(paste("LLS (d °",yr^-1,")", sep = "")))

  # PLOT CORRECTED SEASONAL HYDROZONE AND ELEVATION
  zone_plot(ts_HZgrZ_seas %>% filter(tel == "RAW"), -2,2,0.5) + 
    facet_grid(measurement~grZ, labeller = labeller(measurement = label_parsed))   +
    labs(x = "", y = "", fill = expression(paste("LLS (d ",yr^-1,")", sep = "")))

    ggsave(filename = paste(getwd(),"/Results/Figures/", "fig12_ts_HZgrZ_raw", ns, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), height = 7, width = 12, device = "pdf")

  # PLOT CORRECTED SEASONAL HYDROZONE AND ELEVATION
  zone_plot(ts_HZgrZ_seas %>% filter(tel != "RAW"), -2,2,0.5) +
    facet_grid(season~measurement+grZ, labeller = labeller(measurement = label_parsed))   +
    labs(title = "mod_lm_zone_seas_mean", x = "", y = "", fill = expression(paste("LLS (d °",C^-1,")", sep = "")))

