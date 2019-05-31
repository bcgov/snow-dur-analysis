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


#### 0: HEADER ####

  # CLEAR ENVIRONMENT
    rm(list=ls(all=TRUE))

  # SET WD LAPTOP vs WORK MACHINE
    setwd("G:/Dropbox/FLNRO_p1/Research_Cryosphere/Project_Snow/Git_Snow_MODIS/")
    list.dirs(recursive = F)

    #### 1: LOAD LIBRARIES ####

    library(reshape2)
    library(broom)
    library(tidyverse)
    library(lubridate)
    library(data.table)
    library(doBy)
    library(sf)
    library(sp)
    library(bcmaps)
    library(ggthemes);
    library(cowplot)
    library(scales)

    #### 2: IMPORT  ####

  # IMPORT TERRAIN per point

    # terrain = read.csv(list.files(path = "3_Modis_Output/20190211_loess_daily", pattern = "csv$", full.names = T))
    # names(terrain) = c("id", "aspect", "eastness", "elev", "lat", "lon", "northness", "slope", "water_mask_mode", ".geo")

  # IMPORT MODIS OUTPUT

    # files = list.files(path = "3_Modis_Output/20190211_loess_daily", pattern = "txt$", full.names = T)

    # dat = tibble()

    # for(file in files){
    #   csv = tibble::as_tibble(read.csv(file))
    #   csv = merge(csv %>% dplyr::select(-lat, -lon, -elev), terrain, by=c("id"))
    #   dat = rbind(dat, csv)}
    # 
    # remove(csv, file, files, terrain)

  # IMPORT MODIS FOR REVISED MANUSCRIPT (20190531)

    dat = read.csv("Data/MODIS/Derived/SD_Random_Sampls_withASTER.csv")
    
  # IMPORT TELECONNECTION DATA

    tel = as_tibble(read.csv("Data/TEL/original_TEL_events.csv"))

  # IMPORT ASWS / MODIS DIFFERENCE

    asws = as_tibble(read.csv("Data/ASWS/OUT/ASWS_Error_bw2_thr30.csv"))
    names(asws) = c("Station","Year","SD[ON]","SD[OFF]","SD[DUR]")

#### 3: DEFINE FUNCTIONS ####

  # LINEAR MODEL

    # Run Linear Model and LOOP n times with random adjustment, summarise mean slope difference
    # lm_iter(

    lm_iter = function(raw_df, groups, Y, X, textName)
      {

      # Run linear model on raw data
      lm_raw = raw_df %>%
        dplyr::group_by_(.dots = groups) %>%
        do(broom::tidy(lm(paste(Y,'~',X), data = ., na.action = na.omit))) %>%
        dplyr::filter(term != "(Intercept)")

      # Loop the same model n times for error estimation
      for(i in 1:n_iterations)
      {
        # i = 1
        # print(i)

        # Randomply sample the difference csv
        dif = asws[sample(nrow(asws),dim(raw_df %>% filter(measurement == "SD[ON]"))[1], replace = T),] %>% gather(measurement, asws_diff)
        # print(summary(dif))
        raw_df = raw_df %>% arrange(measurement)

        raw_df$asws_diff = dif$asws_diff


        raw_df$adj = raw_df[[Y]] + raw_df$asws_diff

        # Run linear model for adjusted days
        lm_adj = raw_df %>%
          dplyr::group_by_(.dots = groups) %>%
          do(broom::tidy(lm(paste("adj",'~',X), data = ., na.action = na.omit))) %>%
          dplyr::filter(term != "(Intercept)")

        # Collector: Join raw linear model to all adjusted models
        lm_raw   = full_join(
          x      = lm_raw,
          y      = lm_adj %>% dplyr::select(-statistic, -p.value, -std.error),
          by     = c(groups, "term"),
          suffix = c("",paste("_",as.character(i),sep="")))
        # print(lm_raw)
      }


      # Summarise iterated linear models as
      lm_summary = lm_raw %>%
        gather(iteration, estimate_n, contains("estimate_")) %>%
        dplyr::group_by_(.dots = c(groups, "estimate", "p.value")) %>%
        dplyr::summarise(est_iter_min  = min(estimate_n),
                         est_iter_q25  = quantile(estimate_n, .25),
                         est_iter_mean = mean(estimate_n),
                         est_iter_q75  = quantile(estimate_n, .75),
                         est_iter_max  = max(estimate_n)) %>%
        dplyr::mutate(error_minus = abs(estimate-est_iter_q25),
                      error_plus = abs(estimate-est_iter_q75),
                      error_mean = mean(error_minus, error_plus)) %>%
        dplyr::select(
          -est_iter_min,
          -est_iter_q25,
          -est_iter_mean,
          -est_iter_q75,
          -est_iter_max,
          -error_minus,
          -error_plus)
      # print(lm_summar)y
      filename = paste("5_Draft/Figures/", textName, "_", zone_exp, "_", n, "_", n_iterations, "_", format(x = now(), format = "%Y%m%d%H%M%S"), "_", Y, "_", X,".csv", sep = "")
      # print(filename)
      write.csv(x = lm_summary, file = filename)

        return(lm_summary)
      }

    cor_iter = function(raw_df, groups, Y, X, textName)
      {

      # Run linear model on raw data
      cor_raw = raw_df %>%
        dplyr::group_by_(.dots = groups) %>%
        dplyr::select(X, Y) %>%
        dplyr::rename(X = X, Y = Y) %>%
        dplyr::summarise(estimate = cor.test(Y, X, method = "spearman", exact=FALSE)$estimate,
                         p.value = cor.test(Y, X, method = "spearman", exact=FALSE)$p.value)

      # Loop the same model n times for error estimation
      for(i in 1:n_iterations)
      {
        # print(i)

        # Randomply sample the difference csv
        dif = asws[sample(nrow(asws),dim(raw_df %>% filter(measurement == "SD[ON]"))[1], replace = T),] %>% gather(measurement, asws_diff)
        # print(summary(dif))
        raw_df = raw_df %>% arrange(measurement)

        raw_df$asws_diff = dif$asws_diff


        raw_df$adj = raw_df[[Y]] + raw_df$asws_diff

        # Run linear model for adjusted days
        cor_adj = raw_df %>%
          dplyr::group_by_(.dots = groups) %>%
          dplyr::select(X, adj) %>%
          dplyr::rename(X = X, Y = adj) %>%
          dplyr::summarise(estimate = cor.test(Y, X, method = "spearman", exact=FALSE)$estimate,
                           p.value = cor.test(Y, X, method = "spearman", exact=FALSE)$p.value)

        # Collector: Join raw linear model to all adjusted models
        cor_raw   = full_join(
          x      = cor_raw,
          y      = cor_adj %>% dplyr::select(-p.value),
          by     = c(groups),
          suffix = c("",paste("_",as.character(i),sep="")))

        # print(i)
        # print(cor_raw)
      }


      # Summarise iterated linear models as
      cor_summary = cor_raw %>%
        gather(iteration, estimate_n, contains("estimate_")) %>%
        dplyr::group_by_(.dots = c(groups, "estimate", "p.value")) %>%
        dplyr::summarise(est_iter_q25  = quantile(estimate_n, .25),
                         est_iter_q75  = quantile(estimate_n, .75)) %>%
        dplyr::mutate(error_minus = abs(estimate-est_iter_q25),
                      error_plus = abs(estimate-est_iter_q75),
                      error_mean = error_minus+error_plus/2) %>%
        dplyr::select(
          -est_iter_q25,
          -est_iter_q75,
          -error_minus,
          -error_plus)

      # print(cor_summary)

      filename = paste("5_Draft/Figures/", textName, "_", zone_exp, "_", n, "_", n_iterations, "_", format(x = now(), format = "%Y%m%d%H%M%S"), "_", Y, "_", X,".csv", sep = "")

      write.csv(x = cor_summary, file = filename)

      return(cor_summary)
    }


# Plotting fucntions

    # Colors


    # General plotting function
    zone_plot = function(data,minLab,maxLab,intLab) {
      # data = mod_lm_zone_year_mean
      sig = merge(zones, filter(data, p.value <= 0.05), by = zone_name)
      not = merge(zones, filter(data, p.value > 0.05), by = zone_name)
      max = max(sig$estimate)
      min = min(sig$estimate)

      # zone_no = cbind(data.frame(coordinates(as(sig, "Spatial"))), sig$p.value)
      # names(zone_no) = c("x","y","sig")

      ggplot() +
        geom_sf(data = sig, aes(fill = estimate)) +
        geom_sf(data = not, fill = "grey") +
        # geom_point(data = zone_no, aes(x, y)) +
        # geom_text_repel(data = zone_no,aes(x = x, y = y, label = no), size = 2) +
        scale_fill_gradientn(colors= c("black","darkred","white","dodgerblue4","dodgerblue"), values=rescale(c(min,0,max)), limits=c(min,max), breaks = seq(minLab,maxLab,intLab)) +
        guides(fill = guide_colourbar(barwidth = 20, barheight = 1, direction = "horizontal", title.position = "top", frame.colour = "black", ticks.colour = "black")) +
        theme_few(base_size = 15) +
        theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), aspect.ratio = 1, panel.grid = element_line(linetype = 3, color = "light gray"), legend.position = "bottom", legend.title.align = 0.5)}

    