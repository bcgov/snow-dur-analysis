
zones= c("ALL BC",
  "NORTHERN COAST MOUNTAINS",
  "STIKINE PLATEAU",
  "NORTHERN ROCKY MOUNTAINS",
  "NORTHERN INTERIOR PLAINS",
  "NORTHERN CENTRAL UPLANDS",
  "SOUTHERN INTERIOR PLAINS",
  "SOUTHERN ROCKY MOUNTAIN FOOTHILLS",
  "NECHAKO PLATEAU",
  "SOUTHERN HAZELTON MOUNTAINS",
  "CENTRAL COAST MOUNTAINS",
  "HAIDA GWAII",
  "MCGREGOR BASIN",
  "UPPER FRASER BASIN",
  "NORTHERN COLUMBIA MOUNTAINS",
  "FRASER PLATEAU",
  "SOUTHERN QUESNEL HIGHLAND",
  "NORTHERN THOMPSON PLATEAU",
  "UPPER COLUMBIA BASIN",
  "UPPER KOOTENAY BASIN",
  "CENTRAL KOOTENAY BASIN",
  "LOWER KOOTENAY BASIN",
  "LOWER COLUMBIA BASIN",
  "OKANAGAN HIGHLAND",
  "SOUTHERN THOMPSON PLATEAU",
  "EASTERN SOUTH COAST MOUNTAINS",
  "CENTRAL SOUTH COAST MOUNTAINS",
  "WESTERN SOUTH COAST MOUNTAINS",
  "EASTERN VANCOUVER ISLAND",
  "WESTERN VANCOUVER ISLAND")


# BYZONE

  df_hz = df %>%
      dplyr::group_by_(.dots = c(zone_name, "year")) %>%
      dplyr::summarise(
        SD_ON     = mean(SD_ON, na.rm = T),
        SD_OFF    = mean(SD_OFF, na.rm = T),
        SD_INT    = mean(SD_INT, na.rm = T))

  ts_dev.mean = df_hz %>% ungroup() %>%
    group_by(HYDROLOGICZONE_NAME) %>%
    filter(!is.na(HYDROLOGICZONE_NAME)) %>%
    mutate(mSD_ON = mean(SD_ON)) %>%
    mutate(mSD_OFF = mean(SD_OFF)) %>%
    mutate(mSD_INT = mean(SD_INT)) %>%
    mutate(difON = SD_ON - mSD_ON) %>%
    mutate(difOFF = SD_OFF - mSD_OFF) %>%
    mutate(difINT = SD_INT - mSD_INT)

# ALL BC
  BC_ts_dev.mean =  df %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      SD_ON     = mean(SD_ON, na.rm = T),
      SD_OFF    = mean(SD_OFF, na.rm = T),
      SD_INT    = mean(SD_INT, na.rm = T)) %>%
    mutate(mSD_ON = mean(SD_ON)) %>%
    mutate(mSD_OFF = mean(SD_OFF)) %>%
    mutate(mSD_INT = mean(SD_INT)) %>%
    mutate(difON = SD_ON - mSD_ON) %>%
    mutate(difOFF = SD_OFF - mSD_OFF) %>%
    mutate(difINT = SD_INT - mSD_INT) %>%
    mutate(HYDROLOGICZONE_NAME = "ALL BC") %>%
    select(HYDROLOGICZONE_NAME, everything())

  bind = bind_rows(ts_dev.mean, BC_ts_dev.mean)

  bind.m = bind %>% gather("measurement", "value", contains("dif"))
  bind.m = bind.m %>% gather("mean_msm", "mean_value", contains("mS"))

  bind.m$measurement = ordered(bind.m$measurement, levels = c("difON","difOFF","difINT"), labels = msm_name)
  bind.m$mean_msm = ordered(bind.m$mean_msm, levels = c("mSD_ON","mSD_OFF","mSD_INT"), labels = msm_name)
  bind.m$HYDROLOGICZONE_NAME = ordered(factor(bind.m$HYDROLOGICZONE_NAME), levels = ordered(zones))

  bind.m.mean = bind.m %>% group_by(HYDROLOGICZONE_NAME, mean_msm) %>%
      select(contains("mean")) %>%
      summarize_all(mean) %>%
      rename(measurement = mean_msm)

# stat1 = df %>% filter(!is.na(HYDROLOGICZONE_NAME)) %>%
#     group_by(HYDROLOGICZONE_NAME, year) %>%
#     summarise(count = n()) %>%
#     summarise(count = mean(count))
# stat2 = df %>% filter(!is.na(HYDROLOGICZONE_NAME)) %>%
#     mutate(HYDROLOGICZONE_NAME = "ALL BC") %>%
#     group_by(HYDROLOGICZONE_NAME, year) %>%
#     summarise(count = n()) %>%
#     summarise(count = mean(count))
# stats = rbind(stat1, stat2)

ggplot(bind.m, aes(year, factor(HYDROLOGICZONE_NAME, levels = zones))) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value), color = value), show.legend = F) +
  geom_text(data = bind.m.mean, aes(group = HYDROLOGICZONE_NAME, x = 2019, y = factor(HYDROLOGICZONE_NAME), label = round(mean_value))) +
  # geom_text(data = stats, aes(group = HYDROLOGICZONE_NAME, x = 2021, y = factor(HYDROLOGICZONE_NAME), label = paste0("n=",round(count/1000)))) +

  scale_fill_gradientn(colors= c("black","darkred","white","dodgerblue4","dodgerblue")) + #, values=rescale(c(min(ts_dev.mean.m$x),0,max(data$x))), limits=c(min(data$x),max(data$x)), breaks = seq(minLab,maxLab,intLab)) +
  scale_color_gradientn(colors= c("dodgerblue","dodgerblue4","black","darkred","black")) + #, values=rescale(c(min(ts_dev.mean.m$x),0,max(data$x))), limits=c(min(data$x),max(data$x)), breaks = seq(minLab,maxLab,intLab)) +  scale_x_continuous(breaks = seq(2000,2020,2)) +
  scale_x_continuous(breaks = c(2005,2010,2015), labels = c("2005-06","2010-11","2015-16")) +

  guides(fill = guide_colourbar(barwidth = 20, barheight = 1, direction = "horizontal", title.position = "top", frame.colour = "black", ticks.colour = "black")) +
  labs(fill = "Difference from the mean (days)", x = "Hydrological Year") +

  ylim(rev(levels(bind.m$HYDROLOGICZONE_NAME))) +
  facet_wrap(~measurement, ncol = 1, labeller = labeller(measurement = label_parsed)) +
  theme_few(base_size = 15) +
  theme(aspect.ratio = 0.8, legend.position = "bottom", legend.title.align = 0.5, axis.title.y = element_blank(), plot.margin=grid::unit(c(0,0,0,0), "mm"))


ggsave(filename = paste(getwd(),"/5_Draft/Figures/", "fig06_snowseason_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 18, height = 18, device = "pdf")


