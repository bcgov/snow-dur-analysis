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

# PLOT ASWS STATION VS MODIS ERROR

# CALIBRATION DATA (75%)
asws_cal.m= gather(asws,"measurement","value") %>% 
  mutate(dat = "Calibration (75%)")
asws_cal.m$measurement = ordered(asws_cal.m$measurement, levels = c("SD[ON]","SD[OFF]","SD[DUR]"), labels = msm_name) 

# VALIDATION DATA (25%)
asws_val.m= gather(asws_val,"measurement","value") %>% 
  mutate(dat = "Validation (25%)")
asws_val.m$measurement = ordered(asws_val.m$measurement, levels = c("SD[ON]","SD[OFF]","SD[DUR]"), labels = msm_name) 
  

# MERGE 
asws.m = rbind(asws_cal.m, asws_val.m)

# 
print("ME")
asws.m %>% group_by(measurement, dat) %>% 
  summarize(meanabs = mean(value))

print("MAE")
asws.m %>% group_by(measurement, dat) %>% 
  summarize(meanabs = mean(abs(value)))

ggplot(asws.m, aes(value)) +
  geom_density(aes(), fill = "grey", alpha = 0.8) +
  facet_grid(dat~measurement, labeller = labeller(measurement = label_parsed)) +
  # geom_vline(aes(xintercept = median(value))) +
  # geom_vline(aes(xintercept = mean(value)), linetype = 2) +
  theme_few(base_size = 20) +
  theme(aspect.ratio = 0.5) +
  scale_x_continuous(breaks = seq(-100,100,25), limits = c(-100,100)) +
  labs(x = "MODIS - ASWS Difference (days)", y = "Density")

ggsave(filename = paste(getwd(),"/Results/Figures/", "fig05_error_", ns, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 16, height = 7, device = "pdf")



# Conversions
season_name = c("Spring","Summer","Fall","Winter")
msm_name = c("SD[ON]","SD[OFF]","SD[DUR]")

#### FIGURE 7 ####

# PLOT LM COEFFICIENTS

mod_lm_BC_seas_mean = read.csv(list.files(path = "Results/Tables", pattern = "sup03", full.names = T))
mod_lm_BC_seas_mean$measurement = ordered(mod_lm_BC_seas_mean$measurement, levels = msm_name, labels = msm_name)
mod_lm_BC_seas_mean$season = ordered(mod_lm_BC_seas_mean$season, levels = season_name, labels = season_name)

lm = ggplot(mod_lm_BC_seas_mean, aes(group = season)) +
  geom_vline(xintercept=c(1.5,2.5,3.5), linetype=2, color = "gray")+
  geom_errorbar(aes(x = measurement, ymin = estimate - error_mean, ymax = estimate + error_mean), position = position_dodge(width=1)) +
  geom_point(aes(x = measurement, y = estimate, fill = cut(p.value, breaks = c(-Inf, 0.05, Inf), labels = c("<=0.05", ">0.05")), shape = season, group = season), position=position_dodge(width=1), size = 3) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("black","white")) +
  scale_shape_manual(values = c(21,22,23,24)) +
  scale_x_discrete(labels = parse(text = levels(mod_lm_BC_seas_mean$measurement))) +
  facet_wrap(~tel, ncol = 7) +
  theme_few(base_size = 25) +
  guides(fill = guide_legend(override.aes = list(shape=21, size = 3)), shape = guide_legend(override.aes = list(size = 3)))+
  theme(aspect.ratio = 1.6, legend.position = "left") +
  labs(x = "", y = expression(paste("LLS (d °",C^-1,")", sep = "")), fill = "Significance", shape = "Season")

# PLOT COR COEFFICIENTS

mod_cor_BC_seas_mean = read.csv(list.files(path = "Results/Tables", pattern = "sup04", full.names = T))
mod_cor_BC_seas_mean$measurement = ordered(mod_cor_BC_seas_mean$measurement, levels = msm_name, labels = msm_name)
mod_cor_BC_seas_mean$season = ordered(mod_cor_BC_seas_mean$season, levels = season_name, labels = season_name)

cor = ggplot(mod_cor_BC_seas_mean, aes(group = season)) +
  geom_vline(xintercept=c(1.5,2.5,3.5), linetype=2, color = "gray")+
  geom_errorbar(aes(x = measurement, ymin = estimate - error_mean, ymax = estimate + error_mean), position = position_dodge(width=1)) +
  geom_point(aes(x = measurement, y = estimate, fill = cut(p.value, breaks = c(-Inf, 0.05, Inf), labels = c("<=0.05", ">0.05")), shape = season, group = season), position=position_dodge(width=1), size = 3) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("black","white")) +
  scale_shape_manual(values = c(21,22,23,24)) +
  scale_x_discrete(labels = parse(text = levels(mod_lm_BC_seas_mean$measurement))) +
  facet_wrap(~tel, ncol = 7) +
  theme_few(base_size = 25) +
  guides(fill = guide_legend(override.aes = list(shape=21, size = 3)), shape = guide_legend(override.aes = list(size = 3)))+
  theme(aspect.ratio = 1.6) +
  labs(x = "", y = expression(italic("r")[S]), fill = "Significance", shape = "Season")

# SAVE FIG 7

both = gridExtra::grid.arrange(lm,cor,ncol=2); both
# ggsave(plot = both, filename = paste(getwd(),"/Results/Figures/", "fig07_mod_cor+lm_BC_seas_mean_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 20, height = 10, device = "pdf")


#### FIGURE 8 #####

# PLOT LM COEFFICIENTS

mod_lm_zone_seas_mean = read.csv(list.files(path = "Results/Tables", pattern = "sup07", full.names = T))
mod_lm_zone_seas_mean$measurement = ordered(mod_lm_zone_seas_mean$measurement, levels = msm_name, labels = msm_name)
mod_lm_zone_seas_mean$season = ordered(mod_lm_zone_seas_mean$season, levels = season_name, labels = season_name)

lm = zone_plot(mod_lm_zone_seas_mean, -30,30,5) +
  facet_grid(tel+season~measurement, labeller = labeller(measurement = label_parsed))   +
  labs(title = "", x = "", y = "", fill = expression(paste("LLS (d °",C^-1,")", sep = "")))

# PLOT COR COEFFICIENTS

mod_cor_zone_seas_mean = read.csv(list.files(path = "Results/Tables", pattern = "sup08", full.names = T))
mod_cor_zone_seas_mean$measurement = ordered(mod_cor_zone_seas_mean$measurement, levels = msm_name, labels = msm_name)
mod_cor_zone_seas_mean$season = ordered(mod_cor_zone_seas_mean$season, levels = season_name, labels = season_name)

cor = zone_plot(mod_cor_zone_seas_mean, -1,1,0.2) +
  facet_grid(tel+season~measurement, labeller = labeller(measurement = label_parsed))   +
  labs(title = "", x = "", y = "", fill = expression(italic("r")[S]))

# SAVE FIG 8

both = gridExtra::grid.arrange(lm,cor,ncol=2); both
# ggsave(plot = both, filename = paste(getwd(),"/Results/Figures/", "fig08_mod_cor+lm_zone_seas_mean_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 15, height = 18, device = "pdf")


#### FIGURE 10 ####

mod_cor_zone_year_mean = read.csv(list.files(path = "Results/Tables", pattern = "sup06", full.names = T))
mod_cor_zone_year_mean$measurement = ordered(mod_cor_zone_year_mean$measurement, levels = msm_name, labels = msm_name)
mod_cor_zone_year_mean$season = ordered(mod_cor_zone_year_mean$season, levels = season_name, labels = season_name)

most_imp_year_cor = mod_cor_zone_year_mean %>%
  mutate(sig = p.value <= 0.05) %>%
  filter(!is.na(HYDROLOGICZONE_NAME)) %>%
  group_by(.dots = c(zone_name, "measurement")) %>%
  filter(abs(estimate) == max(abs(estimate)))

ggplot() +
  geom_sf(data = filter(merge(zones, most_imp_year_cor,by=zone_name), sig == T), aes(fill = tel)) +
  geom_sf(data = filter(merge(zones, most_imp_year_cor,by=zone_name), sig == F), fill = "grey") +
  scale_fill_viridis_d() +
  facet_grid(~measurement, labeller = labeller(measurement = label_parsed)) +
  theme_few(base_size = 15) +
  theme(legend.position="bottom", axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), aspect.ratio = 1, panel.grid = element_line(linetype = 3, color = "light gray")) +
  labs(fill = "")

# ggsave(filename = paste(getwd(),"/Results/Figures/", "fig10_most_imp_year_cor_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 10, height = 5, device = "pdf")

#### FIGURE 9 ####

mod_cor_zone_seas_mean = read.csv(list.files(path = "Results/Tables", pattern = "sup08", full.names = T))
mod_cor_zone_seas_mean$measurement = ordered(mod_cor_zone_seas_mean$measurement, levels = msm_name, labels = msm_name)
mod_cor_zone_seas_mean$season = ordered(mod_cor_zone_seas_mean$season, levels = season_name, labels = season_name)

most_imp_seas_cor = mod_cor_zone_seas_mean %>%
  mutate(sig = p.value <= 0.05) %>%
  filter(!is.na(HYDROLOGICZONE_NAME)) %>%
  group_by_(.dots = c(zone_name, "measurement", "tel")) %>%
  filter(abs(estimate) == max(abs(estimate)))

ggplot() +
  geom_sf(data = filter(merge(zones,most_imp_seas_cor,by=zone_name), sig == T), aes(fill = season)) +
  geom_sf(data = filter(merge(zones,most_imp_seas_cor,by=zone_name), sig == F), fill = "grey") +
  scale_fill_viridis_d() +
  facet_grid(tel~measurement, labeller = labeller(measurement = label_parsed)) +
  theme_few(base_size = 15) +
  theme(legend.position="bottom", axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), aspect.ratio = 1, panel.grid = element_line(linetype = 3, color = "light gray")) +
  labs(fill = "")

# ggsave(filename = paste(getwd(),"/Results/Figures/", "fig09_most_imp_seas_cor_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 10, height = 7, device = "pdf")

#### Figure 11 ####

terrain_cor = function(raw_df, groups, textName) {
  cor_oni = raw_df %>% dplyr::group_by_(.dots = groups) %>%
    dplyr::summarise(estimate = cor.test(days, oni, method = "spearman", exact=FALSE)$estimate,
                     p.value = cor.test(days, oni, method = "spearman", exact=FALSE)$p.value)
  cor_pdo = raw_df %>% dplyr::group_by_(.dots = groups) %>%
    dplyr::summarise(estimate = cor.test(days, pdo, method = "spearman", exact=FALSE)$estimate,
                     p.value = cor.test(days, pdo, method = "spearman", exact=FALSE)$p.value)
  cor = rbind(cor_oni %>% mutate(tel = "ONI"),
              cor_pdo %>% mutate(tel = "PDO"))
  return(cor)
}


df_tel_year_all_m = df_tel_year_all_m %>% filter(!is.na(lat))

year_grZ_cor = terrain_cor(raw_df = df_tel_year_all_m, groups = c(zone_name,"grZ","measurement"), "grZ_cor")


terrain_lm = function(raw_df, groups, textName) {
  lm_oni = raw_df %>% dplyr::group_by_(.dots = groups) %>%
    do(broom::tidy(lm(days~oni, data = ., na.action = na.omit))) %>%
    dplyr::filter(term != "(Intercept)")
  lm_pdo = raw_df %>% dplyr::group_by_(.dots = groups) %>%
    do(broom::tidy(lm(days~pdo, data = ., na.action = na.omit))) %>%
    dplyr::filter(term != "(Intercept)")
  lm = rbind(lm_oni %>% mutate(tel = "ONI"),
             lm_pdo %>% mutate(tel = "PDO"))
  return(lm)
}


year_grZ_lm = terrain_lm(raw_df = df_tel_year_all_m, groups = c(zone_name,"grZ","measurement"), "grZ_cor")

give.cor = function(x){return(c(y = min(x)-0.1, label = length(x)))}
give.lm = function(x){return(c(y = min(x)-2, label = length(x)))}

library(gridExtra)
both = grid.arrange(
  ggplot(year_grZ_lm %>% filter(p.value <= 0.05), aes(as.factor(grZ), estimate)) +  geom_hline(yintercept = 0) +geom_boxplot(aes(fill = tel)) + facet_grid(~measurement, labeller = labeller(measurement = label_parsed)) + theme_few() + theme(aspect.ratio = 1, axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") + labs(x = "Elevation (m)", y = expression(paste("LLS (d °",C^-1,")", sep = "")), fill = "") + scale_fill_manual(values = c("gray","white")) +
    stat_summary(aes(group = tel), fun.data = give.lm, geom = "text", position = position_dodge(width = 1), size=3,  family="sans") + 
    theme(axis.title.x = element_blank()),
  ggplot(year_grZ_cor %>% filter(p.value <= 0.05), aes(as.factor(grZ), estimate)) +  geom_hline(yintercept = 0) +geom_boxplot(aes(fill = tel)) + facet_grid(~measurement, labeller = labeller(measurement = label_parsed)) + theme_few() + theme(aspect.ratio = 1, axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") + labs(x = "Elevation (m)", y = expression(italic("r")[S]), fill = "") + scale_fill_manual(values = c("gray","white")) +
    stat_summary(aes(group = tel), fun.data = give.cor, geom = "text", position = position_dodge(width = 1), size=3,  family="sans"),
  ncol = 1)

# ggsave(plot = both, filename = paste(getwd(),"/Results/Figures/", "fig11_boxplot_grZ_", zone_exp, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 8, height = 8, device = "pdf")

# write.csv(year_grZ_lm %>% filter(p.value <= 0.05), paste(getwd(),"/Results/Tables/", "sup19_year_grZ_lm", ns, "_", format(x = now(), format = "%Y%m%d%H%M%S.csv"), sep = ""))
# write.csv(year_grZ_cor %>% filter(p.value <= 0.05), paste(getwd(),"/Results/Tables/", "sup20_year_grZ_cor", ns, "_", format(x = now(), format = "%Y%m%d%H%M%S.csv"), sep = ""))

