
lm_eqn <- function(df, x, y){
    m <- lm(y ~ x, df);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
         list(a = format(unname(coef(m)[1]), digits = 2),
              b = format(unname(coef(m)[2]), digits = 2),
             r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq))}

# dfZ = df %>% 
#     select(-lat.1, -lon.1, -HYDROLOGICZONE_NAME, -grZ,-SD_OBS) %>% 
#     melt(id.vars = c("UID","year","lat","lon","z")) %>% 
#     mutate(y = value, x = z)
# dflat = df %>% 
#     select(-lat.1, -lon.1, -HYDROLOGICZONE_NAME, -grZ,-SD_OBS) %>% 
#     melt(id.vars = c("UID","year","lat","lon","z")) %>% 
#     mutate(y = value, x = lat)
# dflon = df %>% 
#     select(-lat.1, -lon.1, -HYDROLOGICZONE_NAME, -grZ,-SD_OBS) %>% 
#     melt(id.vars = c("UID","year","lat","lon","z")) %>% 
#     mutate(y = value, x = lon)


df_mean = df %>% 
  select(-HYDROLOGICZONE_NAME) %>% 
  # filter(UID %in% sample(unique(df$UID), 1000)) %>% 
  group_by(UID) %>% 
  summarise_all(mean) 

  
plots = gridExtra::grid.arrange(
  
    ggplot(df_mean) + 
      
    # geom_smooth(aes(z, SD_ON, color = "SD_ON"), se = F, method = "lm", linetype = 2) +
    geom_smooth(aes(z, SD_ON, color = "SD_ON"), se = F, linetype = 1) + 
    # geom_text(x = 500, y = 300, color = "red", label = lm_eqn(df = df, x = df$z, y = df$SD_ON), parse = TRUE) +
    
    # geom_smooth(aes(z, SD_OFF, color = "SD_OFF"), se = F, method = "lm", linetype = 2) +
    geom_smooth(aes(z, SD_OFF, color = "SD_OFF"), se = F, linetype = 1) + 
    # geom_text(x = 500, y = 280, color = "blue", label = lm_eqn(df = df, x = df$z, y = df$SD_OFF), parse = TRUE) +
  
    # geom_smooth(aes(z, SD_DUR, color = "SD_DUR"), se = F, method = "lm", linetype = 2) +
    geom_smooth(aes(z, SD_DUR, color = "SD_DUR"), se = F, linetype = 1) + 
    # geom_text(x = 500, y = 260, color = "black", label = lm_eqn(df = df, x = df$z, y = df$SD_DUR), parse = TRUE) +
  
    scale_color_manual(values = c("red","blue","black"), labels = c(expression(SD[DUR]),expression(SD[OFF]),expression(SD[ON]))) +
    scale_linetype_manual(values = c(2,1)) +
    theme_bw() +
    theme(aspect.ratio = 1, panel.grid = element_blank(), legend.position = c(0.2, 0.8)) +
    labs(title = "A", x = "Elevation (m asl)", y = bquote('Days since 1-Sep for'~SD[ON]~"&"~SD[OFF]~" or days for"~SD[DUR]), color = "Measurement")
   ,
    ggplot(df_mean) + 

    # geom_smooth(aes(lat, SD_ON, color = "SD_ON"), se = F, method = "lm", linetype = 2) +
    geom_smooth(aes(lat, SD_ON, color = "SD_ON"), se = F, linetype = 1) + 
    # geom_text(x = 500, y = 300, color = "red", label = lm_eqn(df = df, x = df$lat, y = df$SD_ON), parse = TRUE) +
    
    # geom_smooth(aes(lat, SD_OFF, color = "SD_OFF"), se = F, method = "lm", linetype = 2) +
    geom_smooth(aes(lat, SD_OFF, color = "SD_OFF"), se = F, linetype = 1) + 
    # geom_text(x = 500, y = 280, color = "blue", label = lm_eqn(df = df, x = df$lat, y = df$SD_OFF), parse = TRUE) +
  
    # geom_smooth(aes(lat, SD_DUR, color = "SD_DUR"), se = F, method = "lm", linetype = 2) +
    geom_smooth(aes(lat, SD_DUR, color = "SD_DUR"), se = F, linetype = 1) + 
    # geom_text(x = 500, y = 260, color = "black", label = lm_eqn(df = df, x = df$lat, y = df$SD_DUR), parse = TRUE) +
  
    scale_color_manual(values = c("red","blue","black"), labels = c(expression(SD[DUR]),expression(SD[OFF]),expression(SD[ON]))) +
    scale_linetype_manual(values = c(2,1)) +
    theme_bw() +
    theme(aspect.ratio = 1, panel.grid = element_blank(), legend.position = 'none') +
    labs(title = "B", x = "Latitude (°)", y = "")#bquote('Days since 1-Sep for'~SD[ON]~"&"~SD[OFF]~" or Days for"~SD[DUR]))
  ,
    ggplot(df_mean) + 
    
    # geom_smooth(aes(lon, SD_ON, color = "SD_ON"), se = F, method = "lm", linetype = 2) +
    geom_smooth(aes(lon, SD_ON, color = "SD_ON"), se = F, linetype = 1) + 
    # geom_text(x = 500, y = 300, color = "red", label = lm_eqn(df = df, x = df$lon, y = df$SD_ON), parse = TRUE) +
    
    # geom_smooth(aes(lon, SD_OFF, color = "SD_OFF"), se = F, method = "lm", linetype = 2) +
    geom_smooth(aes(lon, SD_OFF, color = "SD_OFF"), se = F, linetype = 1) + 
    # geom_text(x = 500, y = 280, color = "blue", label = lm_eqn(df = df, x = df$lon, y = df$SD_OFF), parse = TRUE) +
  
    # geom_smooth(aes(lon, SD_DUR, color = "SD_DUR"), se = F, method = "lm", linetype = 2) +
    geom_smooth(aes(lon, SD_DUR, color = "SD_DUR"), se = F, linetype = 1) + 
    # geom_text(x = 500, y = 260, color = "black", label = lm_eqn(df = df, x = df$lon, y = df$SD_DUR), parse = TRUE) +
  
    scale_color_manual(values = c("red","blue","black"), labels = c(expression(SD[DUR]),expression(SD[OFF]),expression(SD[ON]))) +
    scale_linetype_manual(values = c(2,1)) +
    
    theme_bw() +
    theme(aspect.ratio = 1, panel.grid = element_blank(), legend.position = 'none') +
    labs(title = "C", x = "Longitude (°)", y = "")#bquote('Days since 1-Sep for'~SD[ON]~"&"~SD[OFF]~" or Days for"~SD[DUR]))
  ,
    ncol=3); plots

ggsave(plot = plots, filename = paste(getwd(),"/Results/Figures/", "sd_zlatlon", ns, "_", format(x = now(), format = "%Y%m%d%H%M%S.pdf"), sep = ""), width = 12, height = 5, device = "pdf")

df_mean %>%
  do(broom::tidy(lm(SD_ON ~ z, data = ., na.action = na.omit))) %>% dplyr::filter(term != "(Intercept)")
df_mean %>%
  do(broom::tidy(lm(SD_OFF ~ z, data = ., na.action = na.omit))) %>% dplyr::filter(term != "(Intercept)") 
df_mean %>%
  do(broom::tidy(lm(SD_DUR ~ z, data = ., na.action = na.omit))) %>% dplyr::filter(term != "(Intercept)") 

df_mean %>%
  do(broom::tidy(lm(SD_ON ~ lat, data = ., na.action = na.omit))) %>% dplyr::filter(term != "(Intercept)")
df_mean %>%
  do(broom::tidy(lm(SD_OFF ~ lat, data = ., na.action = na.omit))) %>% dplyr::filter(term != "(Intercept)") 
df_mean %>%
  do(broom::tidy(lm(SD_DUR ~ lat, data = ., na.action = na.omit))) %>% dplyr::filter(term != "(Intercept)") 

df_mean %>%
  do(broom::tidy(lm(SD_ON ~ lon, data = ., na.action = na.omit))) %>% dplyr::filter(term != "(Intercept)")
df_mean %>%
  do(broom::tidy(lm(SD_OFF ~ lon, data = ., na.action = na.omit))) %>% dplyr::filter(term != "(Intercept)") 
df_mean %>%
  do(broom::tidy(lm(SD_DUR ~ lon, data = ., na.action = na.omit))) %>% dplyr::filter(term != "(Intercept)") 


normalizz = function(x){(x-min(x))/(max(x)-min(x))}

df_mean = df_mean %>% mutate(
  z_n = normalizz(z),
  lat_n = normalizz(lat),
  lon_n = normalizz(lon))

df_mean %>%
  do(broom::tidy(lm(SD_DUR ~ z_n+lat_n+lon_n, data = ., na.action = na.omit))) %>% dplyr::filter(term != "(Intercept)") 

# gridExtra::grid.arrange(
#   
#     ggplot(dfZ, aes(x, y)) + 
#       geom_hex() + 
#       geom_smooth(method = "lm", color = "red") + 
#       geom_smooth(method = "loess", color = "red") + 
#       # geom_text(x = 25, y = 300, label = lm_eqn(dfZ), parse = TRUE) + 
#       facet_wrap(~variable, ncol = 4) + #, labeller = labeller(variable = label_parsed)
#       scale_fill_viridis_c(limits = c(0,15000), breaks = seq(0,15000,3000)) +
#       guides(fill = guide_colourbar(barwidth = 1, barheight = 10, direction = "vertical", title.position = "top", frame.colour = "black", ticks.colour = "black")) +
#       theme_bw() +
#       theme(aspect.ratio = 1, panel.grid = element_blank()) + 
#       labs(x = "Elevation (m asl)", y = "Day of year or Days", fill = "Dentsity"),
#     
#     ggplot(dflat, aes(x, y)) + 
#       geom_hex() + 
#       geom_smooth(method = "lm", color = "red") + 
#       geom_smooth(method = "loess", color = "red") + 
#       # geom_text(x = 25, y = 300, label = lm_eqn(dfZ), parse = TRUE) + 
#       facet_wrap(~variable, ncol = 4) + #, labeller = labeller(variable = label_parsed)
#       scale_fill_viridis_c(limits = c(0,15000), breaks = seq(0,15000,3000)) +
#       guides(fill = guide_colourbar(barwidth = 1, barheight = 10, direction = "vertical", title.position = "top", frame.colour = "black", ticks.colour = "black")) +
#       theme_bw() +
#       theme(aspect.ratio = 1, panel.grid = element_blank()) + 
#       labs(x = "Latitude (degrees)", y = "Day of year or Days", fill = "Dentsity"),
#     
#     ggplot(dflon, aes(x, y)) + 
#       geom_hex() + 
#       geom_smooth(method = "lm", color = "red") + 
#       geom_smooth(method = "loess", color = "red") + 
#       # geom_text(x = 25, y = 300, label = lm_eqn(dfZ), parse = TRUE) + 
#       facet_wrap(~variable, ncol = 4) + #, labeller = labeller(variable = label_parsed)
#       scale_fill_viridis_c(limits = c(0,15000), breaks = seq(0,15000,3000)) +
#       guides(fill = guide_colourbar(barwidth = 1, barheight = 10, direction = "vertical", title.position = "top", frame.colour = "black", ticks.colour = "black")) +
#       theme_bw() +
#       theme(aspect.ratio = 1, panel.grid = element_blank()) + 
#       labs(x = "Longitude (degrees)", y = "Day of year or Days", fill = "Dentsity"),
#     
#     ncol =1)
# 
# 
#     