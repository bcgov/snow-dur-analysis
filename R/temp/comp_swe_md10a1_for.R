library(tidyverse)

rm(list=ls(all=T))

data <- read.csv("C:/Users/hgleason/Dropbox/Git_Snow_MODIS/Data/ASWS/OUT/Snow_Dur_Comp_it1_dlta3_for_spln.csv")

data <- subset(data,mddur>=0 & sddur>=0)

summary(data)

data %>% 
  dplyr::filter(mdon >=0 ) %>%
  ggplot() + geom_histogram(aes(mdon),color = "blue", fill = "pink")

data %>% 
  dplyr::filter(mdoff >=0) %>%
  ggplot() + geom_histogram(aes(mdoff),color = "blue", fill = "pink")

data %>% 
  dplyr::filter(mddur >=0) %>%
  ggplot() + geom_histogram(aes(mddur),color = "blue", fill = "pink")


data %>% 
  dplyr::filter(sdon >=0 ) %>%
  ggplot() + geom_histogram(aes(sdon),color = "blue", fill = "pink")

data %>% 
  dplyr::filter(sdoff >=0) %>%
  ggplot() + geom_histogram(aes(sdoff),color = "blue", fill = "pink")

data %>% 
  dplyr::filter(sddur >=0) %>%
  ggplot() + geom_histogram(aes(sddur),color = "blue", fill = "pink")



unbia <- data %>% group_by(station) %>%
  mutate(ondif = sdon - mdon) %>%
  mutate(onbias = median(ondif)) %>%
  mutate(offdif = sdoff - mdoff) %>%
  mutate(offbias = median(offdif)) %>%
  mutate(cor_on = ondif-onbias)%>%
  mutate(cor_off= offdif-offbias)


unbia %>% 
  ggplot() + geom_boxplot(aes(station,ondif)) + theme(axis.text.x = element_text(angle = 45)) 

unbia %>% 
  ggplot() + geom_boxplot(aes(station,cor_on)) + theme(axis.text.x = element_text(angle = 45))  

unbia %>% 
  ggplot() + geom_boxplot(aes(station,offdif)) + theme(axis.text.x = element_text(angle = 45)) 

unbia %>% 
  ggplot() + geom_boxplot(aes(station,cor_off)) + theme(axis.text.x = element_text(angle = 45))  


unbia$comb_diff = abs(unbia$cor_on)+abs(unbia$cor_off)


result<-unbia %>%
  ungroup()%>%
  group_by(bw,ndsi_str,ndsi_end)%>%
  summarise(med_ab_dif = median(comb_diff), sd_comb = sd(cor_on)+sd(cor_off) )


unbia %>% 
  ungroup() %>%
  dplyr::filter(mddur >=0 & bw == 0.3 & ndsi_str==35 & ndsi_end==30)  %>%
  ggplot() + geom_histogram(aes(cor_on),color = "blue", fill = "pink")


unbia %>% 
  ungroup() %>%
  dplyr::filter(mddur >=0 & bw == 0.3 & ndsi_str==35 & ndsi_end==30)  %>%
  ggplot() + geom_histogram(aes(cor_off),color = "blue", fill = "pink")


sub <- subset(unbia, bw ==.3 & ndsi_str==35 & ndsi_end == 30)
summary(sub$cor_on)
summary(sub$cor_off)


