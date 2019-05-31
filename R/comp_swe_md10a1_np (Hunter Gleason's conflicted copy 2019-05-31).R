library(tidyverse)

rm(list=ls(all=T))

setwd("C:/Users/hgleason/Dropbox/FLNRO/Projects/MODIS_Snow_Dur_Prj/Git_Snow_MODIS/Data/ASWS/OUT/")

data <- read.csv("C:/Users/hgleason/Dropbox/FLNRO/Projects/MODIS_Snow_Dur_Prj/Git_Snow_MODIS/Data/ASWS/OUT/Snow_Dur_Comp_it1_dlta3_np.csv")

stations<-unique(data$station)

set.seed(0)

rand<-round(runif(length(stations),0,length(unique(data$station))))

print(length(stations))
print(length(rand))

thrsh<- round(0.75*length(rand))

cal_stats<-stations[rand<thrsh]
val_stats<-stations[rand>thrsh]

cal<- data[data$station %in% cal_stats,]

val<- data[data$station %in% val_stats,]


data <- subset(data,mddur>=0 & sddur>=0 & sdon != swestr & sdoff != sweend)

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



unbia <- data %>% 
  group_by(station) %>%
  mutate(ondif = sdon - mdon) %>%
  mutate(onbias = median(ondif)) %>%
  mutate(offdif = sdoff - mdoff) %>%
  mutate(offbias = median(offdif)) %>%
  mutate(durdif = sddur - mddur) %>%
  mutate(durbias = median(durdif)) %>%
  mutate(cor_on = ondif-onbias)%>%
  mutate(cor_off= offdif-offbias)%>%
  mutate(cor_dur= durdif-durbias)



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
  dplyr::filter(station %in% cal_stats) %>%
  ungroup()%>%
  group_by(bw,thresh)%>%
  summarise(med_ab_dif = median(comb_diff), sd_comb = sd(cor_on)+sd(cor_off) )

unbia %>% 
  ungroup() %>%
  dplyr::filter(mddur >=0 & bw == 0.20 & thresh==30)  %>%
  ggplot() + geom_histogram(aes(cor_on),color = "blue", fill = "pink")
  

unbia %>% 
  ungroup() %>%
  dplyr::filter(mddur >=0 & bw == 0.20 & thresh==30)  %>%
  ggplot() + geom_histogram(aes(cor_off),color = "blue", fill = "pink")


sub <- subset(unbia, bw ==.20 & thresh == 30)


diff<-sub %>%
  dplyr::select(station, year, cor_on, cor_off, cor_dur)


hist(sub$cor_on, breaks=20)
hist(sub$cor_off, breaks=20)
hist(sub$cor_dur, breaks=50)

summary(sub$cor_on)
summary(sub$cor_off)
summary(sub$cor_dur)


write.csv(diff, 'ASWS_Error_bw2_thr30.csv')



