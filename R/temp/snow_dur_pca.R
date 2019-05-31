
library(raster)



dir_path <-"C:/Users/hgleason/Dropbox/FLNRO/Projects/MODIS_Snow_Dur__Prj/Git_Snow_MODIS/Data/MODIS/Derived/Annual_Snow_Metrics/MD10A1_SD_"

s <- raster(paste(dir_path,2002,'_clp.tif',sep = ""), band=2)



for (year in 2003:2017){
  
  print(year)
  
  path<-paste(dir_path,year,'_clp.tif',sep = "")
  
  rast <-raster(path, band=3)
  
  s <- stack(s,rast)
}

remove(path, year, rast) 

rast_data <-as.data.frame(s)

remove(s)

rast_data[is.na(rast_data)]<- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

x_pose <- t(rast_data)

remove(rast_data)

#filt <- x_pose[ , apply(x_pose, 2, var) != 0]

#remove(x_pose)


dur_pca <- prcomp(x_pose, center=TRUE)

summary(dur_pca)


std_dev <- dur_pca$sdev

pr_var <- std_dev^2

prop_varex <- pr_var/sum(pr_var)

plot(prop_varex, type = "b", xlab = "PC",ylab = "Proportion of Variance Explained")
plot(cumsum(prop_varex), type = "b", xlab = "PC",ylab = "Cumulative Proportion of Variance Explained")

scores <- as.data.frame(dur_pca$x)

loadings <- dur_pca$rotation

cor(scores, teles, method = "spearman")

pc1_load <- loadings[,3]
pc1_load_mat <- matrix(pc1_load, nrow = 2812, byrow = TRUE)
pc1_load_ras<- raster(pc1_load_mat)
extent(pc1_load_ras) <- extent(s)
res(pc1_load_ras) <- res(s)
crs(pc1_load_ras) <- crs(s)

plot(pc1_load_ras)
summary(loadings)
