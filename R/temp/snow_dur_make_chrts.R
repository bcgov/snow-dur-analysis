
library(raster)

setwd("C:/Users/hgleason/Dropbox/Git_Snow_MODIS/Snow_Dur_Analysis/R/")

dir_path <-"C:/Users/hgleason/Dropbox/Git_Snow_MODIS/Data/MODIS/Derived/Annual_Snow_Metrics/MD10A1_SD_"

sdon <- raster(paste(dir_path,2002,'_clp.tif',sep = ""), band=1)
sdoff <- raster(paste(dir_path,2002,'_clp.tif',sep = ""), band=2)
sddur <- raster(paste(dir_path,2002,'_clp.tif',sep = ""), band=3)
sdobs <- raster(paste(dir_path,2002,'_clp.tif',sep = ""), band=4)



for (year in 2003:2017){
  
  print(year)
  
  path<-paste(dir_path,year,'_clp.tif',sep = "")
  
  sdon_t <-raster(path, band=1)
  sdon <- stack(sdon_t,sdon)
  
  sdoff_t <-raster(path, band=2)
  sdoff <- stack(sdoff_t,sdoff)
  
  sddur_t <-raster(path, band=3)
  sddur <- stack(sddur_t,sddur)
  
  sdobs_t <-raster(path, band=4)
  sdobs <- stack(sdobs_t,sdobs)
  
  remove(sdon_t,sdoff_t,sddur_t,sdobs_t)
}


sdon_av <- mean(sdon)
sdoff_av <- mean(sdoff)
sddur_av <- mean(sddur)
sdobs_av <- mean(sdobs)


stack<- stack(sdon_av,sdoff_av,sddur_av, sdobs_av)

names(stack) = c("Onset","Meltout","Duration","Observations")

# get multiple layers, e.g. the slope _and_ intercept
fun <- function(x) { x[is.na(x)] <- -9999; return(x)} 
filled <- calc(sddur, fun)
time <- 1:nlayers(filled)
X <- cbind(1, time)
invXtX <- solve(t(X) %*% X) %*% t(X)
lin_regr <- function(y) (invXtX %*% y)[2]
slope <- calc(filled, lin_regr) 

plot(stack, col=colorRampPalette(c("black", "brown", "white"))(255))



