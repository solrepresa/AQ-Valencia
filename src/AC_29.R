### Calidad del Aire en Comunitat Valenciana
### 04/05/2019 La Plata, Argentina
### Sol Represa
### Archivo 29


# Objetivo: Armar slack de los mapas de PM25


library(raster)


# # # # # # # # # # # # # # # # # # # # # # # # #

#### 1 - Slack por year   ####

# # # # # # # # # # # # # # # # # # # # # # # # #

dir = "modelo_rf_100/salida/"
dir_slack = "modelo_rf_100/year/"

fs <- list.files(path = dir, 
                 pattern = "tif", full.names = TRUE)


k <- 1
l <- list()

for (j in 2008:2018){
  for (i in 1:length(fs)){
    year <- as.numeric(substring(fs[i], 31, 34))
    if (year == j){
      l[[k]] <- fs[i]
      k <- k + 1
    }
  }
  s <- raster::brick(l)
  
  min_s <- min(s, na.rm=TRUE)  
  max_s <- max(s, na.rm=TRUE)
  mean_s <- calc(s, fun = mean, na.rm=TRUE)
  #median_s <- calc(s, fun = median, na.rm = TRUE)
  sd_s <- calc(s, fun=sd, na.rm=TRUE)
  cv_s <- sd_s/mean_s
  
  #fun <- function(x) { sum(!is.na(x)) }
  #n_s <- calc(s, fun = fun )
  
  writeRaster(min_s, filename = paste(dir_slack, "PM25_year_", j, "_min.tif", sep=""))
  writeRaster(max_s, filename = paste(dir_slack, "PM25_year_", j, "_max.tif", sep=""))
  writeRaster(mean_s, 
              filename = paste(dir_slack, "PM25_year_", j, "_mean.tif", sep = ""),  
              overwrite = TRUE,
              format= "GTiff")
  #writeRaster(median_s, filename = paste(dir_slack, "PM25_year_", j, "_median.tif", sep=""))
  writeRaster(sd_s, filename = paste(dir_slack, "PM25_year_", j, "_sd.tif", sep=""))
  #writeRaster(n_s, filename = paste(dir_slack, "PM25_year_", j, "_n.tif", sep=""))
  writeRaster(cv_s, filename = paste(dir_slack, "PM25_year_", j, "_cv.tif", sep=""))
  
  print(j)
  rm(s, min_s, max_s, sd_s, cv_s)
}



