###  Calidad del Aire en Valencia
###  04/02/2019 Valencia, Spain - Modificado 12/03/2019
###  Sol Represa
###  Archivo 6


# Objetivo: hacer stack


library(raster)


# # # # # # # # # # # # # # # # # # # # # # # # #

#### 0 - Stack diarios   ####

# # # # # # # # # # # # # # # # # # # # # # # # #


aod <- dir("MODIS/crop_res", pattern = ".tif$")

i=1
while(i <= length(aod)){
  current = i
  lista_dias <- data.frame()
  
  while( substring(aod[i], 10, 19) == substring(aod[current], 10, 19)){
    lista_dias <- c(lista_dias, paste("MODIS/crop_res/", aod[i], sep=""))
    i = i + 1
  }
  
  if( length(lista_dias) > 1){
    archivo <- brick(lista_dias)
    mean_s <- calc(archivo, fun = mean, na.rm = TRUE)
    writeRaster(mean_s, 
                filename = paste("MODIS/diarios/", substring(aod[current], 1, 19), ".tif", sep=""))
  }else{
    archivo <- raster(paste("MODIS/crop_res/", aod[current], sep = ""))
    writeRaster(archivo, 
                filename = paste("MODIS/diarios/", substring(aod[current], 1, 19), ".tif", sep=""))
    
  }
}




# # # # # # # # # # # # # # # # # # # # # # # # #

#### 1 - Stack general   ####

# # # # # # # # # # # # # # # # # # # # # # # # #

dir = "MODIS/crop_res/"

fs <- list.files(path = dir, 
                 pattern = "tif", 
                 full.names = TRUE)


writeRaster(stack(fs), filename = "MOD_TODAS.tif", bylayer = FALSE) #tarda una puta vida!
brick(rfile_multi)


# Para abrir stack:
# s <- brick("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MOD_TODAS.tif", package = "raster")  #abre el raster multicapas
# nlayers(s)

 
min_s <- min(s, na.rm = TRUE)  #atenti, q sino me pone los NA como min :/
max_s <- max(s, na.rm = TRUE)
mean_s <- calc(s, fun = mean, na.rm = TRUE)
sd_s <- calc(s, fun = sd, na.rm = TRUE)

# Guardamos los raster :)

writeRaster(min_s, filename = "min_MODIS.tif")
writeRaster(max_s, filename = "max_MODIS.tif")
writeRaster(mean_s, filename = "mean_MODIS.tif")
writeRaster(sd_s, filename = "sd_MODIS.tif")


# # # # # # # # # # # # # # # # # # # # # # # # #

#### 2 - Stack por year-mes   ####

# # # # # # # # # # # # # # # # # # # # # # # # #


aod <- dir("MODIS/crop_res", pattern = ".tif$")


i=1
while(i <= length(aod)){
  current = i
  l <- data.frame()
  
  while( substring(aod[i], 10, 16) == substring(aod[current], 10, 16)){  #year-month
    l <- c(l, paste("MODIS/crop_res/", aod[i], sep=""))
    i = i + 1
  }
  
  s <- brick(l)
  
  min_s <- min(s, na.rm=TRUE)  
  max_s <- max(s, na.rm=TRUE)
  #median_s <- calc(s, fun = median, na.rm = TRUE)
  mean_s <- calc(s, fun=mean, na.rm=TRUE)
  sd_s <- calc(s, fun=sd, na.rm=TRUE)
  
  writeRaster(min_s, filename = paste("stack/year-month/AOD_", y, "_", month_name[j], "_min.tif", sep=""))
  writeRaster(max_s, filename = paste("stack/year-month/AOD_", y, "_", month_name[j], "_max.tif", sep=""))
  #writeRaster(median_s, filename = paste("AOD_", y, "_",month_name[j], "_median.tif", sep=""))
  writeRaster(mean_s, filename = paste("stack/year-month/AOD_", y, "_", month_name[j], "_mean.tif", sep=""))
  writeRaster(sd_s, filename = paste("stack/year-month/AOD_", y, "_",month_name[j], "_sd.tif", sep=""))
}


  
# # # # # # # # # # # # # # # # # # # # # # # # #

#### 3 - Slack por year   ####

# # # # # # # # # # # # # # # # # # # # # # # # #

dir = "/home/usuario/Sol/aire_comunitat/MODIS/crop_res/"
#"C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\crop_res\\"

fs <- list.files(path = dir, 
                 pattern = "tif", full.names = TRUE)



k <- 1
l <- list()

for (j in 2012:2018){
  for (i in 1:length(fs)){
    year <- as.numeric(substring(fs[i], 59, 62))
    if (year == j){
      l[[k]] <- fs[i]
      k <- k + 1
    }
  }
  s <- raster::brick(l)
  min_s <- min(s, na.rm=TRUE)  
  max_s <- max(s, na.rm=TRUE)
  mean_s <- calc(s, fun=mean, na.rm=TRUE)
  #median_s <- calc(s, fun = median, na.rm = TRUE)
  sd_s <- calc(s, fun=sd, na.rm=TRUE)
  
  
  fun <- function(x) { sum(!is.na(x)) }
  n_s <- calc(s, fun = fun )
  
  writeRaster(min_s, filename = paste("AOD_year_", j, "_min.tif", sep=""))
  writeRaster(max_s, filename = paste("AOD_year_", j, "_max.tif", sep=""))
  writeRaster(mean_s, filename = paste("AOD_year_", j, "_mean.tif", sep=""))
  #writeRaster(median_s, filename = paste("AOD_year_", j, "_median.tif", sep=""))
  writeRaster(sd_s, filename = paste("AOD_year_", j, "_sd.tif", sep=""))
  writeRaster(n_s, filename = paste("AOD_year_", j, "_n.tif", sep=""))
  
  print(j)
}


# # # # # # # # # # # # # # # # # # # # # # # # #

#### 4 - Slack por mes  ####

# # # # # # # # # # # # # # # # # # # # # # # # #

dir = "/home/usuario/Sol/aire_comunitat/MODIS/crop_res/"
#"C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\crop_res\\"

fs <- list.files(path = dir, 
                 pattern = "tif", full.names = TRUE)


month_name <- c("01", "02", "03", "04", "05", "06", "07",
                "08", "09", "10", "11", "12")



for (j in 1:12){
  l <- list()
  k = 1
  for (i in 1:length(fs)){
    month <- substring(fs[i], 64, 65)
    month <- as.numeric(month)
    if (j == month){
      l[[k]] <- fs[i]
      k <- k + 1
      }
    }  
  s <- raster::brick(l)
  min_s <- min(s, na.rm=TRUE)  
  max_s <- max(s, na.rm=TRUE)
  #median_s <- calc(s, fun = median, na.rm = TRUE)
  mean_s <- calc(s, fun = mean, na.rm=TRUE)
  sd_s <- calc(s, fun = sd, na.rm=TRUE)
  
  fun <- function(x) { sum(!is.na(x)) }
  n_s <- calc(s, fun = fun )
  
    
  writeRaster(min_s, filename = paste("AOD_mes_", month_name[j], "_min.tif", sep=""), format = "GTiff")
  writeRaster(max_s, filename = paste("AOD_mes_", month_name[j], "_max.tif", sep=""), format = "GTiff")
  #writeRaster(median_s, filename = paste("AOD_mes_", month_name[j],  "_median.tif", sep=""))
  writeRaster(mean_s, filename = paste("AOD_mes_", month_name[j],  "_mean.tif", sep=""), format = "GTiff")
  writeRaster(sd_s, filename = paste("AOD_mes_", month_name[j],  "_sd.tif", sep=""), format = "GTiff")
  writeRaster(n_s, filename = paste("AOD_mes_", month_name[j], "_n.tif", sep=""), format = "GTiff")
  
  rm(l,i,k)
  print(j)

}






#getValues(a)



# Mejor QGIS
#summary(max_s)
#breakpoints <- c(1e+08,1e+09,1e+10,1e+11,1e+12,1e+13,1e+14)
#display.brewer.all()
#paleta <- brewer.pal(6, "RdPu")
#image(min_s, breaks=breakpoints, col=paleta,
#     main = "OMI NO2TropCloud",
#      xlab=" ", ylab=" ")   #grafico
#plot(shape, add=TRUE)     

