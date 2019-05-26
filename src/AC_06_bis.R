###  Calidad del Aire en Valencia
### 14/05/2019 La Plata, Argentina
###  Sol Represa
###  Copia de Archivo 6



# Objetivo: hacer stack de AOD diarios SIN calibrar

library(raster)


# # # # # # # # # # # # # # # # # # # # # # # # #

#### 0 - Stack diarios   ####

# # # # # # # # # # # # # # # # # # # # # # # # #


aod <- dir("MODIS/crop_sin_cal", pattern = ".tif$")

i=1
while(i <= length(aod)){
  current = i
  lista_dias <- data.frame()
  
  while( substring(aod[i], 10, 19) == substring(aod[current], 10, 19)){
    lista_dias <- c(lista_dias, paste("MODIS/crop_sin_cal", aod[i], sep=""))
    i = i + 1
  }
  
  if( length(lista_dias) > 1){
    archivo <- brick(lista_dias)
    mean_s <- calc(archivo, fun = mean, na.rm = TRUE)
    writeRaster(mean_s, 
                filename = paste("stack/sin_cal/diarios/", substring(aod[current], 1, 19), ".tif", sep=""))
  }else{
    archivo <- raster(paste("MODIS/crop_sin_cal/", aod[current], sep = ""))
    writeRaster(archivo, 
                filename = paste("MODIS/diarios/", substring(aod[current], 1, 19), ".tif", sep=""))
    
  }
}



