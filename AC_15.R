### Calidad del Aire en Comunitat Valenciana
### 11/02/2019 Valencia, Spain
### Sol Represa
### Archivo 15



library(rgdal)
library(raster)
library(R.utils)
library(maptools)


# Objetivo: Trabajar con NDVI



id_aq <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\NDVI\\quality", pattern = ".tif") # atencion q no haya .tif.xml en la carpeta..
id <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\NDVI", pattern = ".tif")

#+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0


## Cambiar sistema de referencia del shape

shape <- readOGR("C:\\Users\\narep\\Desktop\\SOL\\AQ-Valencia\\mapa\\valencia.shp")
shape_trans <- spTransform(shape, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))


for (i in 1:length(id_aq)){
  # 1) Abrir imagen calidad de NDVI
  raster_aq <- raster(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\NDVI\\quality\\", id_aq[i], sep = ""))
  
  #range(raster_aq[raster_aq]) #ver rango de valores
  
  # 1) Crear mascara
  # plot(raster_aq)
  
  # intToBien convierte el numero a binario
  # substring se queda con los characteres de interes
  # integer transforma character a numero
  raster_aq[ raster_aq ] <- as.integer(substring(intToBin(raster_aq[ raster_aq]), 15, 16)) #nos quedamos con los bits 0-1
  raster_aq[ raster_aq != 0] <- NA
  # plot(raster_aq)
  
  # 2) Abrir raster de NDVI
  MODISraster <- raster(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\NDVI\\", id[1], sep = ""))

  MODISraster <- MODISraster*0.0001   #factor de escala 
  # plot(MODISraster)
  
  # 3) Aplicar máscara
  salida <- mask(MODISraster, raster_aq)
  # plot(salida)
  
  # 4)Recortar
  data_recorte <- crop(MODISraster, shape_trans)  #recorto imagen para Valencia
  
  # 4) Guardar
  writeRaster(data_recorte, paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\NDVI\\AQ\\", id[i], sep = ""), format = "GTiff")
  rm(data_recorte, raster_aq, MODISraster)
}
