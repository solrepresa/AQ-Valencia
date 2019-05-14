####### Calidad del Aire en Comunitat Valenciana
####### 15/10/2018 Valencia, Spain
####### Sol Represa
####### Archivo 0

# Desarrollo de algoritmo para limpiar raster con valores de calidad





# LEER
# all HDFCEOS products are written in the big-endian referencing scheme, 
# which means the "first" bit is the farthest to the right and the "last" bit is the farthest to the left.
# https://www.nceas.ucsb.edu/~pau/StephanieSite/Home_files/MODIS_LP_QA_Tutorial-1.pdf

# QA for AOD en numeros BINARIOS
# 0000 --- Best quality = 0
# 0001 --- Water Sediments are detected (water)
# 0011 --- There is 1 neighbor cloud
# 0100 --- There is >1 neighbor clouds
# 0101 --- No retrieval (cloudy, or whatever)
# 0110 --- No retrievals near detected or previously detected snow
# 0111 --- Climatology AOD: altitude above 3.5km (water) and 4.2km (land)
# 1000 --- No retrieval due to sun glint (water)
# 1001 --- Retrieved AOD is very low (<0.05) due to glint (water)
# 1010 --- AOD within +-2km from the coastline (may be unreliable)
# 1011 --- Land, research quality: AOD retrieved but CM is possibly cloudy

# Sacado de https://lpdaac.usgs.gov/sites/default/files/public/product_documentation/mcd19_user_guide_v6.pdf



library(rgdal)
library(raster)

library(R.utils)

######################################################

# 1) Aplicar mascara de calidad (QA= 0000) a imagenes MODIS

######################################################

id_aq <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\raster\\AQ", pattern = "AQ")
id <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\raster", pattern = ".tif")


for (i in 1:length(id_aq)){
  raster_aq <- raster(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\raster\\AQ\\", id_aq[i], sep = ""))
  #range(raster_aq[raster_aq]) #ver rango de valores
  
  # 1) Crear mascara
  # plot(raster_aq)
  
  # intToBien convierte el numero a binario
  # substring se queda con los characteres de interes
  # integer transforma character a numero
  raster_aq[ raster_aq ] <- as.integer(substring(intToBin(raster_aq[ raster_aq]), 4, 7)) #nos quedamos con los bits 8-11
  raster_aq[ raster_aq != 0] <- NA
  # plot(raster_aq)
  
  # 2) Abrir raster de AOD
  MODISraster <- raster(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\raster\\", id[i], sep = ""))
  MODISraster <- MODISraster*0.001   #factor de escala por imagenes MODIS
  # plot(MODISraster)
  
  # 3) Aplicar máscara
  salida <- mask(MODISraster, raster_aq)
  # plot(salida)
  
  # 4) Guardar
  writeRaster(salida, paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\quality\\", id[i], sep = ""), format = "GTiff")
  rm(salida, raster_aq, MODISraster)
}

