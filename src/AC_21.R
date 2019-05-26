### Calidad del Aire en Comunitat Valenciana
### 15/02/2019 Valencia, Spain
### Sol Represa
### Archivo 21


library(rgdal)
library(rgeos)
library(GISTools)
library(lubridate)
library(raster)


## Objetivo: Generar raster de distancia a rutas y trenes

# ATENCION! Faltan datos que corren en pc de marta

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 1) Uso imagen MODIS MCD19A2 como modelo para crear raster

MCD19A2 <- raster("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\crop_res\\MCD19A2.A2008-01-01.h17v04.tif")

raster_template <- raster(nrows = 239*10, ncols = 158*10,
                          crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ", 
                          ext = extent(MCD19A2))  # toma las extensiones


# 2) Abro archivos de ruta

fs <- list.files(path = "C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\RUTAS", pattern = ".shp", full.names = TRUE)
nombre <- substring(fs, 59, nchar(fs)-4)

for(i in 1:length(fs)){
  ruta <- readOGR(fs[i])
  ruta <- as(ruta, "SpatialLines")
  
  distancia <- gDistance(ruta, as(raster_template, "SpatialPoints"), byid = TRUE)
  #plot(distancia)
  raster_template[] <- apply(distancia, 1, min)
  writeRaster(raster_template, 
              paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\RUTAS\\salida\\distancia_", nombre[i], ".tif" , sep = ""),
              format = "GTiff")
}


