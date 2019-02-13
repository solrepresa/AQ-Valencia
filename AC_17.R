### Calidad del Aire en Comunitat Valenciana
### 13/02/2019 Valencia, Spain
### Sol Represa
### Archivo 17


## Abrir raster de DEM

# DATOS descargados de:
#Jarvis A., H.I. Reuter, A.  Nelson, E. Guevara, 2008, Hole-filled  seamless SRTM
#data V4, International  Centre for Tropical  Agriculture (CIAT), available  from
#http://srtm.csi.cgiar.org.


library(raster)



# # # # # # # # # # # # # # # # # # # # # # # # # # 

#### 1 - Hacer un mosaico  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # 

fs <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\DEM", pattern = ".tif") # atencion q no haya .tif.xml en la carpeta..

dem_1 <- raster(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\DEM\\", fs[1], sep = ""))
dem_2 <- raster(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\DEM\\", fs[2], sep = ""))
dem_3 <- raster(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\DEM\\", fs[3], sep = ""))
dem_4 <- raster(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\DEM\\", fs[4], sep = ""))


#mosaic(dem_1, dem_2)   # funcion para armar mosaicos


dl <- list(dem_1, dem_2, dem_3, dem_4)  #Armar lista   
dl$fun <- mean                   #Definir funcion resumen
mosaico <- do.call(mosaic, dl)   #Aplicar a toda la lista

plot(mosaico)


# # # # # # # # # # # # # # # # # # # # # # # # # # 

#### 1 - Recortar mosaico  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # 


## Shape
shape <- readOGR("C:\\Users\\narep\\Desktop\\SOL\\AQ-Valencia\\mapa\\valencia.shp")
shape_trans <- spTransform(shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


# Recortar
mosaico_rec <- crop(mosaico, shape_trans)  #recorto imagen para Valencia

# Guardar
writeRaster(mosaico_rec , "C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\DEM\\DEM.tif", format = "GTiff")
