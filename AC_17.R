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

## 2 - Recortar mosaico  



## Shape
shape <- readShapePoly("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\mapa\\valencia_4326.shp",
                       proj4string = CRS(crs_project))


# Recortar
data_recorte <- crop(mosaico, shape)  #recorto imagen para Valencia


# Resampling >> Uso imagen MODIS MCD19A2 como modelo para crear raster
MCD19A2 <- raster("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\crop_res\\MCD19A2.A2008-01-01.h17v04.tif")

raster_template <- raster(nrows = 239*10, ncols = 158*10, 
                          crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ", 
                          ext = extent(MCD19A2))  # toma las extensiones

data_resampling <- raster::resample(data_recorte, raster_template)


# Guardar
writeRaster(data_resampling , "C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\DEM\\DEM.tif", format = "GTiff")
