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

dem_1 <- raster("variables/DEM/srtm_36_04.tif")
dem_2 <- raster("variables/DEM/srtm_36_05.tif")
dem_3 <- raster("variables/DEM/srtm_37_04.tif")
dem_4 <- raster("variables/DEM/srtm_37_05.tif")


#mosaic(dem_1, dem_2)   # funcion para armar mosaicos


dl <- list(dem_1, dem_2, dem_3, dem_4)  #Armar lista   
dl$fun <- mean                   #Definir funcion resumen
mosaico <- do.call(mosaic, dl)   #Aplicar a toda la lista

plot(mosaico)


# # # # # # # # # # # # # # # # # # # # # # # # # # 

## 2 - Recortar mosaico  

crs_project <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "

## Shape
shape <- readShapePoly("mapa/valencia_4326.shp",
                       proj4string = CRS(crs_project))


# Recortar
data_recorte <- crop(mosaico, shape)  #recorto imagen para Valencia


# Resampling >> Uso imagen MODIS MCD19A2 como modelo para crear raster
MCD19A2  <- raster("stack/month/AOD_mes_01_max.tif")

raster_template <- raster(nrows = 239, ncols = 158, 
                          crs = crs_project , 
                          ext = extent(MCD19A2))  # toma las extensiones

data_resampling <- raster::resample(data_recorte, raster_template)


# Guardar
writeRaster(data_resampling , 
            "variables/DEM/DEM.tif", 
            format = "GTiff",
            overwrite = TRUE)
