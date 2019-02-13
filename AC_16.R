### Calidad del Aire en Comunitat Valenciana
### 11/02/2019 Valencia, Spain
### Sol Represa
### Archivo 16



### Objetivo: Trabajar con MERRA

library(gdalUtils)
library(raster)
library(rgdal)
library(R.utils)
library(maptools)


# CORROBORAR:
# gdal_setInstallation(verbose=TRUE) # ver version gdal
# getOption("gdalUtils_gdalPath")  #verificar que podamos abrir HDF5



setwd("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MERRA") #fundamental!
id <- dir(pattern = ".hdf") 

sds <- get_subdatasets(id[1]) # Lista de los nombres de las SDS
name_sds <- sds[1]


## Cambiar sistema de referencia del shape

shape <- readOGR("C:\\Users\\narep\\Desktop\\SOL\\AQ-Valencia\\mapa\\valencia.shp")
shape_trans <- spTransform(shape, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))



i=1
for( i in 1: length(id)){
  #1) Abrir
  filename <- paste(substr(id[i], 0, 35), ".tif", sep="")
  gdal_translate(name_sds, dst_dataset = filename)
  MIRRAraster <- raster(filename)
  
  # 2) Reproyectar
  MIRRAraster <- projectRaster(MIRRAraster, crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
                        method = "bilinear")
  
  # 3)Recortar
  data_recorte <- crop(MIRRAraster, shape_trans)  #recorto imagen para Valencia
  
  # 4) Guardar
  writeRaster(data_recorte, paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MERRA\\raster\\", substr(id[i], 0, 35), "_ANA.tif", sep = ""), format = "GTiff")
  rm(data_recorte, MIRRAraster)
  
}

