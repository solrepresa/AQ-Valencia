###  Calidad del Aire en Valencia
###  14/05/2019 La Plata, Argentina
###  Sol Represa
###  Archivo 5 - BIS


## Objetivo:
# Tomar imagenes recortadas (NO CALIBRADAS con AERONET)
# en "MODIS/crop/"
# y hacer resampling de las medidas AOD MAIAC 
# para que todos los raster tengan iguales dimensiones y poder hacer stack



library(sp)
library(maps)
library(mapdata)
library(maptools)
library(rgdal)
#library(gpclib)
#library(spatstat)
#library(RGtk2)
#library(MODIStsp)

library(gdalUtils) 
library(raster)
#library(MODIS)

library(lubridate)
library(rgeos)
library("reshape2")
library(dplyr)

#library(lmtest)


# # # # # # # # # # # # # # # # # # # # # # # # #

# >>> Resampling de NO calibradas #### 
# (necesario para hacer stack diario)

# # # # # # # # # # # # # # # # # # # # # # # # #

# La proyeccion del proyecto:
crs_project = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# ATENCION!

fs <- list.files(path = "MODIS/crop/", # NO calibradas con AERONET
                 pattern = ".tif", full.names = TRUE)

extension <- data.frame()
for( i in 1:length(fs)){
  a <- raster(fs[i], values=TRUE)
  ex <- extent(a)
  res <- res(a)
  data <- data.frame(substring(fs[i], 67, 83),ex@xmin, ex@xmax, ex@ymin, ex@ymax, res[1], res[2], ncol(a), nrow(a))
  extension <- rbind(extension, data) 
}

names(extension) <- c("name", "xmin", "xmax", "ymin", "ymax", "res_1", "res_2", "ncol", "nrow")

#extent(a)
#res(a)
#ncol(a)
#nrow(a)

xmin <- min(extension$xmin)
ymin <- min( extension$ymin)
xmax <- max(extension$xmax)
ymax <- max( extension$ymax)
ncol <- max(extension$ncol)
nrow <- max(extension$nrow)

e <- extent(c(xmin = xmin, 
              xmax = xmax, 
              ymin = ymin, 
              ymax = ymax))  # extension donde entren todas las imagenes
s <- raster(e, nrows = nrow, ncols = ncol, crs= crs_project ) 


for (i in 1:length(fs)){
  archivo_name <- paste("MODIS/crop_sin_cal/",
                        substring(fs[i], nchar(fs[i])-29, nchar(fs[i])), sep="")   #seteo nombre de guardado
  crop <- raster(fs[i], values=TRUE)
  if(!all(is.na(crop[]))){
    r1<- raster::resample(crop, s, method = "ngb")
    writeRaster(r1, 
                filename = archivo_name,
                format = "GTiff",
                overwrite = TRUE) # Guardo imagen
    print(i)
  }
}
