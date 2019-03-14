###  Calidad del Aire en Valencia
###  04/02/2019 Valencia, Spain - Modificado 12/03/2019
###  Sol Represa
###  Archivo 5


## Objetivo:
## Recortar, Calibrar y realizar un resampling 
## para que todos los raster tengan iguales dimensiones y poder hacer stack



library(sp)
library(maps)
library(mapdata)
library(maptools)
library(rgdal)
#library(gpclib)
#library(spatstat)
library(RGtk2)
library(MODIStsp)

library(gdalUtils) 
library(raster)
library(MODIS)
library(ggplot2)
library(ggmap)

library(lubridate)
library(rgeos)
library("reshape2")
library(dplyr)

library(lmtest)


#library("rhdf5")
library("raster")
library("maptools")
#library(RColorBrewer)
library(reshape2)
#library(ggplot2)



setwd("C:\\Users\\narep\\Desktop\\SOL\\aire_valencia")

# La proeyccion del proyecto:
crs_project = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


# # # # # # # # # # # # # # # # # # # # # # # # #

#### 1 - Recortar TIF     ####

# # # # # # # # # # # # # # # # # # # # # # # # #


id <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\quality", pattern = ".tif") # atencion q no haya .tif.xml en la carpeta..
shape <- readShapePoly("C:\\Users\\narep\\Desktop\\SOL\\AQ-Valencia\\mapa\\valencia.shp")
proj4string(shape) <- CRS("+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs ") # EPSG:3042 
shape <- spTransform(shape, CRS(crs_project))


for (i in 1:length(id)){
  fecha <- paste(as.Date(paste(substring(id[i],14,16), substring(id[i],10,13)), '%j %Y'), 
                 substring(id[i],18,23), sep=".")
  archivo_name <- paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\crop\\", 
                        substring(id[i],1,9),fecha, ".tif", sep="")  #seteo nombre de guardado
  MODISraster <- raster(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\quality\\", id[i], sep = ""))
  if (tryCatch(!is.null(crop(MODISraster,shape)), error=function(e) return(FALSE))){ # cuando no hay superposicion, no corre
    data_recorte <- crop(MODISraster, shape)  #recorto imagen para Valencia
    writeRaster(data_recorte, 
                format = "GTiff",
                filename = archivo_name, 
                overwrite = TRUE) # Guardo imagen
    print(i)
  }
}




# # # # # # # # # # # # # # # # # # # # # # # # #

#### 2 - Calibracion    ####

# # # # # # # # # # # # # # # # # # # # # # # # #




fs <- list.files(path="C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\crop\\", 
                 pattern = "tif", full.names = TRUE)

for (i in 1:length(fs)){
  archivo_name <- paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\crop_cal\\",
                        substring(fs[i],54,83), sep="")   #seteo nombre de guardado
  crop <- raster(fs[i], values=TRUE)
  crop_cal <- (0.84927256*(crop)) + 0.01178339   #calibracion (ver AC_3_bis.R)
  writeRaster(crop_cal, 
              filename = archivo_name, 
              format = "GTiff",
              overwrite = TRUE) # Guardo imagen
  print(i)
}





# # # # # # # # # # # # # # # # # # # # # # # # #

#### 3 - Resampling   ####

# Es necesario hacerlo para igualar la extensión de todas las imagenes y poder hacer el stack

# # # # # # # # # # # # # # # # # # # # # # # # #



fs <- list.files(path = "C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\crop_cal\\", 
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
s <- raster(e, nrows = nrow, ncols = ncol, crs= crs_project ) #raster con 3 veces resolucion


for (i in 1:length(fs)){
  archivo_name <- paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\crop_res\\",
                        substring(fs[i],58,87), sep="")   #seteo nombre de guardado
  crop <- raster(fs[i], values=TRUE)
  if(!all(is.na(crop[]))){
    r1<- resample(crop, s, method = "ngb")
    writeRaster(r1, 
                filename = archivo_name,
                format = "GTiff",
                overwrite = TRUE) # Guardo imagen
    print(i)
  }
}
