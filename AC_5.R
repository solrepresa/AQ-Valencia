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
#library(RGtk2)
#library(MODIStsp)

library(gdalUtils) 
library(raster)
library(MODIS)

library(lubridate)
library(rgeos)
library("reshape2")
library(dplyr)

library(lmtest)


dir = "/home/usuario/Sol/aire_comunitat/MODIS"


# La proyeccion del proyecto:
crs_project = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


# # # # # # # # # # # # # # # # # # # # # # # # #

#### 1 - Recortar TIF     ####

# # # # # # # # # # # # # # # # # # # # # # # # #


id <- dir("/home/usuario/Sol/aire_comunitat/MODIS/quality", pattern = ".tif") # atencion q no haya .tif.xml en la carpeta..
shape <- readShapePoly("/home/usuario/Sol/aire_comunitat/mapa/valencia_4326.shp",
                       proj4string = CRS(crs_project))


for (i in 1:length(id)){
  fecha <- paste(as.Date(paste(substring(id[i],14,16), substring(id[i],10,13)), '%j %Y'), 
                 substring(id[i],18,23), sep = ".")
  archivo_name <- paste("/home/usuario/Sol/aire_comunitat/MODIS/crop/", 
                        substring(id[i], 1, 9), fecha, ".tif", sep = "")  #seteo nombre de guardado
  MODISraster <- raster(paste("/home/usuario/Sol/aire_comunitat/MODIS/quality/", id[i], sep = ""))
  if (tryCatch(!is.null(crop(MODISraster,shape)), error = function(e) return(FALSE))){ # cuando no hay superposicion, no corre
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




fs <- list.files(path = "/home/usuario/Sol/aire_comunitat/MODIS/crop/", 
                 pattern = "tif", full.names = TRUE)

for (i in 1:length(fs)){
  archivo_name <- paste("/home/usuario/Sol/aire_comunitat/MODIS/crop_cal/",
                        substring(fs[i], 46, 83), sep="")   #seteo nombre de guardado
  crop <- raster(fs[i], values = TRUE)
  crop_cal <- (0.84927256*(crop)) + 0.01178339   #calibracion (ver AC_3_bis.R)
  writeRaster(crop_cal, 
              filename = archivo_name, 
              format = "GTiff",
              overwrite = TRUE) # Guardo imagen
  print(i)
}





# # # # # # # # # # # # # # # # # # # # # # # # #

#### 3 - Resampling   #### >>>>>>>>>> desde aca!!

# Es necesario hacerlo para igualar la extensión de todas las imagenes y poder hacer el stack

# # # # # # # # # # # # # # # # # # # # # # # # #



fs <- list.files(path = "/home/usuario/Sol/aire_comunitat/MODIS/crop_cal/", 
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
  archivo_name <- paste("/home/usuario/Sol/aire_comunitat/MODIS/crop_res/",
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
