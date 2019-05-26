### Calidad del Aire en Comunitat Valenciana
### 13/02/2019 Valencia, Spain
### Sol Represa
### Archivo 18

library(rgdal)
library(rgeos)
library(GISTools)
library(lubridate)
library(maptools)

## Objetivo: Generar base de FIRE


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 1)  Abrir files
fs <- list.files(path="/home/usuario/Sol/aire_comunitat/variables/FIRE", pattern = ".shp", full.names = TRUE)

file <- fs[1]  #solo con mediciones MODIS (facilita la integracion de datos)

# 2) Shape recorte

crs_project = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
valencia <- readShapePoly("/home/usuario/Sol/aire_comunitat/mapa/valencia_4326.shp",
                       proj4string = CRS(crs_project))

## 3)  Armar matriz de RASTER 
# Uso imagen MODIS MCD19A2 como modelo
MCD19A2 <- raster("/home/usuario/Sol/aire_comunitat/stack/month/AOD_mes_01_max.tif")

raster_template <- raster(nrows = 239, ncols = 158, #mantengo resolucion de 1km
                          crs = crs_project, 
                          ext = extent(MCD19A2))  # toma las extensiones




fire <- readOGR(file)  #CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#fire <- spTransform(fire, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
variable <- names(fire)
var <- variable[grep("BRIGHT*", variable)]

var <- "BRIGHTNESS"
 
clip <- raster::intersect(fire, valencia) # Reproyectar y recortar shapes
 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## Agrupar por fechas >>> Mensual ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

clip$date <- paste(clip$ACQ_DATE, clip$ACQ_TIME, sep = " ")
clip$date <- as.POSIXct(clip$date, format = "%Y/%m/%d %H%M")
clip$month <- month(clip$date)
clip$year <- year(clip$date)
 
for( i in 2008:2018){
  for(j in 1:12){
    clip.shp <- clip[ which(clip$month == j & clip$year == i), ]
             
    if (length(clip.shp) > 0) {   # control
      rst <- rasterize(clip.shp, raster_template, var) # shp a raster
      #plot(rst, col= "black")
      recalc <- c(0, 0, 0, 0.001, Inf , 1)  # reclasificar >> son col: c[,1] = from; c[,2] = to; c[,3] = becomes 
      rst <- reclassify(rst, recalc)
      fecha <- paste(i, j, "01", sep = " ")
      fecha <- as.Date(fecha, format = "%Y %m %d")
      fecha <- format(fecha, format = "%Y-%j")
      writeRaster(rst, 
                  paste("/home/usuario/Sol/aire_comunitat/variables/FIRE/mes/fire-mes-",
                        fecha,".tif", sep = ""), 
                  format = "GTiff")
    }
 }
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## Seleccionar por fechas >>> Diario

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

clip$date <- paste(clip$ACQ_DATE, clip$ACQ_TIME, sep = " ")
clip$date <- as.POSIXct(clip$date, format = "%Y/%m/%d %H%M")
clip$month <- month(clip$date)
clip$year <- year(clip$date)
clip$day <- day(clip$date)

for( i in 2008:2018){
  for(j in 1:12){
    for(k in 1:31){
      clip.shp <- clip[ which(clip$month == j & clip$year == i & clip$day == k), ]
      
      if (length(clip.shp) > 0) {   # control
        rst <- rasterize(clip.shp, raster_template, var) # shp a raster
        #plot(rst, col= "black")
        recalc <- c(0, 0, 0, 0.001, Inf , 1)  # reclasificar >> son col: c[,1] = from; c[,2] = to; c[,3] = becomes 
        rst <- reclassify(rst, recalc)
        fecha <- paste(i, j, k, sep = " ")
        fecha <- as.Date(fecha, format = "%Y %m %d")
        fecha <- format(fecha, format = "%Y-%j")
        writeRaster(rst, paste("/home/usuario/Sol/aire_comunitat/variables/FIRE/diario/fire-", fecha, ".tif", sep = ""), format = "GTiff")

      }
    }
  }
}

