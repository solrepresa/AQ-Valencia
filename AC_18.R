### Calidad del Aire en Comunitat Valenciana
### 13/02/2019 Valencia, Spain
### Sol Represa
### Archivo 18

library(rgdal)
library(rgeos)
library(GISTools)
library(lubridate)



## 1)  Abrir files
fs <- list.files(path="C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\FIRE", pattern = ".shp", full.names = TRUE)

# Shape recorte
valencia <- readOGR("C:\\Users\\narep\\Desktop\\SOL\\AQ-Valencia\\mapa\\valencia.shp") #"+proj=longlat +ellps=GRS80 +no_defs "
valencia <- spTransform(valencia, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

## 2)  Armar matriz de RASTER 
# Uso imagen MODIS MCD19A2 como modelo
# ATENTI: VIIRS tiene resolucion 375 m y MCD19A2 de 1km
MCD19A2 <- raster("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\crop_res\\MCD19A2.A2008-01-01.h17v04.tif")

raster_template <- raster(nrows = 239/3, ncols = 158/3,
                          crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ", 
                          ext = extent(MCD19A2))  # toma las extensiones


for(i in 1:length(fs)){
    fire <- readOGR(fs[i])  #CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    fire <- spTransform(fire, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
    variable <- names(fire)
    var <- variable[grep("BRIGHT*", variable)]
       
    for( k in 1:length(var)){
      ## 3) Reproyectar y recortar shapes 
      clip <- raster::intersect(fire, valencia) #clip polygon 2 with polygon 1
       
      ## 4) Seleccionar por fechas ####
      #head(clip@data)
           
      clip$date <- paste(clip$ACQ_DATE, clip$ACQ_TIME, sep = " ")
      clip$date <- as.POSIXct(clip$date, format = "%Y/%m/%d %H%M")
      clip$month <- month(clip$date)
      clip$year <- year(clip$date)
       
      for( i in 2008:2018){
        for(j in 1:12){
          clip.shp <- clip[ which(clip$month == j & clip$year == i), ]
                   
          if (length(clip.shp) > 0) {   # control
            rst <- rasterize(clip.shp, raster_template, var[k]) # shp a raster
            writeRaster(rst, paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\FIRE\\fires_", i,"_",j,"_", var[k], ".tif", sep = ""), format = "GTiff")
          }
       }
     }
   }
}