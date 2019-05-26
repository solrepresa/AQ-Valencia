### Calidad del Aire en Comunitat Valenciana
### 15/02/2019 Valencia, Spain
### Sol Represa
### Archivo 21

## Objetivo trabajar datos de CORINE
# https://land.copernicus.eu/user-corner/technical-library/corine-land-cover-nomenclature-guidelines/html/

library(rgdal)
library(rgeos)
library(GISTools)
library(lubridate)
library(raster)
library(dplyr)


# # # # # # # # # # # # # # # # # # 

# 1) Abrir clases de CORINE

# # # # # # # # # # # # # # # # # # 

crs_project <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "

CLC <- readOGR("/home/usuario/Sol/AQ-Valencia/mapa/CLC2012_CV.shp")
CLC <- spTransform(CLC, crs_project)

# Abrir shape valencia
shape <- readShapePoly("mapa/valencia_4326.shp",
                       proj4string = CRS(crs_project))

#CLC <- crop(CLC, shape)  # Recorto imagen para Valencia



# # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 2) Trabajo con df para generar variables utiles

# # # # # # # # # # # # # # # # # # # # # # # # # # # 

CLC_df <- CLC@data
CLC_df$CODE_12 <- as.numeric(levels(CLC_df$CODE_12))[CLC_df$CODE_12]

CLC_df[which(CLC_df$CODE_12 < 200), 4] <- 1 #"Artificial"
CLC_df[which(CLC_df$CODE_12 > 200 & CLC_df$CODE_12 <300), 4] <- 2 #"Agricola"
CLC_df[which(CLC_df$CODE_12 > 300 & CLC_df$CODE_12 <400), 4] <- 3 #"Forestal"
CLC_df[which(CLC_df$CODE_12 > 400 & CLC_df$CODE_12 <500), 4] <- 4 #"Humedales"
CLC_df[which(CLC_df$CODE_12 > 500), 4] <- 5 #"Agua"

CLC_df <- data.frame(CLC$COBERTURA, CLC_df$V4)

names(CLC_df) <- c("COBERTURA", "Clase")
CLC@data <- CLC_df # Guardar en shape


# # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 3) Generar raster por categorías iterativo

# # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Uso imagen MODIS MCD19A2 como modelo para crear raster
MCD19A2 <- raster("stack/month/AOD_mes_01_max.tif")

clc_template <- raster(nrows = 239*3, ncols = 158*3, 
                          crs = crs_project , 
                          ext = extent(MCD19A2))  # toma las extensiones

# Pasar de shp a RASTER 
rst <- rasterize(CLC, clc_template, field = CLC$Clase)


for( i in 1:5){
  #1) Haccer copia para no pisarla
  CLC_copia <- rst

  #2) Poner en cero todas las q no son nuetra clase de interes
  CLC_copia[CLC_copia != i] <- NA  

  # 3) Sumar valores = 1, dividir por total de pixels que fueron agregados (3*3)
  CLC_suma <- aggregate(x = CLC_copia, 
                         fact = 3, 
                         fun = function(x, ...){ (sum(x == 1, ...)/9)*100})
  
  # 4) Guardar
  writeRaster(CLC_suma, 
              file = paste("variables/CLC/CLC_", i, ".tif", sep=""), 
              format= "GTiff", 
              overwrite = TRUE )
  
}
