### Calidad del Aire en Comunitat Valenciana
### 29/03/2019 La Plata, Argentina
### Modificado de AC_4.R
### Sol Represa
### Archivo 4 bis


# Objetivo: Corroborar que AOD, shp y NDVI coinciden



library(foreign)
library(ggplot2)
library(ggmap)   #citation("ggmap")
library(rgdal) #abrir shp


# # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Cargar mapa de google maps

# # # # # # # # # # # # # # # # # # # # # # # # # # # 

register_google(key = "AIzaSyDL10fYmvbe1w0E8y2tfGF6fcDO5x7osBQ")


## Mapa de Valencia 

map <- get_map(location = c(lat = 39.508, lon = -0.418), 
               maptype ="roadmap", 
               api_key = "AIzaSyDL10fYmvbe1w0E8y2tfGF6fcDO5x7osBQ")


# # # # # # # # # # # # # # # # # # # # # # # # # # # 

## 2 - Verificar que puntos y mapas coicidan ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # 


MODIS <- raster("/home/usuario/Sol/aire_comunitat/stack/month/AOD_mes_01_max.tif")


## Opcion A ###
## Hacer un data.frame y graficarlo con ggplot
aod.spdf <- as(MODIS, "SpatialPixelsDataFrame")
aod.df <- as.data.frame(aod.spdf)
names(aod.df)[1]<- "AOD"
head(aod.df)

#Agregar shp de comunidad valenciana
esp_shp <- readOGR("/home/usuario/Sol/aire_comunitat/mapa/valencia_4326.shp")

g <- ggplot() + 
  geom_polygon(data = esp_shp, 
               aes(x=long, y = lat, group = group), 
               fill = NA, color = "grey")

gg <- g + 
  geom_tile(data = aod.df, 
            aes(x= x, y= y, fill = AOD), alpha=.8) + 
  coord_equal()


## Agregar NDVI ####

NDVI <- raster("/home/usuario/Sol/aire_comunitat/variables/NDVI/crop_res/MOD13A3.A2008001.mosaic.006.2019039161045.psmc_000501302431.1_km_monthly_NDVI-1_km_monthly_NDVI.tif")

ndvi.spdf <- as(NDVI, "SpatialPixelsDataFrame")
ndvi.df <- as.data.frame(ndvi.spdf)
names(ndvi.df)[1]<- "ndvi"
head(ndvi.df)


# ¿Como se ve NDVI sobre el mapa?
ng <- g + 
  geom_tile(data = ndvi.df, 
            aes(x= x, y= y, fill = ndvi), alpha=.8) + 
  coord_equal()


# ¿como se ven NDVI + AOD ?
ggg <- gg + 
  geom_tile(data = ndvi.df, 
                      aes(x = x, y = y, fill = ndvi), alpha=.8) 


## Agregar MERRA ####

MERRA <- raster("/media/usuario/Elements SE/MERRA/raster_res/MERRA2_300.inst3_3d_asm_Np.20080101.PS.tif")

merra.spdf <- as(MERRA, "SpatialPixelsDataFrame")
merra.df <- as.data.frame(merra.spdf)
names(merra.df)[1]<- "merra"
head(merra.df)

# ¿Como se ve MERRA sobre el mapa?
mg <- g + 
  geom_tile(data = merra.df, 
            aes(x= x, y= y, fill = merra), alpha=.8) + 
  coord_equal()

