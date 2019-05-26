### Calidad del Aire en Comunitat Valenciana
### 05/02/2019 Valencia, Spain
### Modificado 29/01/2019
### Sol Represa
### Archivo 4


# Objetivo: Corroborar que puntos, shp caen en imagen raster



library(foreign)
library(ggplot2)
library(ggmap)
library(rgdal) #abrir shp



# # # # # # # # # # # # # # # # # # # # # # # # # # # 

##  1 - Sitio de estaciones de monitoreo  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # 

sitios <- read.dbf("sitios.dbf")
sitios <- data.frame(sitios$Latitud, sitios$Longitud, sitios$Cod)
names(sitios) <- c("Latitud", "Longitud", "Codigo")

sitios <- sitios[order(sitios$Codigo),]



## Esta es otra forma pero partiendo del .shp
#points <- readShapePoints("est_monitoreo.shp") ## Carga de puntos para extraer la info
#proj4string(points) <- CRS("+proj=longlat +ellps=WGS84 +no_defs") # Asignar proyeccion
#slot(points, "data") <- data.frame(Id = points@data$codigo, Nombre = points@data$Estacion) # Asigno variables de inter?s (o quito el resto)

register_google(key = "AIzaSyDL10fYmvbe1w0E8y2tfGF6fcDO5x7osBQ")


## Mapa de lo puntos de calidad del aire 

map <- get_map(location = c(lat = 39.508, lon = -0.418), 
               maptype ="roadmap", 
               api_key = "AIzaSyDL10fYmvbe1w0E8y2tfGF6fcDO5x7osBQ")

ggmap(map) + 
  geom_point(aes(x=Longitud, y=Latitud, colour=Codigo), 
             data = sitios,
             shape = 18, alpha = .9, size = 3) +
  ggtitle("Sitios de monitoreo de calidad del aire") +
  labs(x= "Longitud", y= "Latitud") 





# # # # # # # # # # # # # # # # # # # # # # # # # # # 

## 2 - Verificar que puntos y mapas coicidan ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # 


id <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\quality", 
          pattern = ".tif")
MODIS <- raster(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\quality\\", 
                      id[1], sep = ""))


## Opcion A ###
## Hacer un data.frame y graficarlo con ggplot
aod.spdf <- as(MODIS, "SpatialPixelsDataFrame")
aod.df <- as.data.frame(aod.spdf)
names(aod.df)[1]<- "AOD"
head(aod.df)

#Agregar shp de comunidad valenciana
esp_shp <- readOGR("C:\\Users\\narep\\Desktop\\SOL\\AQ-Valencia\\mapa\\valencia.shp")

g <- ggplot() + 
  geom_polygon(data = esp_shp, 
               aes(x=long, y = lat, group = group), 
               fill = NA, color = "grey")

gg <- g + 
  geom_tile(data = aod.df, 
            aes(x= x, y= y, fill = AOD)) + 
  coord_equal()

ggg <- gg + 
  geom_point(data=sitios, 
             aes(x = Longitud, y = Latitud,
                 colour = Codigo),
             shape=18, 
             alpha=.8, 
             size = 5) + 
  coord_fixed(1.3) + 
  theme_minimal()



