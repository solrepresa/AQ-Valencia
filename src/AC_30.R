### Calidad del Aire en Comunitat Valenciana
### 09/05/2019 La Plata, Argentina
### Sol Represa
### Archivo 30


# Objetivo: visualizacion de mapas

library(raster)
library(rgdal)
library(ggplot2)
library(colorRamps)
library(RColorBrewer)


#par(mar=c(3,4,2,2))
#display.brewer.all()
# # # # # # # # # # # # # # # # # # # # # # # # #

#### 2 - Visualizacion slack por year   ####

# # # # # # # # # # # # # # # # # # # # # # # # #
#PM anuales

datos_raster <- data.frame()
for(i in 2008:2018){
  PM <- raster(paste("modelo_rf_10/year/PM25_year_", i, "_mean.tif", sep = "") )
  data <- data.frame( year = i,
                      min = cellStats(PM, min),
                      max = cellStats(PM, max),
                      mean = cellStats(PM, mean),
                      sd = cellStats(PM, sd))
  datos_raster <- rbind(datos_raster, data)
}
datos_raster$rank <- datos_raster$max - datos_raster$min

ggplot(data = datos_raster) + 
  geom_crossbar(aes(y= mean, ymin= min, ymax = max, x=year), 
                fill= "darkorange" , 
                alpha = 0.8,
                col = "gray13") +
  theme_bw() + 
  xlab("") +
  ylab("") +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8, hjust=.5),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)) +
  geom_hline( yintercept =25, col = "red" ) +
  scale_x_continuous(breaks = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))
  


# PM antes PM despues
PM_A <- raster("modelo_rf_10/year/PM25_year_2008_mean.tif")
PM_D <- raster("modelo_rf_10/year/PM25_year_2017_mean.tif")
dif <- PM_D - PM_A
writeRaster(dif, filename = "modelo_rf_10/PM25_year_2008-2017.tif", overwrite = TRUE, format= "GTiff")


# armamos df para graficar
spdf_a <- as(PM_A, "SpatialPixelsDataFrame")
df_a <- as.data.frame(spdf_a)
names(df_a)[1]<- "PM25"
head(df_a)

spdf_d <- as(PM_D, "SpatialPixelsDataFrame")
df_d <- as.data.frame(spdf_d)
names(df_d)[1]<- "PM25"
head(df_d)

spdf_dif <- as(dif, "SpatialPixelsDataFrame")
df_dif <- as.data.frame(spdf_dif)
names(df_dif)[1]<- "PM25"
head(df_dif)


# Visualizar capa CLC << esta limitando modelo
clc <- raster("variables/CLC/CLC_1.tif")
clc <- as(clc, "SpatialPixelsDataFrame")
clc <- as.data.frame(clc)


#Agregar shp de comunidad valenciana
esp_shp <- readOGR("mapa/valencia_4326.shp")


# Mapa antes
ggplot() + 
  geom_polygon(data = esp_shp, 
               aes(x=long, y = lat, group = group), 
               fill = NA, color = "grey") + 
  geom_tile(data = df_a, 
            aes(x = x, y = y, fill = PM25), alpha=.8) +
  scale_fill_distiller(palette = "OrRd",    #se usa distiller cuando es continuo, y palette cuando es categorico
                       limits = c(7,17),
                       direction = 1, 
                       na.value = "gray") +  #para invertir los colores 
  theme_bw()


# Mapa despues
ggplot() + 
  geom_polygon(data = esp_shp, 
               aes(x=long, y = lat, group = group), 
               fill = NA, color = "grey") + 
  geom_tile(data = df_d, 
            aes(x = x, y = y, fill = PM25), alpha=.8) +
  scale_fill_distiller(palette = "OrRd",    #se usa distiller cuando es continuo, y palette cuando es categorico
                       limits = c(0,50),
                       direction = 1, 
                       na.value = "gray") +  #para invertir los colores 
  theme_bw()


# Mapa cambios
ggplot() + 
  geom_polygon(data = esp_shp, 
               aes(x=long, y = lat, group = group), 
               fill = NA, color = "grey") + 
  geom_tile(data = df_dif, 
            aes(x = x, y = y, fill = PM25), alpha=.8) +
  scale_fill_distiller(palette = "RdBu",
                       limits = c(-2,2)) + 
  theme_bw()


geom_tile( data = clc,
           aes(x = x , y=y , fill = clc))


