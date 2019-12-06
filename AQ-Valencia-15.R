### Sol Represa
### 22/02/2019
### AQ - Valencia
# Modificado 7 el 08, 2019, La Plata, Argentina


# Objetivo: Analisis de patrones de puntos

library(spatstat)
library(rgdal)
library(ggmap)


# # # # # # # # # # # # # # # # # # # # # # 


sitios <- read.csv("/home/usuario/Sol/AQ-Valencia/estaciones-valencia-todas.csv", 
                   sep = ",")
#sitios$Longitud <- as.numeric(gsub(",", ".", gsub("\\.", "", sitios$Longitud)))
#sitios$Latitud <- as.numeric(gsub(",", ".", gsub("\\.", "", sitios$Latitud)))
#write.csv(sitios, "estaciones-valencia-todas.csv", row.names = FALSE)

sitios <- data.frame(sitios$Latitud, sitios$Longitud, sitios$Estacion)
names(sitios) <- c("Latitud", "Longitud", "Codigo")


esp_shp <- readOGR("mapa/valencia.shp")
shape_trans <- spTransform(esp_shp, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))


register_google(key = "AIzaSyDL10fYmvbe1w0E8y2tfGF6fcDO5x7osBQ")

# # # # # # # # # # # # # # # # # # # # # # # # 

## 1) Distribucion espacial de las estaciones

# # # # # # # # # # # # # # # # # # # # # # # # 

# Convertir a clase ppp
# x = longitud // y = latitud

sitios.ppp <-ppp(sitios$Longitud, sitios$Latitud,
                 c(min(sitios$Longitud), max(sitios$Longitud)), 
                 c(min(sitios$Latitud), max(sitios$Latitud)),
                 marks = factor(sitios$Codigo ), 
                 unitname = c("Codigo"))

plot(sitios.ppp, 
     #cols = 2:9, 
     pch = 20, 
     main = "")




# # # # # # # # # # # # # # # # # # # # # # # #

## 2) Frecuencia relativa por cuadrante

# # # # # # # # # # # # # # # # # # # # # # # #

res <- try(Q <- quadratcount(sitios.ppp, nx = 4, ny = 5))
if(inherits(res, "try-error")){next}
Q <- quadratcount(sitios.ppp, nx = 4, ny = 5)
Q_tot <- sum(Q)
plot(sitios.ppp, 
    # cols = "#D55E00", 
     legend = FALSE, pch = 20, 
     main = "Frecuencia Relativa")
plot( round(Q/Q_tot, 2), add = TRUE, col= "#0072B2", cex = 1)




# # # # # # # # # # # # # # # # # # # # # # # #

## 3) Intensidad por cuadrante

# # # # # # # # # # # # # # # # # # # # # # # #

plot(density(sitios.ppp), main = "Intensidad")


## Va con ggplot 



map <- get_map(location = c( -1.6, #min Longitud / x
                             37.7,  #min Latitud / y
                             0.7, #max Longitud / x
                             40.9),  #max Latitud / y
               #zoom = 8, 
               zoom = 7,  #se ve toda la comunidad 
               maptype ="roadmap", 
               api_key = "AIzaSyDL10fYmvbe1w0E8y2tfGF6fcDO5x7osBQ")


mapPoints <- ggmap(map)
mapPoints + 
  stat_density2d(data = sitios, aes(Longitud, Latitud, fill= ..level..), 
                 alpha = 0.5, 
                 geom = "polygon") + 
  geom_polygon(data = esp_shp, 
               aes(x=long, y = lat, group = group), 
               fill = NA, color = "black") +

  geom_point(data = sitios, aes(Longitud, Latitud), colour="red", alpha = .5) +
  scale_fill_gradientn(colours = rev(rainbow(100, start=0, end=0.75))) +
  labs( x = "Longitud", y = "Latitud")





# # # # # # # # # # # # # # # # # # # # # # # #

## 4) Prueba proceso homogeneo

# # # # # # # # # # # # # # # # # # # # # # # #

(prueba.chi.cuadrado <- quadrat.test(sitios.ppp, nx = 2, ny = 2))

plot(sitios.ppp, 
     cols = "#D55E00", 
     legend = FALSE, pch = 20, 
     main= "Prueba Chi-Cuadrado")

plot(prueba.chi.cuadrado, 
     add = TRUE, cex = 0.6, col = "#0072B2")

