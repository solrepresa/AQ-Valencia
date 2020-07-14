### Calidad del Aire en Comunitat Valenciana
### 11/02/2019 Valencia, Spain
### Sol Represa
### Archivo 14



## Objetivo: 
# - Analizar raster mensuales y anuales de AOD en Valencia


library(raster)
library(tmap)

fs <- list.files(path="C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\stack\\year", pattern = "tif", full.names = TRUE)


# Juntar todos los raster anuales

k = 1
l <- list()

for( i in 1:length(fs)){
  variable <- substring(fs[i], 71, nchar(fs[i])-4)
  if( variable == "median"){
    l[k] <- fs[i]
    k <- k + 1
  }
}

aod <- brick(l)

plot(aod)

min_aod <- min(aod, na.rm = TRUE)
median_aod <- calc(aod, fun = median, na.rm = TRUE)
max_aod <- max(aod, na.rm = TRUE)


# Ver mapas
aod_1 <- raster(l[[1]])
tm_shape(aod_1) + tm_raster()


#range(aod) #ver rango de valores


# Extraer valores de un punto

cell <- cellFromXY(r.brick, c(526700, 4961400))
r.brick[cell]



elev3b <- disaggregate(elev2, fact=2)
tm_shape(elev3b) + tm_raster(style= "cont") + tm_legend(outside = TRUE)