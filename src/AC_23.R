### Calidad del Aire en Comunitat Valenciana
### 18/02/2019 Valencia, Spain
### Sol Represa
### Archivo 23


## Objetivo: extraer datos de raster en sitios de monitoreo y unir para formar una única bases

library(foreign)
library(raster)
library(lubridate)
library(dplyr)

## ATENCION!
# Antes de correr:
# ¿tengo todos los archivos de distancia? NO, lo voy a hacer sin distancia



#extent(rst_fire)
#res(rst_fire)
#resample(r1, r2, "bilinear")

setwd("/home/usuario/Sol/aire_comunitat")

# # # # # # # # # # # # # # # # # # # # # # 

## Coordenadas de puntos de estaciones ####

# # # # # # # # # # # # # # # # # # # # # # 

# estaciones <- # COMPLETAR
# tienen la misma proyeccion?


sitios <- read.csv("estaciones_ut_conts.csv", sep = ";", dec = ".")
sitios <- data.frame(sitios$Latitud, sitios$Longitud, sitios$Estacion)
names(sitios) <- c("Latitud", "Longitud", "Codigo")

sitios <- sitios[order(sitios$Codigo),]
sitios_names <- t(sitios$Codigo)

sit <- data.frame(sitios$Longitud, sitios$Latitud)


variables_modelo <- list()


# # # # # # # # # # # # # # # # # # # # # # 

## Extraer de cada data set los datos en los puntos####

# # # # # # # # # # # # # # # # # # # # # # 



## 1) DEM >> unico

DEM <- raster("/home/usuario/Sol/aire_comunitat/variables/DEM/DEM.tif")
DEM_ds <- raster::extract(DEM, sit)
DEM_ds <- data.frame(DEM_ds)
DEM_ds$Codigo <- sitios$Codigo
DEM_ds$Variable <- "DEM"
names(DEM_ds)[1] <- "Value" 

variables_modelo[[1]] <- DEM_ds

rm(DEM, DEM_ds)

# 2) CLC >> unicos por categoria  

CLC_i <- dir("/home/usuario/Sol/aire_comunitat/variables/CLC", pattern = ".tif")

ds <- data.frame()


for( i in 1:length(CLC_i)){
  CLC_raster <- raster(paste("/home/usuario/Sol/aire_comunitat/variables/CLC/", CLC_i[i], sep=""))
  CLC_ds <- raster::extract(CLC_raster, sit)
  CLC_ds  <- as.data.frame(CLC_ds)
  CLC_ds$Codigo <- sitios$Codigo
  CLC_ds$CLC <- substring(CLC_i[i], 1, 5)
  ds <- rbind(ds, CLC_ds)
}


ds$CLC_ds <- round(ds$CLC_ds, digits = 2)
ds[is.na(ds$CLC_ds),1] <- 0 


variables_modelo[[2]] <- ds


rm(CLC_raster, CLC_ds, ds, CLC_i)



## 3) NDVI  ####

NDVI_i <- dir("/home/usuario/Sol/aire_comunitat/variables/NDVI/crop_res", pattern = ".tif")


NDVI_ds <- data.frame()
for( i in 1:length(NDVI_i)){
  date <- substring(NDVI_i[i], 10, 16) #tomar dato de la fecha
  NDVI_raster <- raster(paste("/home/usuario/Sol/aire_comunitat/variables/NDVI/crop_res/",NDVI_i[i], sep=""))
  ds <- raster::extract(NDVI_raster, sit)
  ds <- t(ds )
  ds <- as.data.frame(ds )
  ds$date  <- date 
  NDVI_ds <- rbind(ds, NDVI_ds)
}


names(NDVI_ds) <- c(as.character(sitios_names), "date")
NDVI_ds$date <- as.Date(NDVI_ds$date, "%Y%j")

variables_modelo[[3]] <- NDVI_ds

rm(NDVI_ds, NDVI_i, NDVI_raster, ds, date)


# 4) MERRA ####

MERRA_i <- dir("/media/usuario/Elements SE/MERRA/raster_res", pattern = ".tif$")

# Atencion!
# Se colaron qalgunas imagenes q estran repetidas y con el nombre malo
#quitar <- grep("D:",MERRA_i)
#MERRA_i <- MERRA_i[-quitar]

cod_i <-  substring(MERRA_i, 37, nchar(MERRA_i)-4) #tomar dato de la variable
cod_i <- cod_i[!duplicated(cod_i)]



for( j in 1:length(cod_i)){
  tabla <- MERRA_i[which(substring(MERRA_i, 37, nchar(MERRA_i)-4) == cod_i[j])]
  MERRA_ds <- data.frame()
  for( i in 1:length(tabla)){   # 91982    #error i= 30724
    cod <-  substring(tabla[i], 37, nchar(tabla[i])-4) #tomar dato de la variable
    date <- substring(tabla[i], 28, 35) #tomar dato de la fecha
    MERRA_raster <- raster(paste("/media/usuario/Elements SE/MERRA/raster_res/", tabla[i], sep =""))
    ds <- raster::extract(MERRA_raster, sit)
    ds <- t(ds )
    ds <- as.data.frame(ds )
    ds$cod  <- cod 
    ds$date  <- date 
    MERRA_ds <- rbind(ds, MERRA_ds)
  }
  names(MERRA_ds) <- c(as.character(sitios_names), "Codigo", "date")
  variables_modelo[[3 + j]] <- MERRA_ds
  print(j)  # PRINT del numero de variables analizadas VER cod_i
  print(Sys.time())
}


rm(MERRA_ds, MERRA_raster, ds, MERRA_i)


## 5) FIRE ####

fire_i <- dir("/home/usuario/Sol/aire_comunitat/variables/FIRE/diario", pattern = ".tif")

fire_ds <- data.frame()
for( i in 1:length(fire_i)){
  date <- substring(fire_i[i], 6, 13) #tomar dato de la fecha
  fire_raster <- raster(paste("/home/usuario/Sol/aire_comunitat/variables/FIRE/diario/", fire_i[i], sep = ""))
  ds <- raster::extract(fire_raster, sit)
  ds <- t(ds )
  ds <- as.data.frame(ds)
  ds$date  <- date 
  fire_ds <- rbind(ds, fire_ds)
}

# Ver estas lineas:
fire_ds$date <- as.Date(fire_ds$date, format = "%Y-%j") 
fire_ds$FIRE <- is.na(fire_ds$FIRE)
#

names(fire_ds) <- c(as.character(sitios_names), "date")


variables_modelo[[27]] <- fire_ds

rm(fire_ds, fire_raster, fire_i)


# 3) Distancia >> unicos por categoria // falta arreglar esta parte

DIS_i <- dir("/home/usuario/Sol/aire_comunitat/variables/RUTAS/salida", pattern = "_AG.tif$")


DIS_ds <- data.frame()
for( i in 1:length(DIS_i)){
  cod <- substring(DIS_i[i], 11, nchar(DIS_i[i]) - 7) #tomar dato de la variable
  DIS_raster <- raster(paste("/home/usuario/Sol/aire_comunitat/variables/RUTAS/salida/", DIS_i[i], sep =""))
  ds <- raster::extract(DIS_raster, sit)
  ds  <- t(ds)
  ds  <- as.data.frame(ds )
  ds$cod  <- cod 
  DIS_ds <- rbind(ds, DIS_ds)
  
}


names(DIS_ds) <- c("File", "Cod", as.character(sitios_names))

variables_modelo[[28]] <- DIS_ds


# # # # # # # # # # # # # # # # # # 

## SAVE RDS

# # # # # # # # # # # # # # # # # # 

saveRDS(variables_modelo, "variables_modelo.rds")

#variables_modelo <- readRDS("variables_modelo.rds")
