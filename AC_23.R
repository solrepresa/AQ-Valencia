### Calidad del Aire en Comunitat Valenciana
### 18/02/2019 Valencia, Spain
### Sol Represa
### Archivo 23


## Objetivo: extraer datos de raster en sitios de monitoreo y unir para formar una única bases

library(foreign)
library(raster)

## ATENCION!
# Antes de correr:
# ¿tengo todos los archivos de distancia?





#extent(rst_fire)
#res(rst_fire)
#resample(r1, r2, "bilinear")






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

DEM <- raster("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\DEM\\DEM.tif")
DEM_ds <- raster::extract(DEM, sit)
DEM_ds <- data.frame(DEM_ds)
DEM_ds$Codigo <- sitios$Codigo

variables_modelo[[1]] <- DEM_ds

rm(DEM, DEM_ds)

# 2) CLC >> unicos por categoria  

CLC_i <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\CLC", pattern = ".tif")

ds <- data.frame()


for( i in 1:length(CLC_i)){
  CLC_raster <- raster(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\CLC\\", CLC_i[i], sep=""))
  CLC_ds <- raster::extract(CLC_raster, sit)
  CLC_ds  <- as.data.frame(CLC_ds)
  CLC_ds$Codigo <- sitios$Codigo
  CLC_ds$CLC <- substring(CLC_i[i], 1, 5)
  ds <- rbind(ds, CLC_ds)
}



variables_modelo[[2]] <- ds


rm(CLC_raster, CLC_ds, ds, CLC_i)



## 3) NDVI

NDVI_i <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\NDVI\\crop_res", pattern = ".tif")


NDVI_ds <- data.frame()
for( i in 1:length(NDVI_i)){
  date <- substring(NDVI_i[i], 10, 16) #tomar dato de la fecha
  NDVI_raster <- raster(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\NDVI\\crop_res\\",NDVI_i[i], sep=""))
  ds <- raster::extract(NDVI_raster, sit)
  ds <- t(ds )
  ds <- as.data.frame(ds )
  ds$date  <- date 
  NDVI_ds <- rbind(ds, NDVI_ds)
}


names(NDVI_ds) <- c(as.character(sitios_names), "date")

variables_modelo[[3]] <- NDVI_ds

rm(NDVI_ds, NDVI_i, NDVI_raster, ds, date)


# 4) MERRA
MERRA_i <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\MERRA\\raster", pattern = ".tif")


MERRA_ds <- data.frame()
for( i in 1:length(MERRA_i)){
  cod <-  substring(MERRA_i[i], 37, nchar(MERRA_i[i])-4) #tomar dato de la variable
  date <- substring(MERRA_i[i], 28, 35) #tomar dato de la fecha
  MERRA_raster <- raster(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\MERRA\\raster\\", MERRA_i[i], sep =""))
  ds <- raster::extract(MERRA_raster, sit)
  ds <- t(ds )
  ds <- as.data.frame(ds )
  ds$cod  <- cod 
  ds$date  <- date 
  MERRA_ds <- rbind(ds, MERRA_ds)
}


names(MERRA_ds) <- c(as.character(sitios_names), "Codigo", "date")

variables_modelo[[4]] <- MERRA_ds

rm(MERRA_ds, MERRA_raster, ds, MERRA_i)


## 5) FIRE

fire_i <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\FIRE\\diario", pattern = ".tif")

fire_ds <- data.frame()
for( i in 1:length(fire_i)){
  date <- substring(fire_i[i], 6, 14) #tomar dato de la fecha
  fire_raster <- raster(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\FIRE\\diario\\", fire_i[i], sep = ""))
  ds <- raster::extract(fire_raster, sit)
  ds <- t(ds )
  ds <- as.data.frame(ds )
  ds$date  <- date 
  fire_ds <- rbind(ds, fire_ds)
}


names(fire_ds) <- c(as.character(sitios_names), "date")


variables_modelo[[5]] <- fire_ds

rm(fire_ds, fire_raster, fire_i)


# 3) Distancia >> unicos por categoria // faltas rutas

DIS_i <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\RUTAS\\salida", pattern = ".tif")


DIS_ds <- data.frame()
for( i in 1:length(DIS_i)){
  cod <- substring(DIS_i[i], 10, 23) #tomar dato de la variable
  DIS_raster <- raster(DIS_i[i])
  ds <- raster::extract(DIS_raster, sit)
  ds  <- t(ds)
  ds  <- as.data.frame(ds )
  ds$cod  <- cod 
  DIS_ds <- rbind(ds, DIS_ds)
  
}


names(DIS_ds) <- c("File", "Cod", as.character(sitios_names))

variables_modelo[[3]] <- DIS_ds


# # # # # # # # # # # # # # # # # # 

## SAVE RDS

# # # # # # # # # # # # # # # # # # 

saveRDS(variables_modelo, "variables_modelo.rds")

variables_modelo <- readRDS("variables_modelo.rds")
