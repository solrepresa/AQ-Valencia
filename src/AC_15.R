### Calidad del Aire en Comunitat Valenciana
### 11/02/2019 Valencia, Spain
### Sol Represa
### Archivo 15


### Objetivo: Trabajar con NDVI de MOD13A3


library(rgdal)
library(raster)
library(R.utils)
library(maptools)



# # # # # # # # # # # # # # # # # # # # # # # # # # 

#### 1 - Calidad + Recorte  + Reproyeccion + Resampling ####

# # # # # # # # # # # # # # # # # # # # # # # # # # 

crs_project = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Uso imagen del STACK de MODIS MCD19A2 como modelo para crear raster
# tomo el stack porq tiene el tamaño que me interesa conservar

MODIS <- raster("/home/usuario/Sol/aire_comunitat/stack/month/AOD_mes_01_max.tif")

raster_template <- raster(nrows = 239, ncols = 158, #100m de resolucion aprox
                          crs = crs_project, 
                          ext = extent(MODIS))  # toma las extensiones

dir_quality = "/home/usuario/Sol/aire_comunitat/variables/NDVI/quality"
dir_raster = "/home/usuario/Sol/aire_comunitat/variables/NDVI/raster"
  
id_aq <- dir(dir_quality, pattern = ".tif") # atencion q no haya .tif.xml en la carpeta..
id <- dir(dir_raster, pattern = ".tif")


## Cambiar sistema de referencia del shape

shape <- readShapePoly("/home/usuario/Sol/aire_comunitat/mapa/valencia_4326.shp",
                       proj4string = CRS(crs_project))

#shape_trans@bbox


for (i in 1:length(id_aq)){
  # 1) Abrir imagen calidad de NDVI
  raster_aq <- raster(paste(dir_quality, "/",id_aq[i], sep = ""))

  #range(raster_aq[raster_aq]) #ver rango de valores
  
  # 1) Crear mascara
  # plot(raster_aq)
  
  # intToBien convierte el numero a binario
  # substring se queda con los characteres de interes
  # integer transforma character a numero
  raster_aq[ raster_aq ] <- as.integer(substring(intToBin(raster_aq[ raster_aq]), 15, 16)) #nos quedamos con los bits 0-1
  raster_aq[ raster_aq != 0] <- NA
  # plot(raster_aq)
  
  # 2) Abrir raster de NDVI
  MODISraster <- raster(paste(dir_raster, "/", id[i], sep = ""))

  MODISraster <- MODISraster*0.0001   #factor de escala 
  # plot(MODISraster)
  
  # 3) Aplicar máscara
  MODISraster <- mask(MODISraster, raster_aq)
  # plot(salida)
  
  # 6) Resampling
  data_resampling <- raster::resample(MODISraster, raster_template)
  
  # 4) Recortar
  data_recorte <- crop(data_resampling, extent(-1.532917, 0.6930792, 37.84875, 40.79145 ))  #recorto imagen para Valencia
  
  # 7) Guardar
  writeRaster(data_recorte, paste("/home/usuario/Sol/aire_comunitat/variables/NDVI/crop_res/", id[i], sep = ""), 
              format = "GTiff", 
              overwrite = TRUE)
  rm(data_recorte, data_resampling, raster_aq, MODISraster)
}




# # # # # # # # # # # # # # # # # # # # # # # # #

#### 2 - Brick por year   ####

# # # # # # # # # # # # # # # # # # # # # # # # #


fs <- list.files(path="/home/usuario/Sol/aire_comunitat/variables/NDVI/crop_res/", pattern = "tif", full.names = TRUE)



k <- 1
l <- list()

for (j in 2008:2018){
  for (i in 1:length(fs)){
    year <- as.numeric(substring(fs[i], nchar(fs[i])-91, nchar(fs[i])-88))
    if (year == j){
      l[[k]] <- fs[i]
      k <- k + 1
    }
  }
  s <- brick(l)
  min_s <- min(s, na.rm=TRUE)  
  max_s <- max(s, na.rm=TRUE)
  mean_s <- calc(s, fun=mean, na.rm=TRUE)
  #median_s <- calc(s, fun = median, na.rm = TRUE)
  #sd_s <- calc(s, fun = sd, na.rm=TRUE)
  
  
  fun <- function(x) { sum(!is.na(x)) }
  n_s <- calc(s, fun = fun )
  
  writeRaster(min_s, filename = paste("NDVI_year_", j, "_min.tif", sep=""))
  writeRaster(max_s, filename = paste("NDVI_year_", j, "_max.tif", sep=""))
  writeRaster(mean_s, filename = paste("NDVI_year_", j, "_mean.tif", sep=""))
  #writeRaster(median_s, filename = paste("NDVI_year_", j, "_median.tif", sep=""))
  #writeRaster(sd_s, filename = paste("NDVI_year_", j, "_sd.tif", sep=""))
  writeRaster(n_s, filename = paste("NDVI_year_", j, "_n.tif", sep=""))
  
  print(j)
  print(Sys.time())
}


# # # # # # # # # # # # # # # # # # # # # # # # #

#### 3 - Brick por mes  ####

# # # # # # # # # # # # # # # # # # # # # # # # #



fs <- list.files(path="/home/usuario/Sol/aire_comunitat/variables/NDVI/crop_res/", pattern = "tif", full.names = TRUE)


for (j in 1:12){
  l <- list()
  k = 1
  for (i in 1:length(fs)){
    month <- as.Date(paste(substring(fs[i], nchar(fs[i])-87, nchar(fs[i])-85), 
                           substring(fs[i], nchar(fs[i])-91, nchar(fs[i])-88)), 
                           '%j %Y')
    month <- substr(as.character(month), 6,7)
    month <- as.numeric(month)
    if (j == month){
      l[[k]] <- fs[i]
      k <- k + 1
    }
  }  
  s <- raster::brick(l)
  min_s <- min(s, na.rm=TRUE)  
  max_s <- max(s, na.rm=TRUE)
  #median_s <- calc(s, fun = median, na.rm = TRUE)
  mean_s <- calc(s, fun = mean, na.rm=TRUE)
  #sd_s <- calc(s, fun = sd, na.rm=TRUE)
  
  fun <- function(x) { sum(!is.na(x)) }
  n_s <- calc(s, fun = fun )
  
  
  writeRaster(min_s, filename = paste("NDVI_mes_", j, "_min.tif", sep=""))
  writeRaster(max_s, filename = paste("NDVI_mes_", j, "_max.tif", sep=""))
  #writeRaster(median_s, filename = paste("NDVI_mes_", j,  "_median.tif", sep=""))
  writeRaster(mean_s, filename = paste("NDVI_mes_", j,  "_mean.tif", sep=""))
  #writeRaster(sd_s, filename = paste("NDVI_mes_", j,  "_sd.tif", sep=""))
  writeRaster(n_s, filename = paste("NDVI_mes_", j, "_n.tif", sep=""))
  
  rm(l,i,k)
  print(j)
  print(Sys.time())
}




# # # # # # # # # # # # # # # # # # # # # # # # #

#### 4 - MAPA de comparacion antes-despues  ####

# # # # # # # # # # # # # # # # # # # # # # # # #

antes <- "/home/usuario/Sol/aire_comunitat/variables/NDVI/crop_res/MOD13A3.A2008153.mosaic.006.2019088172506.psmcrp_000501319505.1_km_monthly_NDVI-1_km_monthly_NDVI.tif"
despues <- "/home/usuario/Sol/aire_comunitat/variables/NDVI/crop_res/MOD13A3.A2018152.mosaic.006.2019088172710.psmcrp_000501319514.1_km_monthly_NDVI-1_km_monthly_NDVI.tif"
                      
antes <- raster(antes)
despues <- raster(despues)

cambios <- antes - despues
writeRaster(cambios, filename = "NDVI_cambios_2008-06_2018-6.tif")
