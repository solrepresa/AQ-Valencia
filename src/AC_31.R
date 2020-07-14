### Calidad del Aire en Comunitat Valenciana
### 14/05/2019 La Plata, Argentina
### Sol Represa
### Archivo 31


# Objetivo: 
# - trabajar con MERRA de AOD
# - Unir con MAIAC
# - Modelar MAIAC faltantes con Random Forest (se hace en "AC_32.R")

# # # # # # # # # # # # # # # # # # # # # # # # #

# 1) MAIAC hdf >> MERRA tif   ####
### Objetivo: Abrir .hdf de MERRA y guardar en tif las SDS

# # # # # # # # # # # # # # # # # # # # # # # # #

library(gdalUtils)
library(raster)
library(rgdal)
library(R.utils)
library(maptools)


setwd("/media/usuario/Elements SE/MERRA/inst3_2d_gas_Nx")  #fundamental para q funcione gdal!
#id <- dir(pattern = ".hdf") 

id <- list.files(path = getwd(),
                 pattern = "*.hdf",
                 full.names = FALSE)

## Shape recorte

crs_project = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
shape <- readShapePoly("/home/usuario/Sol/aire_comunitat/mapa/valencia_4326.shp",
                       proj4string = CRS(crs_project))


# Para resampling
# Uso imagen MODIS MCD19A2 como modelo para crear raster
MODIS <- raster("/home/usuario/Sol/aire_comunitat/stack/month/AOD_mes_01_max.tif")

raster_template <- raster(nrows = 239, ncols = 158, #100m de resolucion aprox
                          crs = crs_project, 
                          ext = extent(MODIS))  # toma las extensiones

mapply(file = id,
       FUN = function(file){
         tryCatch(
           { #si no sale error quiero que haga todo lo siguiente:
             
             sds <- get_subdatasets(file) 
             name_sds <- sds[1]
             filename <-  paste(substr(name_sds, 19, 54), substr(name_sds, 71, nchar(name_sds)), ".tif", sep="")
             gdal_translate(name_sds, dst_dataset = filename)
             MIRRAraster <- raster(filename)
             
             # 1) Reproyectar
             MIRRAraster <- projectRaster(MIRRAraster,
                                          crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ",
                                          method = "bilinear")
             
             # 2) Resampling
             rst_resampling <- raster::resample(MIRRAraster, raster_template)
             # >> Metodos de resampling: https://gisgeography.com/raster-resampling/
             
             # 3) Recortar
             data_recorte <- crop(rst_resampling, shape)  #recorto imagen para Valencia
             
             # 4) Guardar resampling
             writeRaster(data_recorte, paste("/media/usuario/Elements SE/MERRA/raster_aod/", filename, sep = ""), 
                         format = "GTiff",
                         overwrite = TRUE)
             
             rm(data_recorte, MIRRAraster, rst_resampling)
             
           },
           
           error = function(error_message){
             message("ATENTI")
             message(error_message)
             
           }
         )
       }
)


# # # # # # # # # # # # # # # # # # # # # # # # #

# 2) Coordenadas de cada pixel + datos AOD MERRA  ####

# # # # # # # # # # # # # # # # # # # # # # # # #


id_merra <- dir(path = "/media/usuario/Elements SE/MERRA/raster_aod",
                 pattern = "*.tif",
                 full.names = FALSE)
j = 1


raster <- raster( paste("/media/usuario/Elements SE/MERRA/raster_aod/", id_merra[j], sep ="") )
merra_point <- rasterToPoints(raster)  # punto en el centro del pixel, recorre por fila

MERRA <- as.data.frame(merra_point)
names(MERRA)[3] <- "MERRA"
MERRA$fecha <- substr(id_merra[j], 28, 35)

for(j in 3002:length(id_merra)){
  raster <-  raster( paste("/media/usuario/Elements SE/MERRA/raster_aod/", id_merra[j], sep ="") )
  raster_matrix <- rasterToPoints(raster)  # punto en el centro del pixel, recorre por fila
  
  tabla <- as.data.frame(raster_matrix)
  names(tabla)[3] <- "MERRA"
  tabla$fecha <- substr(id_merra[j], 28, 35)
  
  MERRA <- rbind(MERRA, tabla)
  print(j)
  
}



write.csv(MERRA, "MERRA_pixel_diario.csv", row.names = FALSE)

rm(tabla, r, raster, raster_matrix)

# # # # # # # # # # # # # # # # # # # # # # # # #

# 3) Coordenadas de cada pixel datos AOD MAIAC ####

# # # # # # # # # # # # # # # # # # # # # # # # #


id_maiac <- dir(path = "stack/diarios",
                pattern = "*.tif",
                full.names = FALSE)

k = 1

raster_maiac <- raster( paste("stack/diarios/", id_maiac[k], sep ="") )
raster_maiac[is.na(raster_maiac[])] <- 99999
maiac_point <- rasterToPoints(raster_maiac)  # punto en el centro del pixel, recorre por fila

MAIAC <- as.data.frame(maiac_point)
names(MAIAC)[3] <- "MAIAC"
MAIAC$fecha <- substr(id_maiac[k], 10, 19)

MAIAC[which(MAIAC$MAIAC == 99999),3] <- NA
write.table(MAIAC, "MAIAC_pixel_diario_NA.csv", 
            sep = ",", 
            append = FALSE, #ATENTI! si existe el archivo lo arranca de nuevo
            quote = FALSE,
            col.names = TRUE, # ATENTI! para q tome los nombres
            row.names = FALSE)
rm(MAIAC, maiac_point, raster_maiac)

for(k in 2:length(id_maiac)){
  r <- id_maiac[k]
  raster <- raster( paste("stack/diarios/", id_maiac[k], sep ="") )
  raster[is.na(raster[])] <- 99999
  raster_matrix <- rasterToPoints(raster)  # punto en el centro del pixel, recorre por fila
  
  tabla <- as.data.frame(raster_matrix)
  if(dim(tabla)[1] != 0){
    names(tabla)[3] <- "MAIAC"
    tabla$fecha <- substr(id_maiac[k], 10, 19)
    tabla$fecha <- paste(substr(tabla$fecha, 1, 4), substr(tabla$fecha, 6, 7), substr(tabla$fecha, 9, 10), sep="")
    
    tabla[which(tabla$MAIAC == 99999),3] <- NA
    write.table(tabla, "MAIAC_pixel_diario_NA.csv", 
                sep = ",", quote = FALSE,
                append = TRUE, 
                col.names = FALSE, row.names = FALSE)
  }
  rm(tabla, raster_matrix, raster)
  print(k)
}




## Analisis MAIAC

MAIAC <- read.csv("MAIAC_pixel_diario_NA.csv")
MAIAC$V4 <- as.character(MAIAC_p$V4)


# # # # # # # # # # # # # # # # # # # # # # # # #

# 4) Unificar AOD MERRA - MAIAC ####

# # # # # # # # # # # # # # # # # # # # # # # # #


MAIAC <- read.csv("MAIAC_pixel_diario_NA.csv")
MERRA <- read.csv("MERRA_pixel_diario.csv") #object.size(MERRA)

# Reducir archivo en memoria
MERRA <- MERRA[, 3:4] #object.size(MERRA)
MAIAC$fecha <- as.character(MAIAC$fecha)

MAIAC$fecha <- paste(substr(MAIAC$fecha, 1, 4), substr(MAIAC$fecha, 6, 7), substr(MAIAC$fecha, 9, 10), sep="")

i = 1
j = 1


while( i < nrow(MAIAC)){
  if(MAIAC[i, 4] == MERRA[j, 2]){
    tabla <- MAIAC[i:(i + 37761), 1:3]
    tabla[, 4] <- MERRA[j:(j + 37761),1 ]
    tabla[, 5] <- MERRA[j:(j + 37761),2 ]
    write.table(tabla, "MAIAC_MERRA_pixel_diarios.csv", 
                sep = ",", quote = FALSE,
                append = TRUE, #Atenti! borrar el file 
                col.names = FALSE, 
                row.names = FALSE)
    rm(tabla)
    i = i + 37762
    print(i)
  }

  j = j + 37762
}


MAIAC <- read.csv("MAIAC_MERRA_pixel_diarios.csv", header = FALSE) 
names(MAIAC) <- c( "x", "y", "MAIAC", "MERRA", "fecha")
write.csv(MAIAC, "MAIAC_MERRA_pixel_diarios.csv", 
                   row.names = FALSE, quote = FALSE)
