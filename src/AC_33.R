### Calidad del Aire en Comunitat Valenciana
### 4/06/2019 La Plata, Argentina
### Sol Represa
### Archivo 33


# Rellenado de gaps en raster ##

# "we use an inverse-distance weighted method 
# with a 50km smoothing distance using the IDL function “GridData”."


library(raster)
library(gstat)
library(gdata)
library(maptools)
library(dismo) #kfold
library(caret)
library(parallel)

# # # # # # # # # # # # # # # # # # # # # # # # #

# 1- Obtener puntos de raster

# # # # # # # # # # # # # # # # # # # # # # # # #

dir = "stack/diarios/"
dir_salida = "stack/IDW/"
crs_project = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

fs <- list.files(path = dir, 
                 pattern = "tif",
                 full.names = FALSE)

# Crear raster template >>> spatial Pixels DF
r <- raster(paste(dir, fs[1], sep=""))
raster_template <- raster(nrows = 239, ncols = 158, #100m de resolucion aprox
                          crs = crs_project, 
                          ext = extent(r))  # toma las extensiones
idw.grid <- rasterToPoints(raster_template, spatial = TRUE)
gridded(idw.grid) <- TRUE   #SpatialPixelsDataFrame
rm(r)

shape <- readShapePoly("/home/usuario/Sol/aire_comunitat/mapa/valencia_4326.shp",
                       proj4string = CRS(crs_project))


RMSE_IDW <- data.frame()
kfold = 5 # numero de k-fold cross validation



# Para mejorar la performance, paralelizar el proceso!
cl <- makePSOCKcluster(3)
registerDoParallel(cl) #registro del proceso paralelo

for( i in 663:length(fs)){
  print(i)
  raster_fs <- raster(paste(dir, fs[i], sep=""))
  filename <- paste("IDW-", fs[i], sep="")
  raster_points <- as.data.frame(rasterToPoints(r))
  rm(raster_fs)
  
  if(nrow(raster_points) > 3776){  #### control: raster con un 10% de datos
    coordinates(raster_points) <- ~x+y
    #raster_points@proj4string
    projection(raster_points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    names(raster_points)[1] <- "AOD"

    set.seed(513)  # 5-fold cross-validation
    
    kf <- kfold(nrow(raster_points), k = kfold)
    rmse <- rep(NA, kfold)
    for (k in 1:kfold) {   
      test <- raster_points[kf == k, ]
      train <- raster_points[kf != k, ]
      kat.idw <- gstat::idw(AOD ~ 1, train, idw.grid, 
                            debug.level = -1,   #para ver grado de progreso
                            idp = 5) # IDW power
      
      final.idw <- raster(kat.idw)
      t <- SpatialPoints(test, proj4string = CRS(crs_project)) #convierto formato para aplicar extract
      #plot(final.idw)
      model <- extract(final.idw, t)
      rmse[k] <- caret::RMSE(test$AOD, model)
    }
    
    tabla <- data.frame(archivo = filename, RMSE = mean(rmse, na.rm = TRUE))
    RMSE_IDW <- rbind(RMSE_IDW, tabla)
    
    # Recorto el ultimo armado para valencia
    data_recorte <- crop(final.idw, shape)  
    data_recorte <- mask(data_recorte, shape)  
    
    # Guardar
    writeRaster(data_recorte, paste(dir_salida, filename, sep = ""), 
                format = "GTiff",
                overwrite = TRUE)
    
    rm(data_recorte, raster_template, train, test, idw.grid, kat.idw)
    
  }
  
}

stopCluster(cl) #cerrar
remove(cl)
registerDoSEQ()

write.csv(RMSE_IDW, "error_IDW_diarios_aod.csv" , row.names = FALSE)