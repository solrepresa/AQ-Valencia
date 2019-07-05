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

# # # # # # # # # # # # # # # # # # # # # # # # #

# 1- IDW

# # # # # # # # # # # # # # # # # # # # # # # # #


dir = "modelo_rf_100/salida/"
dir_salida = "modelo_rf_100/IDW/"
crs_project = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


shape <- readShapePoly("/home/usuario/Sol/aire_comunitat/mapa/valencia_4326.shp",
                       proj4string = CRS(crs_project))


fs <- list.files(path = dir, 
                 pattern = "tif")

RMSE_IDW <- data.frame()

for( i in 1:length(fs)){
  r <- raster(paste(dir, fs[1], sep=""))
  filename <- paste("IDW-", fs[1], sep="")
  pp <- as.data.frame(rasterToPoints(r))
  coordinates(pp) <- ~x+y
  #pp@proj4string
  projection(pp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  names(pp)[1] <- "PM25"
  

  # Crear raster template >>> spatial Pixels DF
  raster_template <- raster(nrows = 239, ncols = 158, #100m de resolucion aprox
                            crs = crs_project, 
                            ext = extent(r))  # toma las extensiones
  idw.grid <- rasterToPoints(raster_template, spatial = TRUE)
  gridded(idw.grid) <- TRUE   #SpatialPixelsDataFrame
  
  rm(r)
  
  # 5-fold cross-validation
  set.seed(513)
  kf <- kfold(nrow(pp), k =5)
  rmse <- rep(NA, 5)
  for (k in 1:5) {
    test <- pp[kf == k, ]
    train <- pp[kf != k, ]
    kat.idw <- gstat::idw(PM25 ~ 1, train, idw.grid, 
                          debug.level = -1,   #para ver grado de progreso
                          idp = 5) # IDW power

    final.idw <- raster(kat.idw)
    t <- SpatialPoints(test, proj4string = CRS(crs_project)) #convierto formato para aplicar extract
    #plot(final.idw)
    model <- extract(final.idw, t)
    rmse[k] <- caret::RMSE(test$PM25, model)
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

write.csv(RMSE_IDW, "error_IDW.csv" , row.names = FALSE)
