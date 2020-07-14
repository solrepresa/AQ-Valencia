## Comunidad Valenciana
## 04 de Octubre, 2019
##  Sol Represa
## Archivo 34

# Generar modelos de RR y de FA > 0 para PM2.5
# RR = ( (x + 1) / (x0 - 1))^B  
# donde B = 0.155 para mortalidad cardiovascular
# y B = 0.232 para cancer de pulmon
# x0 = 3

# FA = (RR - 1)/ RR

dir = "modelo_rf_1000/RR_anual/"
  
id <- dir("modelo_rf_1000/year/", 
          pattern = "_mean.tif",
          full.names = TRUE) # atencion q no haya .tif.xml en la carpeta..

for(i in 1:length(id)){
  rs <- raster(id[i])
  y <- substring(id[i], 32, 35)
  RR <- ( (rs + 1) / (3 - 1) ) ^ 0.232 # RR cancer de pulmon
  FA <- (RR - 1)/ RR

  writeRaster(RR, filename = paste(dir, "PM25_RR_", y, ".tif", sep=""), overwrite=TRUE)
  writeRaster(FA, filename = paste(dir, "PM25_FA_", y, ".tif", sep=""), overwrite=TRUE)
  
}
