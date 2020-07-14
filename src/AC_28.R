### Calidad del Aire en Comunitat Valenciana
### 03/05/2019 La Plata, Argentina
### Sol Represa
### Archivo 28



library(raster)
library(rgdal)



# Objetivo: 
# Generar raster a partir del modelo de prediccion con Random Forest  
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

dir_merra = "/media/usuario/Elements SE/MERRA/raster_res/"


# 1) Abrir modelo
modelo_rf <- readRDS("modelo_rf_valencia_100.rds")

#modelo_rf_10tree.rds
  

# Variables por dia
PS_i <- dir(dir_merra, pattern = ".PS.tif$")
RH_i <- dir(dir_merra, pattern = ".RH.tif$")
T_i <- dir(dir_merra, pattern = ".T.tif$")
T_i <- grep(pattern = "\\.T.tif$", x = T_i, value = TRUE)
U_i <- dir(dir_merra, pattern = ".U.tif$")
V_i <- dir(dir_merra, pattern = ".V.tif$")
BCSMASS_i <- dir(dir_merra, pattern = ".BCSMASS.tif$")
DMSSMASS_i <- dir(dir_merra, pattern = ".DMSSMASS.tif$")
DUSMASS_i <- dir(dir_merra, pattern = ".DUSMASS.tif$")
SO4SMASS_i <- dir(dir_merra, pattern = ".SO4SMASS.tif$")
SSSMASS25_i <- dir(dir_merra, pattern = ".SSSMASS25.tif$")
PRECTOT_i <- dir(dir_merra, pattern = ".PRECTOT.tif$")
SPEED_i <- dir(dir_merra, pattern = ".SPEED.tif$")
CLDHGH_i <- dir(dir_merra, pattern = ".CLDHGH.tif$")
CLDLOW_i <- dir(dir_merra, pattern = ".CLDLOW.tif$")
H1000_i <- dir(dir_merra, pattern = ".H1000.tif$")


date <- seq(from = as.Date("2008-01-01"), to = as.Date("2018-09-30"), by = "day")
AOD_i <- dir("stack/diarios/", pattern = ".tif$")


i = 1
j = 1

while(i < length(date)){
  if(as.character(date[i]) == substring(AOD_i[j], 10, 19)){
    
    aod <- paste("stack/diarios/", AOD_i[j], sep = "")  # de j porque tengo menos dias
    
    PS <- paste(dir_merra, PS_i[i], sep = "")
    RH <-paste(dir_merra,  RH_i[i], sep = "")
    T <- paste(dir_merra, T_i[i], sep = "")
    U <- paste(dir_merra, U_i[i], sep = "")
    V <- paste(dir_merra, V_i[i], sep = "")
    BCSMASS <- paste(dir_merra, BCSMASS_i[i], sep = "")
    DMSSMASS <- paste(dir_merra, DMSSMASS_i[i], sep = "") 
    DUSMASS <- paste(dir_merra, DUSMASS_i[i], sep = "")
    SO4SMASS <- paste(dir_merra, SO4SMASS_i[i], sep = "")
    SSSMASS25 <- paste(dir_merra, SSSMASS25_i[i], sep = "")
    PRECTOT <- paste(dir_merra, PRECTOT_i[i], sep = "")
    SPEED <- paste(dir_merra, SPEED_i[i], sep = "")
    CLDHGH <- paste(dir_merra, CLDHGH_i[i], sep = "")
    CLDLOW <- paste(dir_merra, CLDLOW_i[i], sep = "")
    H1000 <- paste(dir_merra, H1000_i[i], sep = "")
    
    # Variables unicas 
    DEM <- "variables/DEM/DEM.tif"
    CLC_1 <- "variables/CLC/CLC_1.tif" 
    
    l <- list(DEM, CLC_1, PS, RH, T, U, V, BCSMASS, DMSSMASS, DUSMASS,
         SO4SMASS, SSSMASS25, PRECTOT, SPEED, CLDHGH, CLDLOW, H1000, aod)
    
    # Armar brick
    archivo <- brick(l)
    
    names(archivo) <- c("DEM", "CLC_1", "PS", "RH", "T", "U", 
                        "V", "BCSMASS", "DMSSMASS", "DUSMASS", "SO4SMASS", "SSSMASS25", 
                        "PRECTOT", "SPEED", "CLDHGH", "CLDLOW", "H1000", "aod")
    
    raster_salida <- predict(archivo, 
                              modelo_rf, 
                              type = "raw", 
                              #progress = "text",
                              predict.all = FALSE,
                              na.rm =TRUE,  ## Agregue estas 2 condiciones nuevas
                              inf.rm = TRUE)
    
    writeRaster(raster_salida, 
                file = paste("modelo_rf_100/salida/RF-PM25-", as.character(date[i]), ".tif", sep=""), 
                format= "GTiff", 
                overwrite = TRUE )
    j <- j + 1
  }
  i <- i + 1
  print(i)
}








# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# cluster en raster:
# Ver pagina 34 https://rspatial.org/rs/rs.pdf
