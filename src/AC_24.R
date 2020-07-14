### Calidad del Aire en Comunitat Valenciana
### 22/02/2019 Valencia, Spain
### Sol Represa
### Archivo 24


library(dplyr)
library(reshape2)
library(lubridate)


# Elaboracion de base de datos para introducir a TensorFlow
# Res , V1, V2, V3, V4, V5 ....




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 1) Armar base por estaciones ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Para faciltiar el trabajo voy a armar matrices de 22 * 4000
# 1 por cada estación

estaciones <- c("X03014006", "X03014009", "X03031002", "X03065006", "X03066003",
                "X12005005", "X12009007", "X12028001", "X12032001", "X12040008", 
                "X12040010", "X12093004", "X12099001", "X12129001", "X12140002", 
                "X12141002", "X46010001", "X46077006", "X46095001", "X46102002", 
                "X46220010", "X46250030", "X46250046", "X46250048", "X46258001")


variables_modelo <- readRDS("variables_modelo.rds") ## FALTA incorporar FIRE + distancias


fecha <- seq( as.Date("2008-01-01"), as.Date("2018-12-13"), by = "day")

var_estac_model <- list()

for( k in 1: length(estaciones)){
  BASE <- data.frame(fecha)
  
  # 1: DEM ###
  V1 <- variables_modelo[[1]]
  names(V1)[1] <- "Value" 
  V1 <- V1 %>% filter(Codigo == estaciones[k])
  BASE$Codigo <- estaciones[k]
  BASE$DEM <- V1$Value
  rm(V1)
  
  # 2: CLC ###
  V2 <- variables_modelo[[2]]
  names(V2) <- c("Value", "Codigo", "Variable")
  V2 <- V2 %>% filter(Codigo == estaciones[k])
  BASE$CLC_1 <- V2[1,1]
  BASE$CLC_2 <- V2[2,1]
  BASE$CLC_3 <- V2[3,1]
  BASE$CLC_4 <- V2[4,1]
  BASE$CLC_5 <- V2[5,1]
  rm(V2)
  
  # 3: NDVI ###
  V3 <- variables_modelo[[3]]
  V3 <- melt(V3, id = "date")
  names(V3) <- c("fecha", "Codigo", "NDVI")
  V3 <- V3 %>% filter(Codigo == estaciones[k])

  df <- V3[,c(1,3)]
  df <- do.call("rbind", lapply(1:nrow(df), function(i) 
    data.frame(fecha = seq(df$fecha[i], 
                          (seq(df$fecha[i], length = 2, by = "months") - 1)[2], by = "1 days"), 
               value = df$NDVI[i])))
  
  BASE <- merge(BASE, df, by = "fecha", all = TRUE)
  rm(V3, df)
  
  
  # 4:26 MERRA ###
  for( i in 4:26){
    V4 <- variables_modelo[[i]]
    V4 <- melt(V4, id = c("date", "Codigo"))
    names(V4) <- c("fecha", "Variable", "Codigo", "Value")
    V4 <- V4 %>% filter(Codigo == estaciones[k])
    
    V4$fecha <- as.Date(V4$fecha, "%Y%m%d")
    names(V4)[4] <- V4[1,2]
    V4 <- V4[,c(1,4)]
    BASE <- merge(BASE, V4, by = "fecha", all = TRUE)
  }
  rm(V4)
  

  
  # 27 FIRE ### <<<< No tengo representatividad de esta variable en todos los meses, todas las variables
  #V5 <- variables_modelo[[27]]
  #V5 <- melt(V5, id = "date")
  #names(V5) <- c("fecha", "Codigo", "FIRE")
  #V5 <- V5 %>% filter(Codigo == estaciones[k])

  

  #BASE <- merge(BASE, V5, by = "fecha", all = TRUE)
  #rm(V5)
  
  
  
  # CIERRE ###
  BASE <- unique(BASE)
  var_estac_model[[k]] <- BASE 
  
  rm(BASE)
}



# guardo las variables por estacion para armar modelo
 saveRDS(var_estac_model, "variables_estacion_modelo.rds")
# Corregido con imagenes nuevas RH, T, U, V, P archivos Np
# Fecha 07/07/2020 








# # # # # # # # # # # # # # # # # # # # # # 

## Unir datos del modelo con AOD + mediciones

# # # # # # # # # # # # # # # # # # # # # # 



var_estac_model <- readRDS("variables_estacion_modelo.rds")
tabla <- read.csv("tierra_buff_MODIS_1km_25.csv")


estaciones <- c("X03014006", "X03014009", "X03031002", "X03065006", "X03066003",
                "X12005005", "X12009007", "X12028001", "X12032001", "X12040008", 
                "X12040010", "X12093004", "X12099001", "X12129001", "X12140002", 
                "X12141002", "X46010001", "X46077006", "X46095001", "X46102002", 
                "X46220010", "X46250030", "X46250046", "X46250048", "X46258001")


var_estac_model_aod <- list()

for( k in 1: length(estaciones)){
  BASE <- var_estac_model[[k]]
  AOD_tierra <- tabla
  
  AOD_tierra  <- AOD_tierra[ which(AOD_tierra$estacion == estaciones[k]),]
  
  if(nrow(AOD_tierra ) != 0 ){
    AOD_tierra <- AOD_tierra[, c(1, 2, 4, 5, 6)]
    AOD_tierra$date <- as.Date(AOD_tierra$date, "%Y-%m-%d")
    names(AOD_tierra)[5] <- "fecha"
    
    AOD_tierra$file <- as.character(AOD_tierra$file)
    AOD_tierra$file <- substring(AOD_tierra$file, 18, nchar(AOD_tierra$file)) 
    
    
    pollutant <- levels(AOD_tierra$pollutant)
    AOD_tierra$pollutant <- as.character(AOD_tierra$pollutant)
    
    tabla_aod <- AOD_tierra[,c(3,4,5)]
    BASE <- merge(BASE, tabla_aod, by = "fecha")
    
    
    for(j in 1:length(pollutant)){  
      tabla_pol <- AOD_tierra[which(AOD_tierra$pollutant == pollutant[j]),]
      if(nrow(tabla_pol) != 0 ){
        names(tabla_pol)[2] <- pollutant[j]
        tabla_pol  <- tabla_pol[,c(2,4,5)]
        BASE <- merge(BASE, tabla_pol, by = c("fecha", "file"))
      }
    }
    
    BASE <- unique(BASE)
    var_estac_model_aod[[k]] <- BASE
    
    
    rm(BASE)
    
  }else{
    print(paste("Atenti!", estaciones[k], "no hay datos"))
  }

}



# Guardo las variables por estaciones con AOD para armar modelo
# saveRDS(var_estac_model_aod, "variables_estacion_aod_modelo.rds")
# Ejecutado 06/03/2020


