####### Calidad del Aire en Comunitat Valenciana
####### 29/01/2019 Valencia, Spain
####### Sol Represa
####### Archivo 3 bis


# Objetivo: 
# - Extraer info de las imagenes satelitales en las estaciones de monitoreo
# (este script utiliza las 25 estaciones definitivas)
# - Extraer datos de .csv monitoreo continuo y juntar con MODIS


#Libreria q se utilizan aqui
library(raster) 
library(maptools)


library(sp)
library(maps)
library(mapdata)
library(rgdal)
#library(gpclib)
#library(spatstat)
library(RGtk2)
#library(MODIStsp)

library(gdalUtils) 
library(foreign) #read dbf
#library(MODIS)
library(ggplot2)
library(ggmap)

library(lubridate)
library(rgeos)  #buffer de AERONET
library(reshape2)
library(dplyr)

library(lmtest)
library(foreign)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# 1- Iterativo obtener datos MODIS en sitios monitoreo terrestre (SIN BUFFER) ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# PRESTAR ATENCION A CARPETA DE SATELITE!!!!

## 5.1 Sin utilizar buffer en estaciones

id <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\quality", pattern = ".tif") # atencion q no haya .tif.xml en la carpeta..


## Extraer info MODIS para sitios de monitoreo #

sitios <- read.csv("estaciones_ut_conts.csv", sep = ";", dec = ".")
sitios <- data.frame(sitios$Latitud, sitios$Longitud, sitios$Estacion)
names(sitios) <- c("Latitud", "Longitud", "Codigo")

sitios <- sitios[order(sitios$Codigo),]

sit <- data.frame(sitios$Longitud, sitios$Latitud)


MODIS_point <- data.frame()
for (i in 1:length(id)){
  MODIS <- raster(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\quality\\", id[i], sep = "")) # abrir geotiff como raster
  cod_sat <- substring(id[i], 10, 23) #tomar dato de la fecha del nombre
  MODIS_ext <- raster::extract(MODIS, sit)
  MODIS_ext <- t(MODIS_ext)
  MODIS_ext <- as.data.frame(MODIS_ext)
  MODIS_ext$cod_sat  <- cod_sat #armar data frame con: fecha + datos en aeronet
  MODIS_point <- rbind(MODIS_point, MODIS_ext)
} 


MODIS_point$date <- substring(MODIS_point[,length(MODIS_point)], 1, 7)
MODIS_point$date <- strptime(MODIS_point$date, tz= "GMT", format = "%Y%j")

MODIS_point <- data.frame(MODIS_point[,length(MODIS_point)], MODIS_point[,(length(MODIS_point)-1)], MODIS_point[,1:(length(MODIS_point)-2)])

sitios_names <- t(sitios$Codigo)
names(MODIS_point) <- c("date", "cod_sat", as.character(sitios_names))




# # # #  ATENCION ACA < < < < < < < < < 


# MODIS calibracion (15 min) 
for (i in 3:length(MODIS_point)){
  MODIS_point[i] <- (0.84927256 *(MODIS_point[i])) + 0.01178339  #calibracion 
}


# No PISAR
#write.csv(MODIS_point, file="MODIS_1km_est_25.csv", row.names = FALSE)




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# 2 - Iterativo obtener datos MODIS en sitios monitoreo terrestre (CON BUFFER) ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


## 1) Extraer info de MODIS en estaciones utilizando buffer  

sitios <- readShapePoly("C://Users//narep//Desktop//SOL//aire_comunitat//mapa//buffer_est_ut.shp") ## Carga buffer de puntos para extraer la info
proj4string(sitios) <- CRS("+proj=longlat +ellps=WGS84 +no_defs") # Asignar proyeccion
slot(sitios, "data") <- data.frame(Codigo = sitios@data$Cod) # Asigno variables de interes (o quito el resto)


id <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\quality", pattern = ".tif") # atencion q no haya .tif.xml en la carpeta..

# Iterativo para extraer datos con el buffer en estaciones

MODIS_point <- data.frame()
for (i in 1:length(id)){
  MODIS <- raster(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\quality\\", id[i], sep = "")) # abrir geotiff como raster
  File <- substring(id[i], 1, 23) # guardar nombre del File
  means <- raster::extract(MODIS, sitios, cellnumbers=TRUE, fun=mean, na.rm=TRUE)
  for (j in 1:length(means)){
    if(is.null( means[[j]])){
      means[[j]] <- NA }    
  }
  MODIS_ext <- t(means)
  MODIS_ext <- as.data.frame(MODIS_ext)
  MODIS_ext$File  <- File #armar data frame con: fecha + datos en aeronet
  MODIS_point <- rbind(MODIS_point, MODIS_ext)
} 



MODIS_point$date <- substring(MODIS_point[,length(MODIS_point)], 10, 16)
MODIS_point <- data.frame(MODIS_point[,length(MODIS_point)], MODIS_point[,(length(MODIS_point)-1)], MODIS_point[,1:(length(MODIS_point)-2)])

sitios_names <- t(sitios$Codigo)
names(MODIS_point) <- c("date", "cod_sat", as.character(sitios_names))

MODIS_point$date <- strptime(MODIS_point$date, tz= "GMT", format = "%Y%j")


# MODIS calibracion (30 min) 
for (i in 3:length(MODIS_point)){
  MODIS_point[i] <- (0.84927256 *(MODIS_point[i])) + 0.01178339  #calibracion 
}


#No PISAR
#write.csv(MODIS_point, file="MODIS_1km_est_buff_25.csv", row.names = FALSE)










# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## NUEVO CODIGO  
## 3 - Más rapido: Extraer datos de .csv monitoreo continuo y juntar con MODIS ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(data.table)

# aod = .csv con mediciones en aod en estaciones
# tiene 1era fila date con la fecha
# tiene 2da fila cod_sat con el codigo de la imagen
# las estaciones llevan un X delante del numero


# Abrir fichero datos MODIS en sitio monitoreo continuo
#INPUT 1

#aod <- fread("MODIS_1km_est_buff_25.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)
aod <- fread("MODIS_1km_est_25.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)

aod$date <- as.IDate(aod$date, tz="GMT")

# arreglo para base 
# ya corregido en el codigo
names(aod)[2] <- "File"

#aod$File <- paste("MCD19A2.A", aod$File, sep="")

#INPUT 2
pollution <- c("PM2.5", "PM10", "Temp.", "NOx", "NO2", "NO", "H.Rel.")  #como figuran en la carpeta (indice k)
#pollution <- c("PM2.5", "PM10")  #como figuran en la carpeta (indice k)

dire <- "C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\datos\\"  #directorio donde estan los archivos


#INPUT 3
orbitas <- fread("orbitas_R.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)


#j = 3
#k = 1
#i= 2907
#aod <- aod[i:(i+20),1:5]
#orbitas <- orbitas[i:(i+20),]



# # # # # # UNIR DATA.FRAMES de estaciones >> PARTE lenta

base <- list()

for (j in 3:length(aod)){   # j son las estaciones
  base_estaciones <- data.table(date= "2009-01-01 00:00:00")  
  estacion <- names(aod)[j]
  for (k in pollution){    # k son los contaminantes
    if(file.exists(paste(dire, k, "_", substr(estacion, 2, 9), ".csv", sep = ""))){  #busco si existe ese fichero
      pollutant <- fread(paste(dire, k, "_", substr(estacion, 2, 9), ".csv", sep = ""),   #si existe lo abro
                         header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)
      pollutant <- pollutant[!duplicated(pollutant),]       # IMPORTANTE! elimino duplicados de la base de datos
      pollutant <- pollutant[complete.cases(pollutant),]
      base_estaciones <- merge(base_estaciones, pollutant, all=TRUE) 
    }
  }
  base_estaciones$DIA <- as.Date(base_estaciones$date)
  base_estaciones$HORA <- lubridate::hour(base_estaciones$date)
  base[[j-2]]<- base_estaciones 
}

# # # # # # # # # # # # # # # # # # # #


pollutants <- data.table()    # <---- variable que acumula datos para todas estaciones

# BUSCO para cada FILE
for ( i in 1:nrow(aod)){   # i es el File
  # Genero orb_hora con las horas que componen el File
  print(i)
  #archivo_orb <- orbitas[orbitas$File == aod$File[i], ] # buscar las orbitas // este con buffer
  archivo_orb <- orbitas[orbitas$File == paste("MCD19A2.A",aod$File[i], sep="")] #// este sin buffer
  orb_file <- strsplit(as.character(archivo_orb[,2]), "  ")[[1]]   # para extraer orbitas del string
  orb_hora <- substring(orb_file, 1, 11) #tomar dato de la fecha del nombre de las orbitas
  orb_hora  <- strptime(orb_hora, tz= "GMT", format = "%Y%j%H%M")
  
  pollutant_estaciones  <- data.table()   # <---- variable que acumula datos para todas estaciones
  
  # BUSCO para cada ESTACION
  for (j in 3:length(aod)){   # j son las estaciones
    estacion <- names(aod)[j]
    pollutant <- base[[j-2]]
    if(length(pollutant)> 3){   # <------- estructura de control por FILES vacios // ningun contaminante se mide ahi
      # BUSCO EN CADA ORBITA
      pollutant_hour <- data.table()   # <---- variable que acumula contaminante medido en hora del satelite
      for (l in 1:length(orb_hora)){   # l son las orbitas 
        
        tabla <- pollutant[pollutant$DIA == as.IDate(orb_hora[l]),]
        if( minute(orb_hora[l]) <= 30){
          mach <- tabla[tabla$HORA == hour(orb_hora[l]), ] # <---- busco que sea en la misma hora
        }else{
          mach <- tabla[tabla$HORA == hour(orb_hora[l]+1),]  # <---- busco que sea en la hora siguiente
        }
        pollutant_hour <- rbind(pollutant_hour, mach)
      }
      mean_pollutant <- pollutant_hour[,-c("date", "HORA", "DIA"),with=FALSE]
      mean_pollutant <- mean_pollutant[,lapply(.SD, mean, na.rm = TRUE)]
      data_pollutant <- data.table(pollutant= names(mean_pollutant), 
                                   mean = transpose( mean_pollutant),
                                   estacion = estacion,
                                   aod = aod[i, estacion, with=FALSE])
      names(data_pollutant) <- c("pollutant", "mean", "estacion", "aod")
      pollutant_estaciones <- rbind(pollutant_estaciones, data_pollutant)
    }
  }
  pollutant_estaciones[, "file"] <- aod$File[i]
  pollutant_estaciones[, "date"] <- aod$date[i]
  
  pollutants <- rbind(pollutants, pollutant_estaciones)
}



# NO PISAR!
# write.csv(pollutants, "tierra_buff_MODIS_1km_25.csv", row.names = FALSE)
# write.csv(pollutants, "tierra_MODIS_1km_25.csv", row.names = FALSE)

rm(list=ls(all=TRUE))




