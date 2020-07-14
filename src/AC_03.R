####### Calidad del Aire en Comunitat Valenciana
####### 10/10/2018 Valencia, Spain
####### Modificado 29/01/2019
####### Sol Represa
####### Archivo 3


# Objetivo: Extraer info de las imagenes satelitales en las estaciones de monitoreo
# (este script no utiliza las estaciones seleccionadas en tesis)

library(sp)
library(maps)
library(mapdata)
library(maptools)
library(rgdal)
#library(gpclib)
#library(spatstat)
library(RGtk2)
#library(MODIStsp)

library(gdalUtils) 
library(raster) # raster()
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



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# 4- Iterativo obtener datos MODIS en sitios monitoreo terrestre (SIN BUFFER) ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# PRESTAR ATENCION A CARPETA DE SATELITE!!!!

## 5.1 Sin utilizar buffer en estaciones

id <- dir("MODIS/quality", pattern = ".tif") # atencion q no haya .tif.xml en la carpeta..


## Extraer info MODIS para sitios de monitoreo #

sitios <- read.dbf("sitios.dbf")
sitios <- data.frame(sitios$Latitud, sitios$Longitud, sitios$Cod)
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


MODIS_point$date <- substring(MODIS_point[,70], 1, 7)
MODIS_point <- data.frame(MODIS_point[,71], MODIS_point[,70], MODIS_point[,1:69])

sitios_names <- t(sitios$Codigo)
names(MODIS_point) <- c("date", "cod_sat", as.character(sitios_names))

MODIS_point$date <- strptime(MODIS_point$date, tz= "GMT", format = "%Y%j")



######### ATENCION ACA ########################################################

# MODIS calibracion (15 min) 
for (i in 3:length(MODIS_point)){
  MODIS_point[i] <- (0.84927256 *(MODIS_point[i])) + 0.01178339  #calibracion 
}


# No PISAR
# write.csv(MODIS_point, file="MODIS_1km_estaciones.csv", row.names = FALSE)




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# 5- Iterativo obtener datos MODIS en sitios monitoreo terrestre (CON BUFFER) ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## b) Extraer info de MODIS en estaciones utilizando buffer  

sitios <- readShapePoly("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\mapa\\est_buff_comunitat.shp") ## Carga buffer de puntos para extraer la info
proj4string(sitios) <- CRS("+proj=longlat +ellps=WGS84 +no_defs") # Asignar proyeccion
slot(sitios, "data") <- data.frame(Codigo = sitios@data$Cod) # Asigno variables de inter?s (o quito el resto)


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



MODIS_point$date <- substring(MODIS_point[,70], 1, 7)
MODIS_point <- data.frame(MODIS_point[,71], MODIS_point[,70], MODIS_point[,1:69])

sitios_names <- t(sitios$Codigo)
names(MODIS_point) <- c("date", "cod_sat", as.character(sitios_names))

MODIS_point$date <- strptime(MODIS_point$date, tz= "GMT", format = "%Y%j")


# MODIS calibracion (30 min) 
for (i in 3:length(MODIS_point)){
  MODIS_point[i] <- (0.84927256 *(MODIS_point[i])) + 0.01178339  #calibracion 
}


#No PISAR
#write.csv(MODIS_point, file="MODIS_1km_estaciones_buff.csv", row.names = FALSE)




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## NUEVO CODIGO  
## 6 - Más rapido: Extraer datos de .csv monitoreo continuo y juntar con MODIS ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(data.table)

# aod = .csv con mediciones en aod en estaciones
# tiene 1era fila date con la fecha
# tiene 2da fila cod_sat con el codigo de la imagen
# las estaciones llevan un X delante del numero


# Abrir fichero datos MODIS en sitio monitoreo continuo
#INPUT 1

#aod1 <- fread("MODIS_1km_estaciones_buff.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)
aod <- fread("MODIS_1km_estaciones.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)


aod$date <- as.IDate(aod$date, tz="GMT")

# arreglo para base 
# ya corregido en el codigo
names(aod)[2] <- "File"
aod$File <- paste("MCD19A2.A", aod$File, sep="")

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



############ UNIR DATA.FRAMES de estaciones >> PARTE lenta

base <- list()

for (j in 3:length(aod)){   # j son las estaciones
  base_estaciones <- data.table(date= "2009-01-01 00:00:00")  
  estacion <- names(aod)[j]
  for (k in pollution){    # k son los contaminantes
    if(file.exists(paste(dire, k, "_", estacion, ".csv", sep = ""))){  #busco si existe ese fichero
      pollutant <- fread(paste(dire, k, "_", estacion, ".csv", sep = ""),   #si existe lo abro
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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


pollutants <- data.table()    # <---- variable que acumula datos para todas estaciones

# BUSCO para cada FILE
for ( i in 1:nrow(aod)){   # i es el File
  # Genero orb_hora con las horas que componen el File
  print(i)
  archivo_orb <- orbitas[orbitas$File == aod$File[i], ] # buscar las orbitas
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
#write.csv(pollutants, "tierra_buff_MODIS_1km.csv", row.names = FALSE)

write.csv(pollutants, "tierra_MODIS_1km.csv", row.names = FALSE)

#rm(list=ls(all=TRUE))



time({
  ### poner codigo para probar velocidad
})



######### FUNCION PARA EVALUAR TIEMPO ##############
time <- function(...) {
  time_measurement <- system.time(eval(...))
  time_measurement[["user.self"]]
}
#####################################################


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## 7 - Más rapido: Extraer datos de .csv monitoreo continuo y juntar con MODIS ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(dplyr)

#tabla <- read.table("tierra_buff_MODIS_1km.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)
tabla <- read.table("tierra_MODIS_1km.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)

tabla$date <- as.Date(tabla$date)
tabla$pollutant <- factor(tabla$pollutant)

tabla$estacion <- factor(tabla$estacion, # sin los 0
                         levels = c( "3009006", "3014004", "3014006", "3014008", 
                                     "3014009", "3014010", "3014012", "3014013", "3014014", 
                                     "3031002", "3065006", "3065007", "3066003",
                                     "3089001", "3099002", "3105001", "3122005", "3133002",
                                     "12005005", "12009007", "12028001", "12032001", "12040008", 
                                     "12040009", "12040010", "12040015", "12046001",
                                     "12080007", "12084003", "12093004", "12099001", "12120001", 
                                     "12124001", "12126003", "12127002", "12129001",
                                     "12138001", "12138002", "12140002", "12141002", "46010001", 
                                     "46017002", "46028001", "46062001", "46064001",
                                     "46077006", "46078004", "46095001", "46099001", "46102002", 
                                     "46109001", "46111001", "46116001", "46131002",
                                     "46136001", "46181002", "46184002", "46190005", "46202001", 
                                     "46202002", "46202003", "46220003", "46220009",
                                     "46220010", "46242001", "46244003", "46248001", "46250030", 
                                     "46250031", "46250033", "46250034", "46250043",
                                     "46250046", "46250047", "46250048", "46250049", "46250050", 
                                     "46250051", "46256001", "46258001", "46258004",
                                     "46263999"),
                         labels = c( "Alcoi - Verge dels Lliris", "Renfe", "Alacant - El Pla",                
                                     "Alacant - Florida Babel", "Alacant - Rabassa", "Alacant  - Parc Mar Prov",           
                                     "Alacant  - AP ISM", "Alacant - AP T Frutero", "Alacant - AP D Pesquera",           
                                     "Benidorm", "Elx - Agroalimentari", "Elx - Parc de Bombers",           
                                     "Elda - Lacy", "Monóver" , "Orihuela",                        
                                     "El Pinós",  "Sant Vicent del Raspeig", "Torrevieja",                      
                                     "L´Alcora", "Almassora - C.P.Ochando", "Benicassim" ,                     
                                     "Burriana", "Castelló - Penyeta" ,"Castelló - Ermita" ,              
                                     "Castelló - Grau" , "Castelló - Patronat d´Esports","Cirat",                           
                                     "Morella", "Onda",  "Coratxar",                        
                                     "Sant Jordi" ,  "Torre Endoménech", "Vall d´Alba PM",                  
                                     "La Vall d´Uixó",   "Vallibona"  , "Vilafranca",                      
                                     "Vinaròs Planta",  "Vinaròs Plataforma" , "Viver",                           
                                     "Zorita"  , "Albalat dels Tarongers",  "Alzira",                          
                                     "Algar de Palància"," Benigànim" , "UM_Benimuslem",                   
                                     "Buñol - Cemex", "Burjassot - Facultats" , "Caudete de las Fuentes",          
                                     "Cortes de Pallás", "Quart de Poblet", "Cheste_UM",                       
                                     "Chiva_UM", "L´Eliana" , "Gandia",                          
                                     "UM_Godelleta",  "Oliva Unidad Móvil (campaña)", "Ontinyent",                       
                                     "Paterna - CEAM", "La Pobla de Vallbona - La Vereda", "La Pobla Cap Horta",              
                                     "La Pobla Maravisa", "Sagunt - Port", "Sagunt - Nord",                   
                                     "Sagunt - CEA",  "Torrebaja" , "Torrent-El Vedat",                
                                     "Turís_UM", "València - Pista de Silla" , "València - Nuevo Centro",         
                                     "Aragón", "València - Linares", "València - Vivers",               
                                     "València - Politècnic",  "València - Avd. Francia" , "València - Molí del Sol",         
                                     "València-Conselleria Meteo.",  "València - Bulevard Sud", "Valencia-Albufera",               
                                     "Vilamarxant", "Villar del Arzobispo", "Villar - Tejeria 2",                
                                     "Zarra - EMEP" ))



tabla_PM25 <- tabla %>% filter(pollutant == "PM2.5")
tabla_PM10 <- tabla %>% filter(pollutant == "PM10")
tabla_HREL <- tabla %>% filter(pollutant == "H.Rel.")


cor.test(tabla_PM25$aod,tabla_PM25$mean, method = "kendall")   #
cor.test(tabla_PM25$aod,tabla_PM25$mean, method = "pearson") # 
cor.test(tabla_PM25$aod,tabla_PM25$mean, method = "spearman") # 


cor.test(tabla_PM10$aod,tabla_PM10$mean, method = "kendall")   #
cor.test(tabla_PM10$aod,tabla_PM10$mean, method = "pearson") # 
cor.test(tabla_PM10$aod,tabla_PM10$mean, method = "spearman") # 


write.csv(tabla_PM10, "V_PM10_MODIS.csv", row.names = FALSE)
write.csv(tabla_PM25, "V_PM25_MODIS.csv", row.names = FALSE)
write.csv(tabla_HREL, "V_HREL_MODIS.csv", row.names = FALSE)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## Agregar los datos de forma mensual

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

tabla_PM25 <- fread("V_PM25_MODIS.csv", header=TRUE, sep=",", dec=".")
tabla_HREL <- fread("V_HREL_MODIS.csv", header=TRUE, sep=",", dec=".")

tabla_PM25$date <- as.Date(tabla_PM25$date)


a <- tabla_HREL
a$date <- as.Date(a$date)
data_na <- na.omit(a)
a$estacion <- factor(a$estacion)
estaciones <- levels(a$estacion)

salida <- data.frame()
for( i in estaciones){
  tabla <- data_na %>% filter(estacion == i)
  if(dim(tabla)[1] > 0){
    aod <- aggregate(tabla[4], FUN = "mean", by = list(format(tabla$date, "%Y-%m")), na.rm= TRUE, na.action= NULL)
    upper_aod <- aggregate(tabla[4], FUN = "max", by = list(format(tabla$date, "%Y-%m")), na.rm= TRUE)
    lower_aod <- aggregate(tabla[4], FUN = "min", by = list(format(tabla$date, "%Y-%m")), na.rm= TRUE)
    sd_aod <-  aggregate(tabla[4], FUN = "sd", by = list(format(tabla$date, "%Y-%m")), na.rm= TRUE)
    mean <- aggregate(tabla[2], FUN = "mean", by = list(format(tabla$date, "%Y-%m")), na.rm= TRUE, na.action= NULL)
    upper <- aggregate(tabla[2], FUN = "max", by = list(format(tabla$date, "%Y-%m")), na.rm= TRUE)
    lower <- aggregate(tabla[2], FUN = "min", by = list(format(tabla$date, "%Y-%m")), na.rm= TRUE)
    sd <-  aggregate(tabla[2], FUN = "sd", by = list(format(tabla$date, "%Y-%m")), na.rm= TRUE)
    b <- data.frame(mean[1], a$pollutant[1] , 
                    aod[2], upper_aod[2], lower_aod[2], sd_aod[2], 
                    mean[2], upper[2], lower[2], sd[2], i)
    names(b) <- c("date", "Variable", 
                  "aod", "upper_aod", "lower_aod", "sd_aod",
                  "mean", "upper", "lower", "sd", "estacion")
    salida <- rbind(salida, b)
  }
}

#write.csv(salida, "V_PM25_MODIS_mensual.csv", row.names = FALSE)
write.csv(salida, "V_HREL_MODIS_mensual.csv", row.names = FALSE)



datos <- tabla_PM25[complete.cases(tabla_PM25),]   #23492
datos <- merge(datos, tabla_HREL, by=c("date", "estacion", "file"))  #12707
