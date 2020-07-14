## Comunidad Valenciana
## 23 de Agosto, 2019
##  Sol Represa
## Archivo 10


# Objetivo: 
# - Extraer info de las imagenes interpoladas con IDW (generadas en AC_33.R)
# en las estaciones de monitoreo
# (este script utiliza las 25 estaciones definitivas)
# - Extraer datos de .csv monitoreo continuo y juntar con MODIS
# - Correlacion
# Modelo



### EDITADO de
### Archivo 3 bis


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

id <- dir("stack/IDW/", pattern = ".tif") # atencion q no haya .tif.xml en la carpeta..


## Extraer info MODIS para sitios de monitoreo #

sitios <- read.csv("estaciones_ut_conts.csv", sep = ";", dec = ".")
sitios <- data.frame(sitios$Latitud, sitios$Longitud, sitios$Estacion)
names(sitios) <- c("Latitud", "Longitud", "Codigo")

sitios <- sitios[order(sitios$Codigo),]

sit <- data.frame(sitios$Longitud, sitios$Latitud)


MODIS_point <- data.frame()
for (i in 1:length(id)){
  MODIS <- raster(paste("stack/IDW/", id[i], sep = "")) # abrir geotiff como raster
  cod_sat <- substring(id[i], 14, 23) #tomar dato de la fecha del nombre
  MODIS_ext <- raster::extract(MODIS, sit)
  MODIS_ext <- t(MODIS_ext)
  MODIS_ext <- as.data.frame(MODIS_ext)
  MODIS_ext$date  <- cod_sat #armar data frame con: fecha + datos en aeronet
  MODIS_point <- rbind(MODIS_point, MODIS_ext)
} 



MODIS_point$date <- strptime(MODIS_point$date, tz= "GMT", format = "%Y-%m-%d")

MODIS_point <- data.frame(date = MODIS_point[,length(MODIS_point)], 
                          MODIS_point[,1:(length(MODIS_point)-1)])

sitios_names <- t(sitios$Codigo)
names(MODIS_point) <- c("date", as.character(sitios_names))




# # # #  ATENCION ACA < < < < < < < < < 


# MODIS calibracion (15 min) 
for (i in 2:length(MODIS_point)){
  MODIS_point[i] <- (0.84927256 *(MODIS_point[i])) + 0.01178339  #calibracion 
}


# No PISAR
# write.csv(MODIS_point, file="MODIS_1km_IDW_est_25.csv", row.names = FALSE)




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# 2 - Iterativo obtener datos MODIS en sitios monitoreo terrestre (CON BUFFER) ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


## 1) Extraer info de MODIS en estaciones utilizando buffer  

sitios <- readShapePoly("mapa//buffer_est_ut.shp") ## Carga buffer de puntos para extraer la info
proj4string(sitios) <- CRS("+proj=longlat +ellps=WGS84 +no_defs") # Asignar proyeccion
slot(sitios, "data") <- data.frame(Codigo = sitios@data$Cod) # Asigno variables de interes (o quito el resto)


id <- dir("stack/IDW", pattern = ".tif") # atencion q no haya .tif.xml en la carpeta..

# Iterativo para extraer datos con el buffer en estaciones

MODIS_point <- data.frame()
for (i in 1:length(id)){
  MODIS <- raster(paste("stack/IDW/", id[i], sep = "")) # abrir geotiff como raster
  cod_sat <- substring(id[i], 14, 23) # guardar fecha
  means <- raster::extract(MODIS, sitios, cellnumbers=TRUE, fun=mean, na.rm=TRUE)
  for (j in 1:length(means)){
    if(is.null( means[[j]])){
      means[[j]] <- NA }    
  }
  MODIS_ext <- t(means)
  MODIS_ext <- as.data.frame(MODIS_ext)
  MODIS_ext$date  <- cod_sat #armar data frame con: fecha + datos en aeronet
  MODIS_point <- rbind(MODIS_point, MODIS_ext)
} 


MODIS_point$date <- strptime(MODIS_point$date, tz= "GMT", format = "%Y-%m-%d")

MODIS_point <- data.frame(date = MODIS_point[,length(MODIS_point)], 
                          MODIS_point[,1:(length(MODIS_point)-1)])

sitios_names <- t(sitios$Codigo)
names(MODIS_point) <- c("date", as.character(sitios_names))



# MODIS calibracion (15 min) 
for (i in 2:length(MODIS_point)){
  MODIS_point[i] <- (0.84927256 *(MODIS_point[i])) + 0.01178339  #calibracion 
}


#No PISAR
#write.csv(MODIS_point, file="MODIS_1km_IDW_est_buff_25.csv", row.names = FALSE)










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

#aod <- fread("MODIS_1km_IDW_est_buff_25.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)
aod <- fread("MODIS_1km_IDW_est_25.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)

aod$date <- as.IDate(aod$date, tz="GMT")


#INPUT 2
pollution <- c("PM2.5", "PM10", "Temp.", "NOx", "NO2", "NO", "H.Rel.")  #como figuran en la carpeta (indice k)
#pollution <- c("PM2.5", "PM10")  #como figuran en la carpeta (indice k)

dire <- "datos/"  #directorio donde estan los archivos


#INPUT 3 <<<< NO LO USO
#orbitas <- fread("orbitas_R.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)


#j = 3
#k = 1
#i= 2907
#aod <- aod[i:(i+20),1:5]
#orbitas <- orbitas[i:(i+20),]



# # # # # # UNIR DATA.FRAMES de estaciones >> PARTE lenta

base <- list()

for (j in 2:length(aod)){   # j son las estaciones
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
  base[[j-1]]<- base_estaciones 
}

# # # # # # # # # # # # # # # # # # # #


pollutants <- data.table()    # <---- variable que acumula datos para todas estaciones

# BUSCO para cada FILE
for ( i in 1:nrow(aod)){   # i es el dia
  print(i)
  pollutant_estaciones  <- data.table()   # <---- variable que acumula datos para todas estaciones
  
  # BUSCO para cada ESTACION
  for (j in 2:length(aod)){   # j son las estaciones
    estacion <- names(aod)[j]
    pollutant <- base[[j-1]]
    if(length(pollutant)> 3){   # <------- estructura de control por FILES vacios // ningun contaminante se mide ahi
      # BUSCO EN CADA ORBITA
      pollutant_hour <- data.frame()   # <---- variable que acumula contaminante medido en hora del satelite
      tabla <- pollutant[which(pollutant$DIA == aod$date[i]),] # <- q coincida el DIA
      pollutant_hour <- tabla[which(tabla$HORA >= 10 & tabla$HORA <= 13 )] # <- q coincida las horas

      mean_pollutant <- pollutant_hour[,-c("date", "HORA", "DIA"), with=FALSE]
      mean_pollutant <- mean_pollutant[,lapply(.SD, mean, na.rm = TRUE)]
      data_pollutant <- data.table(pollutant= names(mean_pollutant), 
                                   mean = transpose(mean_pollutant),
                                   estacion = estacion,
                                   aod = aod[i, estacion, with=FALSE])
      names(data_pollutant) <- c("pollutant", "mean", "estacion", "aod")
      pollutant_estaciones <- rbind(pollutant_estaciones, data_pollutant)
      rm(tabla)
    }
  }
  pollutant_estaciones[, "date"] <- aod$date[i]
  pollutants <- rbind(pollutants, pollutant_estaciones)
}



# NO PISAR!
# write.csv(pollutants, "tierra_MODIS_1km_IDW_est_buff_25.csv", row.names = FALSE)
# write.csv(pollutants, "tierra_MODIS_1km_IDW_est_25.csv", row.names = FALSE)


rm(list=ls(all=TRUE))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# 4 - Análisis de tendencia y correlación

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Copiado de Archivo 7
# Se parte de las bases creadas en AC_3_bis.R
# "tierra_MODIS_1km_IDW_est_buff_25.csv"
# "tierra_MODIS_1km_IDW_est_25.csv"


#tabla <- read.table("tierra_MODIS_1km_IDW_est_buff_25.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)
tabla <- read.table("tierra_MODIS_1km_IDW_est_25.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)

tabla$date <- as.Date(tabla$date)
tabla$pollutant <- factor(tabla$pollutant)
tabla$estacion <- factor(tabla$estacion)


tabla_PM25 <- tabla %>% 
  filter(pollutant == "PM2.5") %>% 
  filter(estacion == "X03014009" | estacion == "X12005005" |
           estacion == "X03066003" | estacion == "X12009007" |
           estacion == "X12028001" | estacion == "X12040008" |
           estacion == "X12140002" | estacion == "X12141002" |
           estacion == "X46010001" | estacion == "X46077006" | 
           estacion == "X46095001" | # estacion == "X46220010" | # esta estacion da vacia
           estacion == "X46250046" | estacion == "X46250048" |
           estacion ==  "X46258001" )


tabla_PM10 <- tabla %>% 
  filter( pollutant == "PM10") %>%
  filter( estacion == "X03014009" | estacion == "X03066003" | estacion == "X12009007" | 
            estacion == "X12028001" | estacion == "X12040008" | estacion == "X12140002" | 
            estacion == "X12141002" | estacion == "X46077006" | estacion == "X46095001" | 
            # estacion == "X46220010" | # esta estacion da vacia
            estacion == "X46250030" | estacion == "X46250046" |
            estacion == "X46250048" | estacion ==  "X46258001")


# Nombres de estaciones
sitios <- read.csv("estaciones_utiles.csv")
sitios <- data.frame( Estacion = levels(sitios$Estacion), 
                      Abrev = c("Pla", "Raba", "Beni",  "Elx", "Elda", "Alcora",  "Alma", "Ben", "Bur", "Peny", "Grau",
                                "Pobla", "SJ", "VCid", "Viver", "Zorita", "Alba", "Buniol", "Caude", "Quart", "Sagunt",
                                "Pista", "Poli", "Moli", "Villar"))

levels(tabla_PM10$estacion) <- sitios[which(sitios$Estacion %in% levels(tabla_PM10$estacion)), 2]
levels(tabla_PM25$estacion) <- sitios[which(sitios$Estacion %in% levels(tabla_PM25$estacion)), 2]



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## 4.1 - Correlacionar datos de monitoreo continuo y MODIS ####
##     Todos los datos

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#tabla <- tabla_PM25
tabla <- tabla_PM10

cor.test(tabla$aod,tabla$mean, method = "spearman") # 
cor.test(tabla$aod,tabla$mean, method = "pearson") # 


## SON MUY BAJOS los r y rho
## NO CONVIENE HACER IDW en este paso.


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## 4.2 - Plot de correlacion    ####


## Plot de correlacion por estacion
# Calculate correlation for each group
cors_PM25 <-tabla_PM25[complete.cases(tabla_PM25),] %>% 
  group_by(estacion) %>%
  summarize(pearson = round(cor.test(mean, aod)$estimate, 2),
            pValue = cor.test(mean, aod)$p.value,
            spearman = round(cor.test(mean, aod, method = "spearman")$estimate, 2),
            pValue_s = cor.test(mean, aod, method = "spearman")$p.value)

cors_PM25[which(cors_PM25$pValue < 0.05), 3] <- "*"
cors_PM25[which(cors_PM25$pValue_s < 0.05), 5] <- "*"


cors_PM10 <-tabla_PM10[complete.cases(tabla_PM10),] %>% 
  group_by(estacion) %>%
  summarize(pearson = round(cor.test(mean, aod)$estimate, 2),
            pValue = cor.test(mean, aod)$p.value,
            spearman = round(cor.test(mean, aod, method = "spearman")$estimate, 2),
            pValue_s = cor.test(mean, aod, method = "spearman")$p.value)

cors_PM10[which(cors_PM10$pValue < 0.05), 3] <- "*"
cors_PM10[which(cors_PM10$pValue > 0.05), 3] <- ""

cors_PM10[which(cors_PM10$pValue_s < 0.05), 5] <- "*"
cors_PM10[which(cors_PM10$pValue_s > 0.05), 5] <- ""


## PM2.5

png("correlacion_PM25-AOD.png", width = 500, height = 500) 
tabla_PM25 %>%
  ggplot(aes( x = mean, y = aod)) + 
  geom_point(  na.rm = TRUE,  fill = "#999999") +
  #geom_abline(aes(intercept=0, slope=1 ), col = "#999999", alpha = 0.6) +
  geom_smooth(method = "lm", 
              na.rm = TRUE, 
              se = FALSE, col = "#0072B2") + 
  facet_wrap( ~estacion, ncol =  4 ) + 
  labs( x = expression(paste("PM"[2.5]," (" , mu,"g.m"^-3, ")")), 
        y = "MODIS 550 nm AOD",
        title = "") +
  theme_bw() +
  theme(axis.title = element_text(size = rel(0.8)),
        axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1)) +
  geom_text(data = cors_PM25, 
            aes(label = paste("r = ", pearson, pValue, sep="")), x= 120, y = 0.65) +
  geom_text(data = cors_PM25, 
            aes(label = paste("rho = ", spearman, pValue_s, sep="")), x= 120, y=0.45)

dev.off()


## PM10
png("correlacion_PM10-AOD.png", width = 500, height = 500) 
tabla_PM10 %>%
  ggplot(aes( x = mean, y = aod)) + 
  geom_point(  na.rm = TRUE,  fill = "#999999") +
  #geom_abline(aes(intercept=0, slope=1 ), col = "#999999", alpha = 0.6) +
  geom_smooth(method = "lm", na.rm = TRUE, se = FALSE, col = "#0072B2") + 
  facet_wrap( ~estacion, ncol =  4 ) +   #scales = "free 
  labs( x = expression(paste("PM"[10]," (" , mu,"g.m"^-3, ")")), 
        y = "MODIS 550 nm AOD",
        title = "") +
  theme_bw() +
  theme(axis.title = element_text(size = rel(0.8)),
        axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1)) +
  geom_text(data = cors_PM10, 
            aes(label = paste("r = ", pearson, pValue, sep="")), x= 280, y=0.6) +
  geom_text(data = cors_PM10, 
            aes(label = paste("rho = ", spearman, pValue_s, sep="")), x= 280, y=0.4)


dev.off()




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## 5 - Analisis de correlacion por estacion de monitoreo ####
##     PM2.5

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

tabla <- tabla_PM25

estaciones <- levels(tabla$estacion)
salida <- data.frame()

for( i in 1:length(estaciones)){
  a <- tabla %>%
    filter( estacion == estaciones[i])
  cor <- data.frame(estacion = estaciones[i],
                    cor_kendall = round(as.numeric(cor.test(a$aod,  a$mean, method = "kendall")$estimate), 2),
                    p_kendall = round(as.numeric(cor.test(a$aod,  a$mean, method = "kendall")$p.value), 4) ,
                    cor_spearman = round(as.numeric(cor.test(a$aod, a$mean, method = "spearman")$estimate), 2),
                    p_spearman = round(as.numeric(cor.test(a$aod, a$mean, method = "spearman")$p.value), 4) ,
                    cor_pearson = round(as.numeric(cor.test(a$aod, a$mean, method = "pearson")$estimate), 2),
                    p_cor = round(as.numeric(cor.test(a$aod, a$mean, method = "pearson")$p.value), 4),
                    N = as.numeric(cor.test(a$aod, a$mean, method = "pearson")$parameter) + 2)
  salida <- rbind(salida, cor)
  
}



#write.csv(salida, "correlacion_estacion.csv", row.names = FALSE)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## 6 - Modelo lineal AOD - PM2.5 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


reg <- lm( tabla_PM25$mean ~ tabla_PM25$aod )  
summary(reg) #R2 =  0.1136
plot(reg)

reg_resid <- reg$residuals
shapiro.test(sample(reg_resid, size = 5000)) # si p < 0.05 entonces los residuos no son normales
hist(reg_resid, breaks = 50) 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## 7) Unir datos del modelo con AOD + mediciones

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# COPIADO DE AC_24.R

var_estac_model <- readRDS("variables_estacion_modelo.rds")
tabla <- read.csv("tierra_MODIS_1km_IDW_est_25.csv")

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
    #AOD_tierra <- AOD_tierra[, c(1, 2, 4, 5, 6)]
    AOD_tierra$date <- as.Date(AOD_tierra$date, "%Y-%m-%d")
    names(AOD_tierra)[5] <- "fecha"
    
    #AOD_tierra$file <- as.character(AOD_tierra$file)
    #AOD_tierra$file <- substring(AOD_tierra$file, 18, nchar(AOD_tierra$file)) 
    
    
    pollutant <- levels(AOD_tierra$pollutant)
    AOD_tierra$pollutant <- as.character(AOD_tierra$pollutant)
    
    tabla_aod <- AOD_tierra[, c(4,5)]  #estacion, aod, fecha
    BASE <- merge(BASE, tabla_aod, by = "fecha")
    
    
    for(j in 1:length(pollutant)){  
      tabla_pol <- AOD_tierra[which(AOD_tierra$pollutant == pollutant[j]),]
      if(nrow(tabla_pol) != 0 ){
        names(tabla_pol)[2] <- pollutant[j]
        tabla_pol  <- tabla_pol[,c(2,5)]
        BASE <- merge(BASE, tabla_pol, by = c("fecha"))
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
# saveRDS(var_estac_model_aod, "variables_estacion_aod_IDW_modelo.rds")
# Ejecutado 07/07/2020



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## 8) Modelo con datassshhh

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# Copiado de AC_26_27.R



library(lubridate)
library(caret)
library(chron)
library(dplyr)
library(ggplot2)
library(nlme)
library(MuMIn)
library(corrplot)
library(colorRamps)



data <- readRDS("variables_estacion_aod_IDW_modelo.rds")

tabla <- data[[1]]   #11532 por estacion
tabla <- data.frame(tabla[3:33], tabla$PM2.5)
names(tabla)[32] <- "PM25"

for(i in 2:length(data)){ 
  datos <- data[[i]]
  if(length(datos$PM2.5) != 0){
    datos <- data.frame(datos[3:33], datos$PM2.5)
    names(datos)[32] <- "PM25"
    tabla <- rbind(tabla, datos)
  }
}

tabla <- tabla[,c(32, 1:31)]
tabla <- tabla[, -8] # sacamos NDVI porq una estcion entera no tiene mediciones :/
tabla <- tabla[, -c(6, 7)] # sacamos CLC_3 y CLC_4

### Analisis de correlacion
corr <- cor(tabla[,  c(1:length(tabla))], method = "pearson", use = "complete.obs")
p.mat <- cor.mtest(tabla[, c(1:length(tabla))])
corrplot(corr, 
         method = "color",
         type = "lower",
         #insig = "p-value", 
         sig.level = 0.01, 
         diag = FALSE, 
         p.mat = round(p.mat$p, 3),
         tl.cex = 0.5)

tabla <- tabla[, -c(4, 5, 11, 12, 13, 18, 22, 24, 25)] 

# CLC 2, 4 y 5
# "DUSMASS25" correlacionan
# BCSMASS 
# "DMSSMASS" - "SSSMASS25"
# "SPEEDMAX"  "USTAR"  correlacionan
# Albedo correlaciona fuerte con varias


#tabla$DEM <- log(tabla$DEM)
tabla[which(tabla$PM25 == 0), 1] <- 0.08  # LD/3


tabla <- tabla[complete.cases(tabla),]
#write.csv(tabla, "variables_estacion_aod_IDW_modelo.csv", row.names = FALSE)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 6. Generar set de prueba  #####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 


#con datos altos
base <- read.csv("/home/usuario/Sol/aire_comunitat/variables_estacion_aod_IDW_modelo.csv")


set.seed(123) #seteamos para obtener resultados reproducibles

i_entrena <- createDataPartition(y = base$PM25, 
                                 p = 0.8, 
                                 list = FALSE)
entrena <- base[i_entrena,]
test <- base[-i_entrena,]


rm(i_entrena)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Linear Regression model

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

set.seed(111)
model_lm <- caret::train(PM25 ~ . , 
                         data = entrena,
                         method = "lm",
                         preProcess= c("center", "scale"))

#RMSE      Rsquared   MAE     
#6.978945  0.1892698  4.600862

pm25_predic <- predict(model_lm, test)
postResample(pm25_predic , test$PM25)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

set.seed(123)

# Random forest con 10 fold cv - library(caret) ####

train.control <- trainControl(method = "cv", #"repeatedcv", 
                              number = 10,
                              # repeats = 10,
                              search = "random") # Set up repeated k-fold cross-validation

model_rf <- train(PM25 ~ ., 
                  data = entrena,
                  method = 'rf', 
                  trControl = train.control,
                  #tuneGrid = data.frame(mtry = 1:34),
                  ntree = 100,
                  importance = TRUE)


#>>> GUARDAR
#saveRDS(model_rf, file = "modelo_rf_100tree_IDW.rds")

model_rf <- readRDS("modelo_rf_1000tree_IDW.rds")  #1000 arboles - 10cv

# # # Varios modelos evaluados # # #

# (ntree = 100) cv simple 10-fold >> aod, 2do
# mtry  RMSE      Rsquared   MAE      
#  8    2.400073  0.9030933  0.8023690


# (ntree = 1000) Cross-Validated (10 fold) >> aod, 2do
# mtry  RMSE      Rsquared  MAE      
# 8    2.381575  0.904636  0.7879371


plot(varImp(model_rf))
plot(model_rf)


pred_rf <- predict(model_rf, test)

my_data <-  as.data.frame(cbind(predicted = pred_rf,
                                observed = test$PM25,
                                Error = abs(pred_rf - test$PM25)))

ggplot(my_data, aes(predicted, observed, colour = Error)) +
  geom_point(alpha = 0.8) + 
 # geom_smooth(method = lm, fullrange= TRUE, col = "gray38")+ 
  ggtitle("RF: ntree = 1000, mtry = 8") +
  xlab(expression( paste(PM[2.5],  " modelado"))) +
  ylab(expression( paste(PM[2.5],  " observado"))) +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, hjust=.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) + 
  theme_bw() +
  coord_fixed() +
  geom_abline(slope = 1, intercept = 0) +
  scale_colour_gradientn(colours = matlab.like(10)) +
  xlim(0,200)+
  ylim(0, 200)



RMSE = function(m, o){   # m = model, o = observed
  sqrt(mean((m - o)^2))
}

MAE <- function(m, o) { mean(abs(m - o)) }


RMSE(my_data$predicted, my_data$observed) #  2.39 (ntree = 1000) //2.3 (ntree = 100)
MAE(my_data$predicted, my_data$observed) #  0.76 (ntree = 1000) // 0.74 (ntree = 100)


datos_rf <- data.frame(ID = row.names(test), PM25_modelado = pred_rf)
#write.csv(datos_rf, file = "modelo_rf_1000_IDW.csv", row.names = F)


postResample(my_data$predicted, my_data$observed)

# MAPE #
my_data$eror_prom <- abs(my_data$observed - my_data$predicted)/my_data$observed
sum(my_data$eror_prom)/nrow(my_data)*100 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 7 - Generar salidas del modelo ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 


library(raster)
library(rgdal)



dir_merra = "/media/usuario/Elements SE/MERRA/raster_res/"


modelo_rf <- readRDS("modelo_rf_1000tree_IDW.rds")

date <- seq(from = as.Date("2008-01-01"), to = as.Date("2018-09-30"), by = "day")

variables <-c( "PS", "RH", "T", "U", "V", "DUSMASS", "OCSMASS", 
               "SO2SMASS", "SO4SMASS", "SSSMASS", "PBLH", "PRECTOT", "SPEED", 
                "CLDHGH", "CLDLOW", "H1000" )

PM25 ~ DEM + CLC_1 + PS + RH + T + U + V + DUSMASS + OCSMASS + 
  SO2SMASS + SO4SMASS + SSSMASS + PBLH + PRECTOT + SPEED + 
  CLDHGH + CLDLOW + H1000 + aod

i=1
j=1

while(i < length(date)){
  
  # busco file AOD para ese dia
  aod <- dir("stack/IDW", 
             pattern = paste( as.character(date[i]),".tif" , sep = ""),
             full.names = TRUE)      
  
  #busco files MERRA para ese dia
  for( k in 1:length(variables)){
    file <- dir(dir_merra, 
                pattern = paste("\\.", format(as.Date(date[i], "%Y%j"), "%Y%m%d"),".", 
                                variables[k], ".tif$", sep = ""),
                full.names = TRUE)
    
    if(length(file) != 0){  # Si no está vacio el archivo continuar
      assign(variables[k], file)
    }
  }
  
  # <<<  Variables unicas se incorporan ACA
  DEM <- "variables/DEM/DEM.tif"
  CLC_1 <- "variables/CLC/CLC_1.tif" 

  # Compruebo si tengo todas con un tryCatch:
  tryCatch({
    # Esto es lo q quiero hacer:
    
    l <- list(DEM, CLC_1, PS, RH, T, U, V, DUSMASS, OCSMASS, 
                SO2SMASS, SO4SMASS, SSSMASS, PBLH, PRECTOT, SPEED, 
                CLDHGH, CLDLOW, H1000, aod)
    archivo <- brick(l)
    names(archivo) <- c("DEM", "CLC_1", "PS", "RH", "T", "U", "V", "DUSMASS", "OCSMASS", 
                        "SO2SMASS", "SO4SMASS", "SSSMASS", "PBLH", "PRECTOT", "SPEED", 
                        "CLDHGH", "CLDLOW", "H1000", "aod")
    
    raster_salida <- predict(archivo, 
                             modelo_rf, 
                             type = "raw", 
                             #progress = "text",
                             predict.all = FALSE,
                             na.rm =TRUE,  ## Agregue estas 2 condiciones nuevas
                             inf.rm = TRUE)
    
    writeRaster(raster_salida, 
                file = paste("modelo_rf_1000/salida/RF-PM25-", as.character(date[i]), ".tif", sep=""), 
                format= "GTiff", 
                overwrite = TRUE )
    
  }, 
  #TryCatch: Si no tengo todos los datos de MERRA o AOD envio mensaje
  error = function(mensaje_error){
    print(paste(date[i], "sin datos MERRA"))},
  
  #TryCatch: Es necesario pasar a la siguiente fecha de AOD
  finally = {
    i <- i + 1   
  })  
  
  i <- i + 1
  print(i)
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 8 - Stack  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 


library(raster)



# 1 - Stack por year   ####


dir = "modelo_rf_1000/salida/"
dir_slack = "modelo_rf_1000/year/"

fs <- list.files(path = dir, 
                 pattern = "tif", full.names = TRUE)


k <- 1
l <- list()

for (j in 2008:2017){
  for (i in 1:length(fs)){
    year <- as.numeric(substring(fs[i], 32, 35))
    if (year == j){
      l[[k]] <- fs[i]
      k <- k + 1
    }
  }
  s <- raster::brick(l)
  
  # min_s <- min(s, na.rm=TRUE)  
  # max_s <- max(s, na.rm=TRUE)
  mean_s <- calc(s, fun = mean, na.rm=TRUE)
  #median_s <- calc(s, fun = median, na.rm = TRUE)
  sd_s <- calc(s, fun=sd, na.rm=TRUE)
  cv_s <- sd_s/mean_s
  
  #fun <- function(x) { sum(!is.na(x)) }
  #n_s <- calc(s, fun = fun )
  
  #writeRaster(min_s, filename = paste(dir_slack, "PM10_year_", j, "_min.tif", sep=""))
  #writeRaster(max_s, filename = paste(dir_slack, "PM10_year_", j, "_max.tif", sep=""))
  writeRaster(mean_s, 
           filename = paste(dir_slack, "PM25_year_", j, "_mean.tif", sep = ""),  
            overwrite = TRUE,
           format= "GTiff")
  #writeRaster(median_s, filename = paste(dir_slack, "PM25_year_", j, "_median.tif", sep=""))
  writeRaster(sd_s, filename = paste(dir_slack, "PM25_year_", j, "_sd.tif", sep=""),
              overwrite = TRUE, format= "GTiff")
  #writeRaster(n_s, filename = paste(dir_slack, "PM25_year_", j, "_n.tif", sep=""))
  writeRaster(cv_s, filename = paste(dir_slack, "PM25_year_", j, "_cv.tif", sep=""),
              overwrite = TRUE, format= "GTiff")
  
  print(j)
  rm(s, min_s, max_s, sd_s, cv_s)
}





#### 2 - Stack por year - mes   ####


dir = "modelo_rf_1000/salida/"
dir_slack = "modelo_rf_1000/year-mes/"

fs <- list.files(path = dir, 
                 pattern = "tif", full.names = TRUE)


k <- 1
l <- list()

for (j in 2008:2017){  #yars
  for(m in 1:12){  #meses
    
    for (i in 1:length(fs)){
      year <- as.numeric(substring(fs[i], 32, 35))
      mes <- month(as.Date( substring(fs[i], 32, 41), format = "%Y-%m-%d"))
      if (year == j && mes == m){
        l[[k]] <- fs[i]
        k <- k + 1
      }
    }
    
    # control >> solo si l guarda algun elemento 
    if(length(l) > 0){
      s <- raster::brick(l)
      
      min_s <- min(s, na.rm=TRUE)  
      max_s <- max(s, na.rm=TRUE)
      mean_s <- calc(s, fun = mean, na.rm=TRUE)
      #median_s <- calc(s, fun = median, na.rm = TRUE)
      sd_s <- calc(s, fun=sd, na.rm=TRUE)
      cv_s <- sd_s/mean_s
      
      fun <- function(x) { sum(!is.na(x)) }
      n_s <- calc(s, fun = fun )
      
      writeRaster(min_s, filename = paste(dir_slack, "PM25_year-mes_", j, m, "min.tif", sep=""))
      writeRaster(max_s, filename = paste(dir_slack, "PM25_year-mes_", j, m, "max.tif", sep=""))
      writeRaster(mean_s, 
                  filename = paste(dir_slack, "PM25_year-mes_", j, m, "mean.tif", sep = ""),  
                  overwrite = TRUE,
                  format= "GTiff")
      #writeRaster(median_s, filename = paste(dir_slack, "PM25_year_", j, "median.tif", sep=""))
      writeRaster(sd_s, filename = paste(dir_slack, "PM25_year-mes_", j, m, "sd.tif", sep=""))
      writeRaster(n_s, filename = paste(dir_slack, "PM25_year-mes_", j, m, "n.tif", sep=""))
      writeRaster(cv_s, filename = paste(dir_slack, "PM25_year-mes_", j, m, "cv.tif", sep=""))
      
      print(j)
      rm(s, min_s, max_s, sd_s, cv_s)
    }
    
  }
}




#### 3 - Stack por mes   ####


dir = "modelo_rf_1000/salida/"
dir_slack = "modelo_rf_1000/mes/"

fs <- list.files(path = dir, 
                 pattern = "tif", full.names = TRUE)


k <- 1
l <- list()

for(m in 1:12){  #meses
  
  for (i in 1:length(fs)){
    mes <- month(as.Date( substring(fs[i], 32, 41), format = "%Y-%m-%d"))
    if (mes == m){
      l[[k]] <- fs[i]
      k <- k + 1
    }
  }
  
  # control >> solo si l guarda algun elemento 
  if(length(l) > 0){
    s <- raster::brick(l)
    
    min_s <- min(s, na.rm=TRUE)  
    max_s <- max(s, na.rm=TRUE)
    mean_s <- calc(s, fun = mean, na.rm=TRUE)
    #median_s <- calc(s, fun = median, na.rm = TRUE)
    sd_s <- calc(s, fun=sd, na.rm=TRUE)
    cv_s <- sd_s/mean_s
    
    fun <- function(x) { sum(!is.na(x)) }
    n_s <- calc(s, fun = fun )
    
    writeRaster(min_s, filename = paste(dir_slack, "PM25_mes_", m, "_min.tif", sep=""))
    writeRaster(max_s, filename = paste(dir_slack, "PM25_mes_", m, "_max.tif", sep=""))
    writeRaster(mean_s, 
                filename = paste(dir_slack, "PM25_mes_", m, "_mean.tif", sep = ""),  
                overwrite = TRUE,
                format= "GTiff")
    #writeRaster(median_s, filename = paste(dir_slack, "PM10_mes_", m, "_median.tif", sep=""))
    writeRaster(sd_s, filename = paste(dir_slack, "PM25_mes_", m, "_sd.tif", sep=""))
    writeRaster(n_s, filename = paste(dir_slack, "PM25_mes_", m, "_n.tif", sep=""))
    writeRaster(cv_s, filename = paste(dir_slack, "PM25_mes_", m, "_cv.tif", sep=""))
    
    print(m)
    rm(s, min_s, max_s, sd_s, cv_s)
  }
  
}




