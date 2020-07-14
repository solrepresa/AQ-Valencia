####### Calidad del Aire en Comunitat Valenciana
####### 15/10/2018 Valencia, Spain
####### Sol Represa
####### Archivo 1

##### Indice 

# 1- Abrir ficheros
# 2- AERONET: Analisis de Factor Angstrom y Calculo de AOD_550
# 3 - Estaciones de monitoreo

library(sp)
library(maps)
library(mapdata)
library(maptools)
library(rgdal)
#library(gpclib)
#library(spatstat)
library(RGtk2)
library(MODIStsp)

library(gdalUtils) 
library(raster) # raster()
library(foreign) #read dbf
library(MODIS)
library(ggplot2)
library(ggmap)

library(lubridate)
library(rgeos)  #buffer de AERONET
library(reshape2)
library(dplyr)

library(lmtest)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#### 1 - Abrir tif     ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



id <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\raster", pattern = ".tif")

MODISraster <- raster(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\raster\\", id[1], sep = ""))
MODISraster <- MODISraster*0.001   #factor de escala para archivos en carpeta raster

rm(MODISraster)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

### 2 - Sitio AERONET  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

aeronet <- data.frame(-0.384395, 39.424932) #Burjassot
names(aeronet) <- c("Longitud", "Latitud")
coordinates(aeronet) <- ~Longitud+Latitud
proj4string(aeronet) <- CRS("+proj=longlat +ellps=WGS84 +no_defs")

#aeronet_sp <- spTransform(aeronet, CRS()) # Transformar proyeccion 



## MAPA aeronet ###

map <- get_map(location= c(lat=39.47, lon=-0.37), maptype="roadmap")

aeron <- data.frame(-0.38, 39.42)
names(aeron) <- c("Longitud", "Latitud")
ggmap(map) + geom_point(data=aeron, aes(x=Longitud, y=Latitud), 
                        shape=24, alpha=1, size = 5, fill= "firebrick3") +
  labs(x= "Longitud", y= "Latitud", title="Sitio AERONET")
scale_shape_manual(values=c(21,24))



### 2.1  Analisis datos AERONET ####


# fechas de datos AERONET?

data <- read.csv("Burjassot_tot.csv", header=TRUE, sep=";", dec=".", skip= 4, na.strings = "N/A", stringsAsFactors = FALSE  )
date <- paste(data$Date.dd.mm.yy, data$Time.hh.mm.ss, sep=" "  )
date <- strptime(date, format="%d/%m/%Y %H:%M:%S", tz="GMT") 


### 2.2 - Calculo AOD 550 con interpolacion cuadratica ####


##### FUNCION INTERPOLACION CUADRATICA  ##########

# Esta funcion realiza una interpolaci?n tomando 3 puntos,
# basandose en los polinomios de Lagrange.

interpol_cuad <- function(x, x0, y0, x1, y1, x2, y2){ 
  a = ((x - x1)*(x-x2))/((x0-x1)*(x0-x2))
  b = ((x - x0)*(x-x2))/((x1-x0)*(x1-x2))
  c = ((x - x0)*(x-x1))/((x2-x0)*(x2-x1))
  y = (y0*a) + (y1*b) + (y2*c)
  return(y)
}

# # # # # # # # # # # # # # # # # # # # # # # # # #

# Utilizando 675nm

x = log(550)  #incognita

x0= log(440)         #punto 1
y0= log(data$AOT_440)
x1= log(500)         #punto 2
y1=log(data$AOT_500)
x2= log(675)         #punto 3
y2= log(data$AOT_675)

y <- interpol_cuad(x, x0, y0, x1, y1, x2, y2)

y <- exp(y)

data_aeronet <- data.frame(date, y)
names(data_aeronet) <- c("date", "AOT_550")


# Utilizando 870 nm


x2= log(870)
y2= log(data$AOT_870)

data_aeronet$AOT_550_2  <- interpol_cuad(x, x0, y0, x1, y1, x2, y2)
data_aeronet$AOT_550_2  <- exp(data_aeronet$AOT_550_2)

# Utilizando 1020nm

x2= log(1020)
y2= log(data$AOT_1020)

data_aeronet$AOT_550_3 <- interpol_cuad(x, x0, y0, x1, y1, x2, y2)
data_aeronet$AOT_550_3 <- exp(data_aeronet$AOT_550_3)

#Analizar correlacion entre los AOD 550 calculados con los distintos puntos 
ggplot(data= data_aeronet, aes(x= AOT_550, y= AOT_550_2)) + geom_point(na.rm=TRUE, colour="gray48") + theme_bw() +
  labs(x= "AOT 550 (675 Angstrom) ", y="AOT 550 (870 Angstrom)", title= "Transformaci?n valores AERONET ")  + 
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0)) 

ggplot(data= data_aeronet, aes(x= AOT_550_3, y= AOT_550_2)) + geom_point(na.rm=TRUE, colour="gray48") + theme_bw() +
  labs(x= "AOT 550 (870 Angstrom) ", y="AOT 550 (1020 Angstrom)", title= "Transformaci?n valores AERONET ")  + 
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0)) 



# NO PISAR CSV!
# write.csv(data_aeronet, file="Burjassot.csv", row.names = FALSE)






# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

### 3 - Extraer datos de raster  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


sitios <- read.dbf("sitios.dbf")

sit <- data.frame(sitios$Longitud, sitios$Latitud)

extract(MODISraster, aeronet)
extract(MODISraster, sit)





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

### 4- Extraer de MODIS valores en BUFFER de Aeronet ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


aeronet <- data.frame(-0.418, 39.508)
names(aeronet) <- c("Longitud", "Latitud")
coordinates(aeronet) <- ~Longitud+Latitud
proj4string(aeronet) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")



### FUNCION para hacer BUFFER de puntos ####

custom.buffer <- function(p, r) {        # p  son coordenadas, r es la distancia en metros
  stopifnot(length(p) == 1)
  cust <- sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0",    #Azimuthal equidistant projection (AEQ) An AEQ projection centred on each point will project equal distances in all directions.
                  p@coords[[2]], p@coords[[1]])
  projected <- spTransform(p, CRS(cust))                      #transforma el punto en proyeccion AEQ
  buffered <- gBuffer(projected, width=r, byid=TRUE)          #genera el buffer
  spTransform(buffered, p@proj4string)                        #transforma el buffer a la proyeccion original del punto
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



#buffered <- custom.buffer(aeronet, 3250)       # buffer de 3.25km de radio = 7.5 de diametro  >> imagen pixel de 3km                     
buffered <- custom.buffer(aeronet, 1500)       # buffer de 1.5km de radio = 3km de diametro  >> imagen pixel de 1km  




# Grafica para ver BUFFER

buff <- fortify(buffered)
buff$transf <- "custom"

map <- get_map(location= c(lon=-0.418, lat=39.508 ), maptype="roadmap")

aeron <- data.frame(-0.38, 39.42)
names(aeron) <- c("Longitud", "Latitud")
ggmap(map) + geom_point(data=aeron, aes(x=Longitud, y=Latitud), 
                        shape=24, alpha=1, size = 5, fill= "firebrick3") +
  labs(x= "Longitud", y= "Latitud", title="Sitio AERONET") + 
  geom_path(data=buff, aes(x=long, y=lat, group=transf)) 




# Iterativo para extraer datos con el buffer sobra datos de 1km

id <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\quality", pattern = ".tif") # atencion q no haya .tif.xml en la carpeta..

aod_point_raster <- data.frame()
for (i in 1:length(id)){ 
  aod <- raster(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\quality\\", id[i], sep = "")) # abrir geotiff como raster
  date <- substring(id[i], 10, 16) #tomar dato de la fecha del nombre
  means <- raster::extract(aod, buffered, cellnumbers=TRUE, fun=mean, na.rm=TRUE)
  if(is.null( means[[1]])){
    means <- NA } 
  file <- substring(id[i], 1, 23)
  aero_dato <- data.frame( date , means, file )   #armar data frame con: fecha + datos en aeronet
  names(aero_dato) <- c("date", "aod_550", "file")
  aod_point_raster <- rbind(aero_dato, aod_point_raster)
  rm(aero_dato)
} 

aod_point_raster$date <- as.character(aod_point_raster$date)
fechas <- strptime(aod_point_raster$date, tz= "GMT", format = "%Y%j")
fechas <- as.data.frame(fechas)
aod_point_raster[1] <- fechas


#write.csv(aod_point_raster, file="MODIS_sitio_raster_aeronet.csv", row.names = FALSE)
write.csv(aod_point_raster, file="MODIS_sitio_raster_aeronet_1km.csv", row.names = FALSE)





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

### 5 - Obtener numero de órbitas ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


require("tm")

orbitas <- read.csv("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\MODIS\\orbitas.csv", 
                    header=FALSE, sep=",", dec=".", stringsAsFactors = FALSE)
orbitas <- orbitas[,-1]
names(orbitas) <- c("File", "Orbitas")
orbitas<- substring()
orbitas[,1] <- substring(orbitas[,1], 3,25)

orbitas[,2] <- gsub("\\[", "", orbitas[,2])
orbitas[,2] <- gsub("\\]", "", orbitas[,2])
orbitas[,2] <- gsub("\\'", "", orbitas[,2])

#write.csv(orbitas, file="orbitas_R.csv", row.names = FALSE)


# strsplit(orbitas[i,2], "  ")[[1]]   # para extraer orbitas del string




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

### 6 - Extraer datos de .csv AERONET "Burjassot.csv" ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# 6.1  Abrir fichero datos MODIS en sitio aeronet ####

#aod_point <- read.csv("MODIS_sitio_raster_aeronet.csv", header=TRUE, sep=",", 
#                      dec=".", stringsAsFactors = FALSE)

aod_point <- read.csv("MODIS_sitio_raster_aeronet_1km.csv", header=TRUE, sep=",", 
                      dec=".", stringsAsFactors = FALSE)


aod_point$date <- as.character(aod_point$date)
aod_point$date <- as.POSIXlt(aod_point$date, tz="GMT") 

# Abrir ficheros de orbitas

orbitas <- read.csv("orbitas_R.csv", header=TRUE, sep=",", dec=".")


# Abrir fichero AERONET y acomodar el formato
data_aeronet <- read.csv("Burjassot.csv", header=TRUE, sep=",", dec=".", na.strings = "NA", 
                         stringsAsFactors = FALSE  )
data_aeronet$date <- as.POSIXlt(data_aeronet$date, tz="GMT") 


# 6.2 Tomar valores de AERONET dentro de 30min (1800 sec) que pasa MODIS ####

# Como el producto es un promedio de las imágenes Aqua y Terra, 
# el criterio debe ser el promedio de los valores registrados por AOD en ese rango?

MODIS_aeronet <- data.frame()
AOD <- data.frame()
Crudo <- data.frame()

#i=5213
for (i in 1: nrow(aod_point)){                    ### Me devuelve la media de datos que comparten año, mes y dia
  tabla_aeronet <- data_aeronet
  eq_year <- which(year(tabla_aeronet$date) == year(aod_point[i,]$date))
  tabla_aeronet <- tabla_aeronet[eq_year,] 
  eq_month <- which(month(tabla_aeronet$date) == month(aod_point[i,]$date))
  tabla_aeronet <- tabla_aeronet[eq_month,] 
  eq_day <- which(day(tabla_aeronet$date) == day(aod_point[i,]$date))
  tabla_aeronet <- tabla_aeronet[eq_day,]
  dim_tabla <- dim(tabla_aeronet)
  if(dim_tabla[1] == 0){
    salida <- data.frame(NA,NA, NA, NA, NA, NA, NA, NA, NA, NA )                ## si no tengo datos ese dia, coloca un NA :) para evitarnos el warning
  }else{
### VER criterio -->  
    archivo_orb <- orbitas[which(orbitas$File == aod_point[i,]$file),] #coincide el file
    orb_file <- strsplit(as.character(archivo_orb[,2]), "  ")[[1]]   # para extraer orbitas del string
    orb_hora <- substring(orb_file, 1, 11) #tomar dato de la fecha del nombre
    orb_hora  <- strptime(orb_hora, tz= "GMT", format = "%Y%j%H%M")
    en_dif <- data.frame()
    for (j in 1:length(orb_hora)){
      mach <- which(tabla_aeronet$date - orb_hora[j] <= 15) # <---- busco dentro de 15 min // 30min
      mach <- data.frame(mach)
      en_dif <- rbind(en_dif, mach)
    }   
    en_dif <- unique(en_dif)
    en_dif <- as.list(en_dif)
    tabla_dif <- tabla_aeronet[en_dif$mach,]
    dim_tabla <- dim(tabla_dif)
# # # # # # # # # # # # # # # # # # # # # # # # #     
    if(dim_tabla[1] == 0){                                          ## si no tengo datos dentro de lo 30 minutos, coloca un NA
      salida <- data.frame(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA ) 
    }else{
      salida <- data.frame(mean(tabla_dif[,2]), mean(tabla_dif[,3]),
                           mean(tabla_dif[,4]), 
                           median(tabla_dif[,2]), median(tabla_dif[,3]),
                           median(tabla_dif[,4]),
                           sd(tabla_dif[,2], na.rm=TRUE), 
                           sd(tabla_dif[,3], na.rm=TRUE),
                           sd(tabla_dif[,4], na.rm=TRUE), dim_tabla[1])
      resumen <- data.frame()
      resumen <- data.frame(aod_point[i,1], aod_point[i,2], tabla_dif[,1:4])
      names(resumen) <- c("Date_MODIS", "AOD", "date_AERO", "AOT_550", "AOT_550_2", "AOT_550_3")
      Crudo <- rbind(Crudo, resumen)
      names(Crudo) <- c("Date_MODIS", "AOD", "date_AERO", "AOT_550", "AOT_550_2", "AOT_550_3")
    }
  }
  MODIS_aeronet <- data.frame(aod_point[i,1], aod_point[i,2], salida[,1:10])
  names(MODIS_aeronet) <- c("Date_MODIS", "AOD", 
                            "med_AOT_550", "med_AOT_550_870", "med_AOT_550_1020",
                            "median_AOT_550", "median_AOT_550_870", "median_AOT_550_1020",
                            "sd_AOT_550", "sd_AOT_550_870", "sd_AOT_550_1020", "N_AOT" )
  AOD <- rbind(AOD, MODIS_aeronet)
  names(AOD) <- c("Date_MODIS", "AOD", 
                  "med_AOT_550", "med_AOT_550_870", "med_AOT_550_1020",
                  "median_AOT_550", "median_AOT_550_870", "median_AOT_550_1020",
                  "sd_AOT_550", "sd_AOT_550_870", "sd_AOT_550_1020", "N_AOT" )
  
}

# La variable "Crudo" guarda todos lo valores de AERONET que son promediados

# NO PISAR BASE! 
#write.csv(AOD, "AOD_MODIS_1km_AERONET_mean_30.csv", row.names = FALSE)
#write.csv(AOD, "AOD_MODIS_1km_AERONET_mean.csv", row.names = FALSE)

#write.csv(AOD, "AOD_MODIS_1km_AERONET_1km_mean_30.csv", row.names = FALSE)
write.csv(AOD, "AOD_MODIS_1km_AERONET_1km_mean_15.csv", row.names = FALSE)


# LEER
# tabla_aeronet contiene las mediciones de AERONET para el día aod_point[i,]
# ejemplo con i=5213



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#### 5- Correlacion entre AERONET y MODIS (Calibracion) ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


#aod <- read.csv("AOD_MODIS_1km_AERONET_mean_30.csv", header=TRUE, sep=",", dec=".")
aod <- read.csv("AOD_MODIS_1km_AERONET_mean.csv", header=TRUE, sep=",", dec=".")
#aod <- read.csv("AOD_MODIS_1km_AERONET_1km_mean_30.csv", header=TRUE, sep=",", dec=".")
#aod <- read.csv("AOD_MODIS_1km_AERONET_1km_mean_15.csv", header=TRUE, sep=",", dec=".")


aod$Date_MODIS <- as.character(aod$Date_MODIS)
aod$Date_MODIS <- as.POSIXct(aod$Date_MODIS, tz="GMT") 


#grafica periodo de tiempo
ggplot(data = aod) + geom_line(aes(x = Date_MODIS, y = med_AOT_550), colour="firebrick2", na.rm=TRUE) + 
  geom_line(aes(x = Date_MODIS,  y = AOD), colour ="dodgerblue2", na.rm=TRUE ) + 
  theme_bw() +  labs(x= "Fecha", y="550 nm AOD", title = "Serie de tiempo AERONET ")


#grafica de correlacion
ggplot(data= aod, aes(x= AOD, y= med_AOT_550)) + geom_point(na.rm=TRUE) + theme_bw() +
  labs(x= "MODIS 550 nm AOD", y="AERONET 550 nm AOD", title= "Correlacion MODIS - AERONET ")  + 
  geom_hline(aes(yintercept=0)) + 
  geom_vline(aes(xintercept=0)) + 
  geom_smooth(method = "lm", formula= y ~ x, se=TRUE, na.rm=TRUE, fullrange=TRUE) + 
  coord_cartesian( xlim= c(0,1), ylim= c(0,1)) + 
  scale_colour_distiller(palette = "Spectral", limits=c(0,0.07))


#Analisis de correlacion lineal
shapiro.test(aod$med_AOT_550) # con p < 0.05 podemos rechazar la normalidad 
cor.test(aod$AOD,aod$med_AOT_550, method = "kendall") # con 15 min 0.63   #con 30 min 0.63
cor.test(aod$AOD,aod$med_AOT_550, method = "pearson") # con 15 min 0.63   #con 30 min 0.63
cor.test(aod$AOD,aod$med_AOT_550, method = "spearman") # con 15 min 0.63   #con 30 min 0.63



reg <- lm( aod$med_AOT_550 ~ aod$AOD )  
summary(reg) #R2 =  0.6344 #R2 = 0.6333
coef(reg)
#anova(reg)

layout(matrix(1:4,2,2)) 
plot(reg)

# Al analizar el modelo se observan 2 datos extremos con muchos poder
# sobre el modelo lineal:
# aod[3183,] del 2016-07-27
# aod[4507,] del 2015-08-31

# Como el modelo busca representar las situaciones más probables y 
# no las situaciones extremas,
# se analiza el modelo descartando esos valores:

aod <- aod[-c(3183,4507),]
cor.test(aod$AOD,aod$med_AOT_550, method = "kendall") #0.63

reg <- lm( aod$med_AOT_550 ~ aod$AOD )  
summary(reg) #R2 =  0.6379   mejoro levemente   #para 30min R2 = 0.6367

layout(matrix(1:4,2,2)) 
plot(reg)



#grafica de correlacion
ggplot(data= aod, aes(x= AOD, y= med_AOT_550, colour=sd_AOT_550)) + geom_point(na.rm=TRUE) + theme_bw() +
  labs(x= "MODIS 550 nm AOD", y="AERONET 550 nm AOD", title= "Correlacion MODIS - AERONET ")  + 
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0)) + 
  geom_smooth(method = "lm", formula= y ~ x, se=TRUE, na.rm=TRUE, fullrange=TRUE) + 
  coord_cartesian( xlim= c(0,1), ylim= c(0,1)) + 
  scale_colour_distiller(palette = "Spectral", limits=c(0,0.07))

aod <- aod[,1:3]
aod <- aod[complete.cases(aod),]  #1240 puntos 
aod$modis_coregido <- reg$fitted.values


#grafica del modelo
ggplot(data= aod, aes(x= AOD, y= modis_coregido)) + geom_point(na.rm=TRUE) + theme_bw() +
  labs(x= "AOD por MODIS", y="AOD calibrado con AERONET", title= "Calibracion MODIS - AERONET ")  + 
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0)) + 
  geom_smooth(method = "lm", formula= y ~ x + 0, se=TRUE, na.rm=TRUE, fullrange=TRUE)+
  coord_cartesian( xlim= c(0,0.6), ylim= c(0,0.6))

# Al calibrar los datos de MODIS vemos que las mediciones mayores 
# ahora responden a valores mas bajos,
# mientras que los valores mas bajos han aumentado.

coef(reg)
# (Intercept)     aod$AOD 
# 0.01178339  0.84927256 

# Está claro q hay una relación temporal, 
# la cual no es considerada dentro del modelo de calibracion
# y por ello, se observa 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# ## CONCLUSIONES:  
#    un buffer más pequeño NO mejora la correlacion entre AOD y AERONET
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#### 6- Validacion datos MODIS   ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

aod <- read.csv("AOD_MODIS_1km_AERONET_mean.csv", header=TRUE, sep=",", dec=".")
aod$Date_MODIS <- as.character(aod$Date_MODIS)
aod$Date_MODIS <- as.POSIXlt(aod$Date_MODIS, tz="GMT") 
aod <- aod[-c(3183,4507),]

reg <- lm( aod$med_AOT_550 ~ aod$AOD )  
coef(reg)

confint(reg, level = 0.95)

modelo <- aod[,1:3]
modelo <- modelo[complete.cases(modelo[,2:3]),]  #1240 puntos 
modelo$fitted.reg <- fitted(reg)
modelo$residuals.reg <- residuals(reg)
modelo$rstudent.reg <- rstudent(reg)

#grafica del modelo 
ggplot(data= modelo , aes(x= med_AOT_550, y= fitted.reg)) + geom_point(na.rm=TRUE) + theme_bw() +
  labs(x= "AOD medido", y="AOD modelado", title= "Diagrama de dispersion")  + 
  geom_abline(aes(intercept=0, slope=1)) 

## Analisis de Residuos 

shapiro.test(modelo$rstudent.reg)   #Shapiro test 

# QQ Plot
qqnorm(modelo$rstudent.reg, main = "Normal(0,1)") 
qqline(modelo$rstudent.reg)

bptest(reg) #analisis de homogeneidad de varianza. p< 0.05 la variana no es constante en toda la muestra

plot(modelo$residuals.reg, pch = 20, ylab = "Residuos", xlab = "?ndices")
abline(h = cor(modelo$med_AOT_550, modelo$AOD))

dwtest(modelo$med_AOT_550 ~ modelo$AOD, alternative = "two.sided")  # test de contraste de Durbin-Watson

# p-value es muy pequeño, por tanto podemos rechazar la hipotesis nula
# y por tanto, los residuos se encuentran autocorrelacionados


#write.csv(modelo, "AOD_MODIS_AERONET.csv", row.names = FALSE)



############################################################

#### 7- Enfoque series de tiempo   ####

###########################################################

# 3 enfoques:
# * transformacion de los datos para estabilizar la media y la varianza
# * añadir variables explicativas cointegradas con la variable endógena 
# * Añadir variables explicativas deterministas para capturar la tendencia y estacionalidad


modelo <- read.csv("AOD_MODIS_AERONET.csv", header=TRUE, sep=",", dec=".", 
                   stringsAsFactors = FALSE)
modelo$Date_MODIS <- as.POSIXlt(modelo$Date_MODIS, tz="GMT") 
names(modelo) <- c("Date","MODIS","AERONET")
modelo <- modelo[order(modelo$Date),]

mod <- ts(modelo[,2:3], 
          frequency=365, # frecuencia anual
          start = c(2009, as.numeric(format(modelo[1,1], "%j")))) #es necesario convertirlo a julianos

plot(mod, main= "")

plot(decompose(mod[,1])) # MODIS
plot(decompose(mod[,2])) # AERONET

require(forecast)
fit <- auto.arima(mod[,1])  ## use auto.arima to choose ARIMA terms
summary(fit)

fore <- forecast(fit, h = 20)  #al ampliar el márgen no puede predecir las caidas
plot(fore)


par(mfrow = c(1,2))
acf(mod[,1],main="ACF Sales")
pacf(mod[,1],main="PACF Sales")

m= 200
fit <- Arima(mod[,1], order = c(2,0,1), xreg=fourier(mod[,1], K=4))
plot(forecast(fit, h=2*m, xreg=fourier(mod[,1], K=4, h=2*m)))
#https://robjhyndman.com/hyndsight/longseasonality/




# LA cita de mi tesis <3
# All models are wrong and some are even wronger

# If you wanted to be a ninja, 
# you might not take the full data, 
# and you might use the first 2/3 to predict the last 1/3 
# so you could evaluate forecasting errors vs. actual