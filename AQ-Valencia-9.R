# Sol Represa
# AQ Valencia
# 11/12/2018


# Objetivo:  Analisis exploratorio de las medias


library(openair)
library(ggplot2)
library(reshape)
library(dplyr)
library(lubridate)
library(RColorBrewer)


medias_dia <- readRDS("medias_dia_est_utiles.Rds")
pollutant_names <- readRDS("pollutant_names.Rds")
pollutant <- readRDS("pollutant_ut.Rds")

sitios <- read.csv("estaciones_utiles.csv")
sitios <- data.frame( Estacion = levels(sitios$Estacion), 
                      Abrev = c("Pla", "Raba", "Beni",  "Elx", "Elda", "Alcora",  "Alma", "Ben", "Bur", "Peny", "Grau",
                                "Pobla", "SJ", "VCid", "Viver", "Zorita", "Alba", "Buniol", "Caude", "Quart", "Sagunt",
                                "Pista", "Poli", "Moli", "Villar"))


# # # # # # # # # # # # # # # # # # # # # 

# 0. Carga de bases de datos   ####

# # # # # # # # # # # # # # # # # # # # # 


## Bases de datos 

i=1
datos_no_dia <- medias_dia[[i]]
datos_no_dia$date <- as.POSIXct(datos_no_dia$date, format = "%Y-%m-%d")
names(datos_no_dia)[2:22] <- as.character(sitios[which(sitios$Estacion %in% names(datos_no_dia)), 2])

datos_no_hora <- pollutant[[i]]
datos_no_hora <- datos_no_hora[,-1]
names(datos_no_hora)[2:22] <- as.character(sitios[which(sitios$Estacion %in% names(datos_no_hora)), 2])



i=2
datos_no2_dia <- medias_dia[[i]]
datos_no2_dia$date <- as.POSIXct(datos_no2_dia$date, format = "%Y-%m-%d")
names(datos_no2_dia)[2:22] <- as.character(sitios[which(sitios$Estacion %in% names(datos_no2_dia)), 2])

datos_no2_hora <- pollutant[[i]]
datos_no2_hora <- datos_no2_hora[,-1]
names(datos_no2_hora)[2:22] <- as.character(sitios[which(sitios$Estacion %in% names(datos_no2_hora)), 2])


#datos_no_no2 <- cbind(datos_no_dia[1], round(datos_no_dia[-1]/datos_no2_dia[-1], 2))


i=4
datos_o3_dia <- medias_dia[[i]]
datos_o3_dia$date <- as.POSIXct(datos_o3_dia$date, format = "%Y-%m-%d")
names(datos_o3_dia)[2:24] <- as.character(sitios[which(sitios$Estacion %in% names(datos_o3_dia)), 2])

datos_o3_hora <- pollutant[[i]]
datos_o3_hora <- datos_o3_hora[,-1]
names(datos_o3_hora)[2:24] <- as.character(sitios[which(sitios$Estacion %in% names(datos_o3_hora)), 2])



i=5
datos_pm10_dia <- medias_dia[[i]]
datos_pm10_dia$date <- as.POSIXct(datos_pm10_dia$date, format = "%Y-%m-%d")
names(datos_pm10_dia)[2:15] <- as.character(sitios[which(sitios$Estacion %in% names(datos_pm10_dia)), 2])

datos_pm10_hora <- pollutant[[i]]
datos_pm10_hora <- datos_pm10_hora[,-1]
names(datos_pm10_hora)[2:15] <- as.character(sitios[which(sitios$Estacion %in% names(datos_pm10_hora)), 2])

i=6
datos_pm25_dia <- medias_dia[[i]]
datos_pm25_dia$date <- as.POSIXct(datos_pm25_dia$date, format = "%Y-%m-%d")
names(datos_pm25_dia)[2:16] <- as.character(sitios[which(sitios$Estacion %in% names(datos_pm25_dia)), 2])

datos_pm25_hora <- pollutant[[i]]
datos_pm25_hora <- datos_pm25_hora[,-1]
names(datos_pm25_hora)[2:16] <- as.character(sitios[which(sitios$Estacion %in% names(datos_pm25_hora)), 2])


i=7
datos_so2_dia <- medias_dia[[i]]
datos_so2_dia$date <- as.POSIXct(datos_so2_dia$date, format = "%Y-%m-%d")
names(datos_so2_dia)[2:22] <- as.character(sitios[which(sitios$Estacion %in% names(datos_so2_dia)), 2])

datos_so2_hora <- pollutant[[i]]
datos_so2_hora <- datos_so2_hora[,-1]
names(datos_so2_hora)[2:22] <- as.character(sitios[which(sitios$Estacion %in% names(datos_so2_hora)), 2])



# # # # # # # # # # # # # # # # # # # # # 

## 1. TABLA estadisticos datos diarios ####

# # # # # # # # # # # # # # # # # # # # # 


summ_no2 <- datos_no2_dia %>% 
  melt( id = "date") %>% 
  group_by(variable) %>%
  summarize(
    #median = round(median(value,na.rm=TRUE),2),
    #sd = round(sd(value,na.rm=TRUE),2),
    mean = round(mean(value, na.rm=TRUE),2),
    min = round(min(value,na.rm=TRUE), 2), 
    max = round(max(value,na.rm=TRUE),2)) 

summ_no <- datos_no_dia %>% 
  melt( id = "date") %>% 
  group_by(variable) %>%
  summarize(
    mean = round(mean(value, na.rm=TRUE),2),
    min = round(min(value,na.rm=TRUE), 2), 
    max = round(max(value,na.rm=TRUE),2))


summ_o3 <- datos_o3_dia %>% 
  melt( id = "date") %>% 
  group_by(variable) %>%
  summarize(
    mean = round(mean(value, na.rm=TRUE),2),
    min = round(min(value,na.rm=TRUE), 2), 
    max = round(max(value,na.rm=TRUE),2))


summ_so2 <- datos_so2_dia %>% 
  melt( id = "date") %>% 
  group_by(variable) %>%
  summarize(
    mean = round(mean(value, na.rm=TRUE),2),
    min = round(min(value,na.rm=TRUE), 2), 
    max = round(max(value,na.rm=TRUE),2))


summ_pm10 <- datos_pm10_dia %>% 
  melt( id = "date") %>% 
  group_by(variable) %>%
  summarize(
    mean = round(mean(value, na.rm=TRUE),2),
    min = round(min(value,na.rm=TRUE), 2), 
    max = round(max(value,na.rm=TRUE),2)) %>%
  mutate( rango = max -min)


summ_pm2.5 <- datos_pm25_dia %>% 
  melt( id = "date") %>% 
  group_by(variable) %>%
  summarize(
    mean = round(mean(value, na.rm=TRUE),2),
    min = round(min(value,na.rm=TRUE), 2), 
    max = round(max(value,na.rm=TRUE),2)) %>%
  mutate( rango = max -min)


a <- merge(summ_no2, summ_no, by= "variable", suffixes = c(".no2",".no"))
b <- merge(summ_so2, summ_o3, by = "variable", suffixes = c(".so2",".o3" ), all = TRUE)
c <- merge(summ_pm10, summ_pm2.5, by = "variable", suffixes = c(".pm10",".pm25" ), all = TRUE)

d <- merge(a,b, by = "variable", all = TRUE)
salida <- merge(d, c, by = "variable", all = TRUE)

tabla <- data.frame(Estacion = salida$variable,
                    NO = paste(salida$mean.no, " (", salida$min.no, " - ", salida$max.no, ")", sep = ""),
                    NO2 = paste(salida$mean.no2, " (", salida$min.no2, " - ", salida$max.no2, ")", sep = ""),
                    O3 = paste(salida$mean.o3, " (", salida$min.o3, " - ", salida$max.o3, ")", sep = ""),
                    SO2 = paste(salida$mean.so2, " (", salida$min.so2, " - ", salida$max.so2, ")", sep = ""),
                    PM10 = paste(salida$mean.pm10, " (", salida$min.pm10, " - ", salida$max.pm10, ")", sep = ""),
                    PM25 = paste(salida$mean.pm25, " (", salida$min.pm25, " - ", salida$max.pm25, ")", sep = ""))


#write.csv(tabla, "summary_daily.csv", row.names = FALSE)

rm(summ_no, summ_no2, summ_o3, summ_pm10, summ_pm2.5, summ_so2, a, b,c, d)



# # # # # # # # # # # # # # # # # # # # # 

# 2. Plot exploratorios  ####

# # # # # # # # # # # # # # # # # # # # # 


## 2. 1 ) Histogramas por estaciones y por contaminante ###


tabla <- datos_pm25_hora

par(mfrow=c(3,7))
for( i in 2:length(tabla)){
   hist(tabla[,i], 
       main="", 
       xlab=c("Concentración", names(tabla[i])),
       ylab="Frecuencia",
       border="white", 
       col="grey", 
       las=1, 
       breaks=50, 
       prob = FALSE)
}


# ver O3 que teien distribucion normal
# ver NO2 que muestra un doble comportamiento 



## 2.2 Box- Plot mensual  ###
# Sacado de "file:///F:/BKP/DiscoE/UNLU/TESIS/Plot exploratorio.R"


tabla <-datos_no_hora

par(mfrow=c(7,3))
for( i in 2:length(tabla)){
  plot(as.factor(format(tabla$date, "%Y-%B")), 
     tabla[,i], ylab=c(names(tabla[i]), unit),  col="grey", 
     frame.plot=FALSE, outline =FALSE)
}




# # # # # # # # # # # # # # # # # # # # # 

# NOx (NO + NO2)  ####

# # # # # # # # # # # # # # # # # # # # # 


# Estadisticos datos diarios

datos_no_hora %>% 
  melt( id = "date") %>% 
  mutate( year = year(date)) %>%
  group_by(variable, year) %>%
  summarize(
    #median = round(median(value,na.rm=TRUE),2),
    #sd = round(sd(value,na.rm=TRUE),2),
    #min = round(min(value,na.rm=TRUE), 2), 
    #max = round(max(value,na.rm=TRUE),2),
    mean = round(mean(value, na.rm=TRUE),2))
    
datos_no2_year <- datos_no2_hora %>% 
  melt( id = "date") %>% 
  mutate( year = year(date)) %>%
  group_by(variable, year) %>%
  summarize(
    #median = round(median(value,na.rm=TRUE),2),
    #sd = round(sd(value,na.rm=TRUE),2),
    #min = round(min(value,na.rm=TRUE), 2), 
    #max = round(max(value,na.rm=TRUE),2),
    mean = round(mean(value, na.rm=TRUE),2))


# Limite NO2 anual
View(datos_no2_year[which( datos_no2_year$mean >= 40),]) 


# Limite NO2 diario
datos <- datos_no2_hora %>% 
  melt( id = "date") 
View(datos[which( datos$value >= 200),]) 


# Violin todos los datos
datos_no2_hora %>% 
  melt( id = "date") %>%
  group_by(variable) %>%
  ggplot(aes(x= variable, y = value)) + 
  geom_violin(na.rm = TRUE)  +
  theme_bw() +  
  labs(x = "", y ="", title= "Mediciones horarias NO2 (units)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1) ) + 
  geom_hline(yintercept=200, linetype="dashed", color = "red" )




# # # # # # # # # # # # # # # # # # # # # 

# O3 ####

# # # # # # # # # # # # # # # # # # # # # 


# Estadisticos O3


# Plot medias de las mediciones diarias

datos_o3_dia %>% 
  melt( id = "date") %>% 
  group_by(variable) %>%
  summarize(#min = round(min(value,na.rm=TRUE), 2), 
    #median = round(median(value,na.rm=TRUE),2),
    #max = round(max(value,na.rm=TRUE),2),
    #sd = round(sd(value,na.rm=TRUE),2),
    mean = round(mean(value, na.rm=TRUE),2)) %>%
  ggplot(aes(x= variable, y = mean )) + 
  geom_col() +
  labs(x = "", y = "Mean O3 (units)", title = "O3  - media de las mediciones diarias") +
  theme_bw() +  
  theme(axis.title = element_text(size = rel(0.8) ),
        axis.text.x = element_text(angle = 90, vjust = 0.5 , hjust = 1))


# maximos valores en las estaciones

datos_o3_hora %>% 
  melt( id = "date") %>% 
  group_by(variable) %>%
  summarize(#min = round(min(value,na.rm=TRUE), 2), 
    #median = round(median(value,na.rm=TRUE),2),
    #sd = round(sd(value,na.rm=TRUE),2),
    #mean = round(mean(value, na.rm=TRUE),2),
    max = round(max(value,na.rm=TRUE),2)) %>%
  ggplot(aes(x= variable, y = max )) + 
  geom_col() +
  theme_bw() +  
  theme(axis.title = element_text(size = rel(0.8)),
        axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1) ) +
  labs(x = "", y = "Mean O3 (units)", title = "O3  - Maximo de las mediciones horarias")


# Sin utilidad 
datos_o3_hora %>% 
  melt( id = "date") %>%
  group_by(variable) %>%
  ggplot(aes(x= variable, y = value)) + 
  geom_violin(na.rm = TRUE)  +
  theme_bw() +  
  labs(x = "", y ="", title= "Mediciones horarias O3 (units)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1) )



### LIMITES LEGALES


library(zoo)
# Media movil (simple moving average)

datos_o3_8hs <- rollapply(datos_o3_hora[2:length(datos_o3_hora)], 
)

fecha <- seq(from= as.POSIXct("2009-01-01 00:00:00", tz = "GMT"),
             to = as.POSIXct("2018-07-31 23:00:00", tz = "GMT"), by = "1 hour")
datos_o3_8hs <- data.frame(fecha, datos_o3_8hs)


# Ver también
#datos_o3_8hs <- rollmean(datos_o3_hora[2], 8, na.pad = TRUE)



# Violin promedios 8 hs

png(file = "violin_8hora_o3.png", width = 450, height = 280)
datos_o3_8hs %>% 
  melt( id = "fecha") %>%
  group_by(variable) %>%
  ggplot(aes(x= variable, y = value)) + 
  geom_violin(na.rm = TRUE)  +
  theme_bw() +  
  labs(x = "", y ="", title= "Promedios 8hs de las mediciones O3") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1) ) + 
  geom_hline(yintercept=100, linetype="dashed", color = "blue") +  #OMS
  geom_hline(yintercept=120, linetype="dashed", color = "red")  # umbral alerta
dev.off()


#caja limite legal octohorario 120
png(file = "lim_o3_8hs_super_tab.png", width = 350, height = 350)
datos_o3_8hs  %>% 
  melt( id = "fecha") %>%
  .[which( .$value >= 120),] %>% 
  mutate(year = year(fecha)) %>% 
  mutate(mes = month(fecha)) %>% 
  mutate(day = day(fecha)) %>%
  group_by(variable, year, mes, day) %>% 
  summarise(mediciones_dia = n()) %>%
  group_by(variable, year) %>% 
  summarise(n = n()) %>%
  filter(n>25) %>%
  ggplot(aes(x = factor(year) , y = variable , fill = n )) + 
  geom_tile() +
  geom_text(aes(label= n)) +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1),
        legend.position = "none") + 
  scale_fill_continuous(low="thistle2", high ="darkred", na.value = "transparent") + 
  labs(x = "", y ="", title= "Superacion de limite 8hs - O3")
dev.off()


#caja limite OMS octohorario 100
png(file = "lim_o3_8hs_super_OMS_tab.png", width = 350, height = 350)
datos_o3_8hs  %>% 
  melt( id = "fecha") %>%
  .[which( .$value >= 100),] %>% 
  mutate(year = year(fecha)) %>% 
  mutate(mes = month(fecha)) %>% 
  mutate(day = day(fecha)) %>%
  group_by(variable, year, mes, day) %>% 
  summarise(mediciones_dia = n()) %>%
  group_by(variable, year) %>% 
  summarise(n = n()) %>%
  filter(n>25) %>%
  ggplot(aes(x = factor(year) , y = variable , fill = n )) + 
  geom_tile() +
  geom_text(aes(label= n)) +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1),
        legend.position = "none") + 
  scale_fill_continuous(low="thistle2", high ="darkred", na.value = "transparent") + 
  labs(x = "", y ="", title= "Superacion de limite OMS 8hs - O3")
dev.off()


# cuando se superan los limites de 120 ugm-3 de 8hs

sup <- datos_o3_8hs  %>% 
  melt( id = "fecha") %>%
  .[which( .$value >= 120),] %>% 
  mutate(year = year(fecha)) %>% 
  mutate(mes = month(fecha)) %>%
  mutate(day = day(fecha)) %>%
  group_by(variable, year, mes, day) %>% 
  summarise(mediciones_dia = n()) %>%
  group_by(variable, year) %>% 
  summarise(n = n()) %>%
  filter(n > 25) %>%
  select(variable, year)

write.csv(sup, "estaciones_superan_o3.csv", row.names = FALSE)  


### AVISOS 

# Violin - umbral de aviso a la población 180, y alerta 240
png(file = "lim_aviso_hora_o3.png", width = 700, height = 400)
datos_o3_hora %>% 
  melt( id = "date") %>%
  group_by(variable) %>%
  ggplot(aes(x= variable, y = value)) + 
  geom_violin(na.rm = TRUE)  +
  theme_bw() +  
  labs(x = "", y ="", title= "Mediciones horarias de O3") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1) ) + 
  geom_hline(yintercept=180, linetype="dashed", color = "red") +
  geom_hline(yintercept=240, linetype="dashed", color = "darkred")
dev.off()

# The information threshold 
# (considered to carry health risks for short-time exposure of particularly sensitive groups) 
# is 180 μg m−3 for hourly average mass concentration, and the alert threshold (considered 
# to carry health risks for short-time exposure of the population in general) is 240 μg m−3.


# cuando se supero el los limites de 180 ugm-3 horario (y emitir aviso)

sup_aviso <- datos_o3_hora %>% 
  melt( id = "date") %>%
  .[which( .$value >= 180),] %>% 
  mutate(year = year(date)) %>% 
  mutate(mes = month(date)) %>%
  mutate(day = day(date)) %>%
  group_by(variable, year,mes, day) %>% 
  summarise(mediciones_dia = n()) %>%
  group_by(variable, year) %>% 
  summarise(n = n()) 
#write.csv(sup_aviso, "estaciones_superan_o3_aviso.csv", row.names = FALSE)  



i = 4 
cluster[[i]]


# en el cluster están juntas las concentraciones que obtuvieron máximos menores
# en q año ocurre?
# X46250046  el maximo de los valores máximos (116.583)
# X46258001  el mínimo de los valores máximos (113.75)
#(mismas estaciones que para el NO-NO2)

# Analizar cuartil 95 !!



## CUANDO HAY EXCESOS?


tabla_m <- melt(tabla, id = "date")

max(tabla_m$value, na.rm = TRUE)


ggplot(tabla_m, aes(x= date, y = value, col= variable)) + 
  geom_point() + 
  theme_bw() +
  scale_x_datetime(date_labels = "%Y-%m") + labs(title = "O3")



# # # # # # # # # # # # # # # # # # # # # 

# SO2 ####

# # # # # # # # # # # # # # # # # # # # # 


# Estadisticos SO2


# Plot medias de las mediciones diarias

datos_so2_dia %>% 
  melt( id = "date") %>% 
  group_by(variable) %>%
  summarize(#min = round(min(value,na.rm=TRUE), 2), 
    #median = round(median(value,na.rm=TRUE),2),
    #max = round(max(value,na.rm=TRUE),2),
    #sd = round(sd(value,na.rm=TRUE),2),
    mean = round(mean(value, na.rm=TRUE),2)) %>%
  ggplot(aes(x= variable, y = mean )) + 
  geom_col() +
  labs(x = "", y = "Mean O3 (units)", title = "O3  - media de las mediciones diarias") +
  theme_bw() +  
  theme(axis.title = element_text(size = rel(0.8) ),
        axis.text.x = element_text(angle = 90, vjust = 0.5 , hjust = 1))


# maximos valores en las estaciones

datos_so2_hora %>% 
  melt( id = "date") %>% 
  group_by(variable) %>%
  summarize(#min = round(min(value,na.rm=TRUE), 2), 
    #median = round(median(value,na.rm=TRUE),2),
    #sd = round(sd(value,na.rm=TRUE),2),
    #mean = round(mean(value, na.rm=TRUE),2),
    max = round(max(value,na.rm=TRUE),2)) %>%
  ggplot(aes(x= variable, y = max )) + 
  geom_col() +
  theme_bw() +  
  theme(axis.title = element_text(size = rel(0.8)),
        axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1) ) +
  labs(x = "", y = "Mean O3 (units)", title = "O3  - Maximo de las mediciones horarias")


### LIMITES LEGALES

# Violin datos horarios - lim = 350
png(file = "violin_hora_so2.png", width = 450, height = 280)
datos_so2_hora %>% 
  melt( id = "date") %>%
  group_by(variable) %>%
  ggplot(aes(x= variable, y = value)) + 
  geom_violin(na.rm = TRUE)  +
  theme_bw() +  
  labs(x = "", y ="", title= "Mediciones horarias de SO2") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1) ) + 
  geom_hline(yintercept=200, linetype="dashed", color = "red") # legal
dev.off()


# Violin datos diarios - lim = 125 EU, lim = 20 OMS
png(file = "lim_dia_so2.png", width = 700, height = 400)
datos_so2_dia %>% 
  melt( id = "date") %>%
  group_by(variable) %>%
  ggplot(aes(x= variable, y = value)) + 
  geom_violin(na.rm = TRUE)  +
  theme_bw() +  
  labs(x = "", y ="", title= "Mediciones diarias de SO2") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1) ) + 
  geom_hline(yintercept=125, linetype="dashed", color = "red")  +  # legal
  geom_hline(yintercept= 20, linetype="dashed", color = "blue")    # OMS
dev.off()



## CUANDO HAY EXCESOS?


# Extrem values DIARIOS
datos <- datos_so2_dia  %>% 
  melt( id = "date") %>% 
  .[which( .$value >= 125),] 

#OMS
a <- datos_so2_dia %>% 
  melt( id = "date") %>%
  .[which( .$value >= 20),] %>% 
  mutate(year = year(date)) %>% 
  mutate(mes = month(date)) %>%
  mutate(day = day(date)) %>%
  group_by(variable, year,mes, day) %>% 
  summarise(mediciones_dia = n()) %>%
  group_by(variable, year) %>% 
  summarise(n = n()) 


# Extrem values horario
datos <- datos_so2_hora  %>% 
  melt( id = "date") %>% 
  .[which( .$value >= 200),] 

#evento extremo 
datos_so2_hora[1960:1990, ] %>% 
  melt( id= "date") %>% 
  filter( variable == "X12009007") %>%
  ggplot( aes( x= date , y = value)) + 
  geom_point() + 
  geom_line() +
  theme_bw() +
  labs( x= "", y = "", title = "Evento extremo 24/03/2009 estacion X12009007")



# # # # # # # # # # # # # # # # # # # # # 

# PM10 ####

# # # # # # # # # # # # # # # # # # # # # 


library(zoo)
# Media movil (simple moving average)

datos_pm10_year <- rollapply(datos_pm10_dia[2:length(datos_pm10_dia)], 
                          width = 365, 
                          FUN = function(x) mean(x, na.rm=TRUE), 
                          by.column = TRUE, 
                          partial = FALSE, #siempre completa la ventana 
                          fill = NA, 
                          align = "left")


fecha <- seq(from= as.POSIXct("2009-01-01 00:00:00", tz = "GMT"),
             to = as.POSIXct("2018-07-31 23:00:00", tz = "GMT"), by = "1 day")
datos_pm10_year <- data.frame(fecha, datos_pm10_year)


# ¿Cuando se sobre pasan limiter anuales
datos_pm10_year %>% 
  melt( id = "fecha") %>%
  .[which( .$value >= 40),]   # concentracion limite anual

View(datos_pm10_year %>% 
  melt( id = "fecha") %>%
  .[which( .$value >= 20),] %>% # concentracion limite anual
  mutate(year = year(fecha)) %>% 
  mutate(mes = month(fecha)) %>% 
  mutate(day = day(fecha)) %>%
  group_by(variable, year, mes, day) %>% 
  summarise(mediciones_dia = n()) %>%
  group_by(variable, year) %>% 
  summarise(n = n()))


# Violin datos diarios - lim = 50 EU, 35 veces EU y 3 veces la 20 OMS
png(file = "lim_dia_pm10.png", width = 450, height = 280)
datos_pm10_dia %>% 
  melt( id = "date") %>%
  group_by(variable) %>%
  ggplot(aes(x= variable, y = value)) + 
  geom_violin(na.rm = TRUE)  +
  theme_bw() +  
  labs(x = "", y ="", title= "Mediciones diarias de PM10") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1) ) + 
  geom_hline(yintercept = 50, linetype="dashed", color = "red")   # legal
dev.off()



# ¿cuantas veces por año se supera el limite diario de 50?
png(file = "lim_pm10_dia_super_tab.png", width = 450, height = 450)
datos_pm10_dia  %>% 
  melt( id = "date") %>%
  .[which( .$value >= 50),] %>% # concentracion limite diaria
  mutate(year = year(date)) %>% 
  mutate(mes = month(date)) %>% 
  mutate(day = day(date)) %>%
  group_by(variable, year, mes, day) %>% 
  summarise(mediciones_dia = n()) %>%
  group_by(variable, year) %>% 
  summarise(n = n()) %>%
  filter(n>3) %>%
  ggplot(aes(x = factor(year) , y = variable , fill = n )) + 
  geom_tile() +
  geom_text(aes(label= n)) +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1),
        legend.position = "none") + 
  scale_fill_continuous(low="powderblue", high ="royalblue3", na.value = "transparent") + 
  labs(x = "", y ="", title= "Superacion de limite diario - PM10")
dev.off()


sup_pm10 <- datos_pm10_dia %>% 
  melt( id = "date") %>%
  .[which( .$value >= 50),] %>% 
  mutate(year = year(date)) %>% 
  mutate(mes= month(date)) %>%
  mutate(day = day(date)) %>%
  group_by(variable, year, mes, day) %>% 
  summarise(mediciones_dia = n()) %>%
  group_by(variable, year) %>% 
  summarise(n = n())  %>%
  filter( n > 3)
write.csv(sup_pm10, "estaciones_superan_pm10.csv", row.names = FALSE)


# # # # # # # # # # # # # # # # # # # # # 

# PM25 ####

# # # # # # # # # # # # # # # # # # # # # 



datos_pm25_year <- rollapply(datos_pm25_dia[2:length(datos_pm25_dia)], 
                             width = 365, 
                             FUN = function(x) mean(x, na.rm=TRUE), 
                             by.column = TRUE, 
                             partial = FALSE, #siempre completa la ventana 
                             fill = NA, 
                             align = "left")


date <- seq(from= as.POSIXct("2009-01-01 00:00:00", tz = "GMT"),
             to = as.POSIXct("2018-07-31 23:00:00", tz = "GMT"), by = "1 day")
datos_pm25_year <- data.frame(date, datos_pm25_year)


# ANUALES

# superacion anual - lim = 25 

View(datos_pm25_year  %>%
       melt( id = "date") %>% 
   mutate(year = year(date)) %>% 
  mutate(mes = month(date)) %>% 
    mutate(day = day(date)) %>%
    group_by(variable, year) %>%
  summarize(
    mean = round(mean(value, na.rm=TRUE),2),
    min = round(min(value,na.rm=TRUE), 2), 
    max = round(max(value,na.rm=TRUE),2)) %>%
  mutate( rango = max -min))

# superacion anual - lim = 10 de la OMS
# cantidad de superaciones del limite anual
sup_pm25 <- datos_pm25_year %>% 
  melt( id = "date") %>%
  .[which( .$value >= 10),] %>% 
  mutate(year = year(date)) %>% 
  mutate(mes = month(date)) %>% 
  mutate(day = day(date)) %>%
  group_by(variable, year, mes, day) %>% 
  summarise(mediciones_dia = n()) %>%
  group_by(variable, year) %>% 
  summarise(n = n()) 


# DIARIOS
# Violin datos diarios - lim = 25 EU, 3 veces la OMS

png(file = "lim_dia_pm25.png", width = 450, height = 280)
datos_pm25_dia %>% 
  melt( id = "date") %>%
  group_by(variable) %>%
  ggplot(aes(x= variable, y = value)) + 
  geom_violin(na.rm = TRUE)  +
  theme_bw() +  
  labs(x = "", y ="", title= "Mediciones diarias de PM2.5") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1) ) + 
  geom_hline(yintercept = 25, linetype="dashed", color = "red")   # legal
dev.off()


#superacion diaria
sup_pm25 <- datos_pm25_dia %>% 
  melt( id = "date") %>%
  .[which( .$value >= 25),] %>% 
  mutate(year = year(date)) %>% 
  mutate(mes = month(date)) %>%
  mutate(day = day(date)) %>%
  group_by(variable, year, mes, day) %>% 
  summarise(mediciones_dia = n()) %>%
  group_by(variable, year) %>% 
  summarise(n = n())  %>%
  filter( n > 3)
#write.csv(sup_pm25, "estaciones_superan_pm25.csv", row.names = FALSE)


# cantidad de veces que se supera el valor limite 
#png(file = "lim_pm25_dia_super.png", width = 680, height = 600)
datos_pm25_dia  %>% 
  melt( id = "date") %>%
  .[which( .$value >= 10),] %>% 
  mutate(year = year(date)) %>% 
  mutate(mes = month(date)) %>% 
  mutate(day = day(date)) %>%
  group_by(variable, year, mes, day) %>% 
  summarise(mediciones_dia = n()) %>%
  group_by(variable, year) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x= year, y = n )) + 
  geom_bar(stat = "identity", fill = "white", col = "black") +
  geom_hline(yintercept = 3, linetype="dashed", color = "blue") +
  facet_wrap(~variable) +
  theme_bw() +  
  labs(x = "", y ="", title= "Superacion de limite horario - PM25")
#dev.off()



png(file = "lim_pm25_dia_super_tab.png", width = 470, height = 470)
datos_pm25_dia  %>% 
  melt( id = "date") %>%
  .[which( .$value >= 10),] %>% 
  mutate(year = year(date)) %>% 
  mutate(mes = month(date)) %>% 
  mutate(day = day(date)) %>%
  group_by(variable, year, mes, day) %>% 
  summarise(mediciones_dia = n()) %>%
  group_by(variable, year) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x = factor(year) , y = variable , fill = n )) + 
  geom_tile() +
  geom_text(aes(label= n)) +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1),
        legend.position = "none") + 
  scale_fill_continuous(low="powderblue", high ="royalblue3", na.value = "transparent") + 
  labs(x = "", y ="", title= "Superacion de limite horario - PM25")
dev.off()


