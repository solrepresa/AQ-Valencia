# Sol Represa
# AQ Valencia
# 31/01/2019


# Objetivo:  Analisis exploratorio


library(openair)
library(ggplot2)
library(reshape)
library(dplyr)
library(lubridate)
library(RColorBrewer)
# library(patchwork)

#cluster <- readRDS("cluster_estaciones.Rds")
medias_dia <- readRDS("medias_dia_est_utiles.Rds")
pollutant_names <- readRDS("pollutant_names.Rds")
pollutant <- readRDS("pollutant_ut.Rds")

sitios <- read.csv("estaciones_utiles.csv")
sitios <- data.frame( Estacion = levels(sitios$Estacion), 
                      Abrev = c("Pla", "Raba", "Beni",  "Elx", "Elda", "Alcora",  "Alma", "Ben", "Bur", "Peny", "Grau",
                                "Pobla", "SJ", "VCid", "Viver", "Zorita", "Alba", "Buniol", "Caude", "Quart", "Sagunt",
                                "Pista", "Poli", "Moli", "Villar"))

# # # # # # # # # # # # # # # # # # # # # 

## Data sets 

# # # # # # # # # # # # # # # # # # # # # 

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

# 1. Graficas de tendencia   ####

# # # # # # # # # # # # # # # # # # # # # 


# 1. 1 Generar graficas de tendencias NO + NO2 ####


# NO2
png(file = paste("trend_NO2.png", sep =""), 
    width = 500, 
    height = 600)
Theil_no2 <- datos_no2_hora %>% 
  melt( id = "date") %>% 
  rename( site = variable) %>%
  rename( no2 = value) %>%
  TheilSen(., pollutant = "no2", ylab = "NO2", 
           deseason = TRUE, 
           slope.percent = TRUE,
           type = "site",
           layout = c(3, 8),
           scales = list(y = list(rot=45), x = list(rot = 45)))

dev.off()


#NO
png(file = paste("trend_NO.png", sep =""), 
    width = 500, 
    height = 600)
Theil_no <- datos_no_hora %>% 
  melt( id = "date") %>% 
  rename( site = variable) %>%
  rename( no = value) %>%
  TheilSen(., pollutant = "no", ylab = "NO", 
           deseason = TRUE, 
           slope.percent = TRUE,
           type = "site",
           layout = c(3, 7),
           scales = list(y = list(rot=45), x = list(rot = 45)))

dev.off()

#O3
png(file = paste("trend_O3.png", sep =""), 
    width = 500, 
    height = 600)
Theil_o3 <- datos_o3_hora %>% 
  melt( id = "date") %>% 
  rename( site = variable) %>%
  rename( o3 = value) %>%
  TheilSen(., pollutant = "o3", ylab = "O3", 
           deseason = TRUE, 
           slope.percent = TRUE,
           type = "site",
           layout = c(3, 8),
           scales = list(y = list(rot=45), x = list(rot = 45)))
dev.off()



#SO2
png(file = paste("trend_SO2.png", sep =""), 
    width = 500, 
    height = 600)
Theil_so2 <- datos_so2_hora %>% 
  melt( id = "date") %>% 
  rename( site = variable) %>%
  rename( so2 = value) %>%
  TheilSen(., pollutant = "so2", ylab = "SO2", 
           deseason = TRUE, 
           slope.percent = TRUE,
           type = "site",
           layout = c(3, 7),
           scales = list(y = list(rot=45), x = list(rot = 45)))

dev.off()



# PM10
png(file = paste("trend_PM10.png", sep =""), 
    width = 500, 
    height = 600)
Theil_pm10 <- datos_pm10_hora %>% 
  melt( id = "date") %>% 
  rename( site = variable) %>%
  rename( pm10 = value) %>%
  TheilSen(., pollutant = "pm10", ylab = "PM10", 
           deseason = TRUE, 
           slope.percent = TRUE,
           type = "site",
           layout = c(3, 5),
           scales = list(y = list(rot=45), x = list(rot = 45)))

dev.off()



# PM2.5
png(file = paste("trend_PM25.png", sep =""), 
    width = 500, 
    height = 600)
Theil_pm25 <- datos_pm25_hora %>% 
  melt( id = "date") %>% 
  rename( site = variable) %>%
  rename( pm25 = value) %>%
  TheilSen(., pollutant = "pm25", ylab = "PM2.5", 
           deseason = TRUE, 
           slope.percent = TRUE,
           type = "site",
           layout = c(3, 5),
           scales = list(y = list(rot=45), x = list(rot = 45)))

dev.off()


# # # # # # # # # # # # # # # # # # # # # 


a <-Theil_pm25$data$res2
a <- a[,c(1,2,12,16, 17)]
a <- data.frame( site = a$site,
                 p.stars = a$p.stars,
                 tendencia = paste(round(a$slope, 3), 
                                   " [", 
                                   round(a$lower, 3), ", ", 
                                   round(a$upper, 3), "]", sep="" ))







#tend_no[,5:length(tend_no)] <- round(tend_no[,5:length(tend_no)], digits = 2)
#tend_no2[,5:length(tend_no2)] <- round(tend_no2[,5:length(tend_no2)], digits = 2)

#tend_no <- tend_no[,c(1,3,13,17:21)]
#tend_no2 <- tend_no2[,c(1,3,13,17:21)]

#write.csv(tend_no, "tend_no.csv", row.names = FALSE)
#write.csv(tend_no2, "tend_no2.csv", row.names = FALSE)


# De este codigo armé función 
# para generar gráficas de tendencia + archivo con p-value 


# # # # # # # # # # # # # # # # # # # # # 

# 1. 2 Generar graficas de tendencias ####

# # # # # # # # # # # # # # # # # # # # # 

#source("C:/Users/narep/Desktop/SOL/AQ-Valencia/fuction/tendencia.R")

#tendencia(datos_o3_hora, "o3")
#tendencia(datos_pm10_hora, "pm10")
#tendencia(datos_pm25_hora, "pm25")
#tendencia(datos_so2_hora, "so2")


#timeVariation(., pollutant = c("no", "no2", "o3", "so2"), normalise = TRUE )




# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## 2 Graficas de comportamiento ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 


## 2.1 Graficas de tendencia - open air ####

for (i in 2:20) {
  # NO-NO2 y O3 no tienen las mismas estaciones,
  # por lo tanto en las gráficas de comportamiento no podrán estar los mismos contaminantes
  # solo se graficarán aquellas en donde estén los 3
  
  datos_no2_sin <- datos_no2_hora[,-c(7,8)]
  datos_no_sin <- datos_no_hora[,-c(7,8)]
  datos_o3_sin <- datos_o3_hora[,-c(3,15, 22, 23)]
  
  tabla <- cbind(datos_no_sin[1], datos_no_sin[i], datos_no2_sin[i], datos_o3_sin[i])
  names(tabla) <- c("date", "no", "no2", "o3")
  estacion <- names(datos_no_hora)[i]
  
  
  # Behaviour Normal
  png(file = paste("behaviour_", estacion ,".png", sep =""), 
      width = 800, 
      height = 550)
  tabla %>% timeVariation(., pollutant = c("no", "no2", "o3"), normalise = TRUE )
  dev.off()
}



# Graficas de comportamiento por año ###
# Analizando las estaciones que comparten los 3 contaminantes 

for (i in 2:20) {
  datos_no2_sin <- datos_no2_hora[,-c(7,8)]
  datos_no_sin <- datos_no_hora[,-c(7,8)]
  datos_o3_sin <- datos_o3_hora[,-c(3,15, 22, 23)]
  
  tabla <- cbind(datos_no_sin[1], datos_no_sin[i], datos_no2_sin[i], datos_o3_sin[i])
  names(tabla) <- c("date", "no", "no2", "o3")
  estacion <- names(datos_no_hora)[i]
  tabla$year <- year(tabla$date)
  
  for (j in 2009:2018) {     # Behaviour Normal per year
    control <- tabla %>% filter(year == j )
    
    if(!all(is.na(control[,2:4]))){    #para evitar error por años sin datos
      png(file = paste("behaviour_", estacion ,"_", j ,".png", sep =""), 
          width = 800, 
          height = 550)
      timeVariation(control, pollutant = c("no", "no2", "o3"), normalise = TRUE )
      dev.off()
    }
  }
}




## 2.2 Graficas de tendencia - ANUAL c/boxplot ####


for(i in 1:length(pollutant)){

  data <- pollutant[[i]] 
  data <- data[, -1] %>% 
    melt( id = "date") 
  levels(data$variable) <- sitios[which(sitios$Estacion %in% data$variable), 2]
  

    png(paste("comp_mes_",pollutant_names[i], ".png", sep=""), width = 700, height = 800) 
  data %>% 
    #mutate(year = year(date)) %>% 
    mutate(mes = month(date)) %>%
    #mutate(dia = day(date)) %>%
    #mutate(hora = hour(date)) %>%
    #filter(cluster == "D") %>%
    #filter(mes =="8") %>%
    ggplot(aes(x= factor(mes), y = value)) + 
    geom_boxplot(na.rm = TRUE, coef = 100, outlier.colour = NA ) + #  col =  force the whiskers to extend to the max and min values
    facet_wrap( ~variable, ncol =  4 ) +
    labs( x = "", y = "",
          title = paste( pollutant_names[i], sep = "" )) + 
    theme_bw()
  dev.off()
}



## 2.3 Graficas de tendencia por cluster - ANUAL c/ medianas ####


for(i in 1:length(pollutant)){
  
  data <- pollutant[[i]] 
  data <- data[, -1] %>% 
    melt( id = "date") 
  levels(data$variable) <- sitios[which(sitios$Estacion %in% data$variable), 2]
  

  png(paste("comp_mes_mediana_",pollutant_names[i], ".png", sep=""), width = 700, height = 800) 
  data %>% 
    #mutate(year = year(date)) %>% 
    mutate(mes = month(date)) %>%
    #mutate(dia = day(date)) %>%
    #mutate(hora = hour(date)) %>%
    #filter(cluster == "D") %>%
    #filter(mes =="8") %>%
    group_by(variable, mes) %>%
    summarise( mediana = median (value, na.rm = TRUE )) %>%
    ggplot(aes(x = mes, y = mediana)) + 
    geom_point(na.rm = TRUE) + 
    geom_line(na.rm = TRUE) +
    facet_wrap( ~variable, ncol =  4 ) +
    labs( x = "", y = "",
          title = paste( pollutant_names[i], sep = "" )) + 
    scale_x_continuous( breaks = c(1, 4, 8, 12)) +
    theme_bw()
  dev.off()
}



## 2.4 Graficas de tendencia >> ANUAL c/ medianas y cuartiles ####

for(i in 1:length(pollutant)){
  data <- pollutant[[i]] 
  data <- data[, -1] %>% 
    melt( id = "date") 
  levels(data$variable) <- sitios[which(sitios$Estacion %in% data$variable), 2]
 
  png(paste("comp_mes_cuartil_",pollutant_names[i], ".png", sep=""), width = 600, height = 500) 
  data %>% 
    mutate(mes = month(date)) %>%
    group_by(variable, mes) %>%
    summarise( q1 = quantile(value, probs= 0.25, na.rm = TRUE),
               q2 = median (value, na.rm = TRUE ),
               q3 = quantile(value, probs= 0.75, na.rm = TRUE)) %>%
    ggplot(aes(x = mes, y = q2)) + 
    geom_point(na.rm = TRUE) + 
    geom_line(na.rm = TRUE) +
    geom_crossbar(aes( ymin = q1 , ymax = q3 ), 
                  alpha = 0.3, 
                  fill = "red",
                  col = "transparent") + 
    facet_wrap( ~variable, ncol =  4 ) +
    labs( x = "", y = "",
          title = paste( pollutant_names[i], sep = "" )) + 
    scale_x_continuous( breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +
    theme_bw()
  
  dev.off()
}




## 2.5 Grafico de tendencia >> dia c/ medianas y cuartiles ####


for(i in 1:length(pollutant)){
  
  data <- pollutant[[i]] 
  data <- data[, -1] %>% 
    melt( id = "date") 
  levels(data$variable) <- sitios[which(sitios$Estacion %in% data$variable), 2]
  

  data <- data %>% 
      mutate( dia = wday(date),
              hora = hour(date)) %>%
      group_by(variable, dia, hora) %>%
      summarise( q1 = quantile(value, probs= 0.25, na.rm = TRUE),
                 q2 = median (value, na.rm = TRUE ),
                 q3 = quantile(value, probs= 0.75, na.rm = TRUE)) 
  #data$semana <- factor(data$dia, labels = c("Lun", "Mart", "Mierc", "Juev", "Viern", "Sab", "Dom") )
  data$fecha <- paste("2018 10", data$dia, data$hora, "0", sep = " ")
  data$fecha <- as.POSIXct(data$fecha, format= "%Y %m %d %H %M" )

  png(paste("comp_dia_cuartil_",pollutant_names[i], ".png", sep=""), width = 600, height = 800) 
  
  data %>% 
    ggplot(aes(x = fecha, y = q2)) + 
    geom_point(na.rm = TRUE, 
               col = "royalblue4", 
               fill = "royalblue4", 
               alpha = 0.8) + 
    geom_line(na.rm = TRUE, col = "royalblue3") +
    geom_crossbar(aes( ymin = q1 , ymax = q3 ), alpha = 0.3, 
                  col = "transparent", 
                  fill = "royalblue4") + 
    facet_wrap( ~variable, ncol =  4 ) +  #, scales = "free_y"
    labs( x = "", y = "",
          title = paste( pollutant_names[i], sep = "" )) + 
    scale_x_datetime( date_breaks = "1 day", date_labels = "%a") +
    theme_bw() +
    theme(axis.title = element_text(size = rel(0.8)),
          axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1) )
  
  dev.off()
}
  

## 2.6 Grafico de tendencia >> dia solo con medianas (sin cuartiles) ####


for(i in 1:7){
  
  data <- pollutant[[i]] 
  data <- data[, -1] %>% 
    melt( id = "date") 
  levels(data$variable) <- sitios[which(sitios$Estacion %in% data$variable), 2]
  
  
  data <- data %>% 
    mutate( dia = wday(date),
            hora = hour(date)) %>%
    group_by(variable, dia, hora) %>%
    summarise(q2 = mean (value, na.rm = TRUE )) 
  #data$semana <- factor(data$dia, labels = c("Lun", "Mart", "Mierc", "Juev", "Viern", "Sab", "Dom") )
  data$fecha <- paste("2018 10", data$dia, data$hora, "0", sep = " ")
  data$fecha <- as.POSIXct(data$fecha, format= "%Y %m %d %H %M" )
  
  png(paste("comp_dia_media_",pollutant_names[i], ".png", sep=""), width = 600, height = 800) 
  
  data %>% 
    ggplot(aes(x = fecha, y = q2)) + 
    geom_point(na.rm = TRUE, 
               col = "royalblue4", 
               fill = "royalblue4", 
               alpha = 0.8) + 
    geom_line(na.rm = TRUE, col = "royalblue3") +
    facet_wrap( ~variable, ncol =  4 ) +  #, scales = "free_y"
    labs( x = "", y = "",
          title = paste( pollutant_names[i], sep = "" )) + 
    scale_x_datetime( date_breaks = "1 day", date_labels = "%a") +
    theme_bw() +
    theme(axis.title = element_text(size = rel(0.8)),
          axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1) )
  
  dev.off()
}
