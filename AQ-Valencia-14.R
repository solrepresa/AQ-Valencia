### Sol Represa
### 01/02/2019
### AQ - Valencia
# Modificado 8/8/19 La Plata, Argentina


# Objetivo:
# Analisis de mediciones meterologicas y calidad de aire
# Continua AQ-Valencia-13.R



library(ggplot2)
library(reshape2)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(openair)
library(classInt)


# # # # # # # # # # # # # # # # # # # # 

## Abrir estaciones

# # # # # # # # # # # # # # # # # # # # 

meteo <- readRDS("estac_ut_con_meteo.Rds")
esta_meteo <- readRDS("meteo_names.Rds")
pollutant_names <- readRDS("pollutant_names.Rds")


sitios <- read.csv("estaciones_utiles.csv")
sitios <- data.frame( Estacion = levels(sitios$Estacion), 
                      Abrev = c("Pla", "Raba", "Beni",  "Elx", "Elda", "Alcora",  "Alma", "Ben", "Bur", "Peny", "Grau",
                                "Pobla", "SJ", "VCid", "Viver", "Zorita", "Alba", "Buniol", "Caude", "Quart", "Sagunt",
                                "Pista", "Poli", "Moli", "Villar"))

esta_meteo <- sitios[which(sitios$Estacion %in% esta_meteo), 2]


# # # # # # # # # # # # # # # # # # # # 

## 1 - Wind rose ####

# # # # # # # # # # # # # # # # # # # # 

vientos <- data.frame()

# # 1.0 Crear breaks para vientos

for( i in 1:length(meteo)){
  tabla <- meteo[[i]]
  variables <- names(tabla)
  
  if ("Veloc" %in% variables | "Direc." %in% variables){
    q <- match("Veloc", variables)
    names(tabla)[q] <- "ws"
    vien <- tabla$ws
    vientos <- rbind(vientos, vien)

  }
}

vientos <- t(vientos)
vientos <- data.frame(vientos)
vientos$id <- seq(1, nrow(vientos))

vientos <- melt(vientos, id = "id")

#i = 1

clase <- classIntervals(sample(vientos$value, size = 5000), 5, style = "jenks")
breaks <- clase$brks

#breaks <- c(0.0,  1.2,  2.7,  4.7,  8.0, 15.3, 25)


# 1.1 Armar ROSAS VIENTOS

## COLORES

#display.brewer.all()
#display.brewer.pal(n = 4, name = "RdYlBu")
#brewer.pal(n = 4, name = "RdYlBu")

col <- c("#74ADD1" , "#E0F3F8", "#FEE090", "#F46D43" )


# type="daylight" // type="season"
# hemisphere = "southern"



for( i in 1:length(meteo)){
  tabla <- meteo[[i]]
  variables <- names(tabla)
  
  if ("Veloc" %in% variables | "Direc." %in% variables){
    p <- match("Direc.", variables)
    q <- match("Veloc", variables)
    names(tabla)[p] <- "wd"
    names(tabla)[q] <- "ws"
    
    png(paste("rosa_", esta_meteo[i], ".png", sep=""), width = 320, height = 300)
    windRose(tabla, angle = 45, paddle = FALSE, 
             dig.lab = 3 , 
             cols = col, 
             key.position = "right",
             breaks = breaks,
             main = esta_meteo[i])
    dev.off()
    
  }
}



# por estaciones del year
for( i in 1:length(meteo)){
  tabla <- meteo[[i]]
  variables <- names(tabla)
  
  if ("Veloc" %in% variables | "Direc." %in% variables){
    p <- match("Direc.", variables)
    q <- match("Veloc", variables)
    names(tabla)[p] <- "wd"
    names(tabla)[q] <- "ws"
    
    png(paste("rosa_",esta_meteo[i], "_year.png", sep=""), width = 420, height = 400)
    windRose(tabla, angle = 45, paddle = FALSE, 
             dig.lab = 3 , 
             cols = col, 
             key.position = "right",
             type = "season", 
             breaks = breaks,
             main = esta_meteo[i])
    dev.off()
    
  }
}

# # # # # # # # # # # # # # # # # # # # 

## 2- Rosa pollution ####

# # # # # # # # # # # # # # # # # # # # 

# # 2.0 Crear breaks para cada contaminantes

pollutant <- readRDS("pollutant_ut.Rds")

#i = 1

breaks <- list()
for(i in 1:length(pollutant)){
  
  tabla <- pollutant[[i]]
  tabla <- tabla[,-1]
  
  tabla <- tabla %>%   
    melt( id = "date") 
  
  clase <- classIntervals(sample(tabla$value, size = 5000), 5, style = "jenks")
  breaks[[i]] <- clase$brks
}



# # 2.1 Anual   # # # # # 



for( i in 1:length(meteo)){
  tabla <- meteo[[i]]
  variables <- names(tabla)
  
  if ("Veloc" %in% variables | "Direc." %in% variables){
    p <- match("Direc.", variables)
    q <- match("Veloc", variables)
    names(tabla)[p] <- "wd"
    names(tabla)[q] <- "ws"
    
    for( j in 1: length(pollutant_names) ){
      if(pollutant_names[j] %in% variables){
        png(paste("rosa_",esta_meteo[i], "_", pollutant_names[j], "_year.png", sep=""), width = 350, 
            height = 310)
        pollutionRose(tabla, normalise =TRUE, 
                      annotate = FALSE,
                      pollutant = pollutant_names[j], 
                      angle = 45, 
                      breaks = breaks[[j]],
                      main = esta_meteo[i])
        dev.off()
      }
    }
  }
}



# # 2.2 Season   # # # # # 


for( i in 1:length(meteo)){
  tabla <- meteo[[i]]
  variables <- names(tabla)
  
  if ("Veloc" %in% variables | "Direc." %in% variables){
    p <- match("Direc.", variables)
    q <- match("Veloc", variables)
    names(tabla)[p] <- "wd"
    names(tabla)[q] <- "ws"
    
     for( j in 1: length(pollutant_names) ){
       if(pollutant_names[j] %in% variables){
         png(paste("rosa_",esta_meteo[i], "_", pollutant_names[j], ".png", sep=""), 
             width = 340, height = 300)
         pollutionRose(tabla, 
                       pollutant = pollutant_names[j], 
                       type = "season", 
                       annotate = FALSE,
                       grid.line = 0.5,
                       normalise = TRUE, 
                       angle = 45,
                       breaks = breaks[[j]],
                       main = esta_meteo[i])
         dev.off()
       }
    }
  }
}


# # # # # # # # # # # # # # # # # # # # 

## 3 - Tendencia por viento ####

# # # # # # # # # # # # # # # # # # # # 



trend <- data.frame()


for( i in 1:length(meteo)){
  tabla <- meteo[[i]]
  variables <- names(tabla)
  
  if ("Veloc" %in% variables | "Direc." %in% variables){
    p <- match("Direc.", variables)
    q <- match("Veloc", variables)
    names(tabla)[p] <- "wd"
    names(tabla)[q] <- "ws"
    
    for( j in 1: length(pollutant_names) ){
      if(pollutant_names[j] %in% variables){
        png(paste("trend_wind_",pollutant_names[j], "_", esta_meteo[i], ".png", sep=""),
            width = 730, height = 440)
        Theil <- TheilSen(tabla, 
                 pollutant = pollutant_names[j], 
                 slope.percent = TRUE,
                 type = "wd", 
                 avg.time = "month", 
                 deseason = TRUE,
                 main = esta_meteo[i])
        dev.off()
        
        #tabla
        valor <- data.frame(esta_meteo[i],pollutant_names[j], Theil$data$res2)
        names(valor)[1] <- "estacion"
        names(valor)[2] <- "contaminantes"
        trend <- rbind(trend, valor)
        
      }
    }
  }
}

trend <- trend[,c(1,2,3,4,14,18:22)]
trend[,5:10] <- round(trend[,5:10], digits = 2)


#write.csv(trend, "tend_wind.csv", row.names = FALSE)


# # # # # # # # # # # # # # # # # # # # 

## 4 - Analisis vientos ####

# # # # # # # # # # # # # # # # # # # # 

vientos <- data.frame()


for( i in 1:length(meteo)){
  tabla <- meteo[[i]]
  variables <- names(tabla)
  
  if ("Veloc" %in% variables | "Direc." %in% variables){
    q <- match("Veloc", variables)
    names(tabla)[q] <- "ws"
    vien <- data.frame( tabla$estacion,tabla$ws)
    vientos <- rbind(vientos, vien)
    
  }
}

names(vientos) <- c("estacion", "ws")

a <- vientos %>% 
  group_by(estacion) %>%
  summarise(mean = mean(ws, na.rm = TRUE),
            max = max(ws, na.rm = TRUE))

vientos$ws <- vientos$ws*60/1000
