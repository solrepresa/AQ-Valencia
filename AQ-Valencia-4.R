# Sol Represa
# AQ Valencia
# 10/12/2018

# Objetivo: 
# Análisis de datos faltantes

library(ggplot2)
library(reshape2)


source("AQ-Valencia-3.R")


############################################################

## Analisis de datos faltantes ####

###########################################################


# 1. Visualizar % Datos Faltantes por Estaciones en todo el período ####

source("fuction/plot_missing.R")

plot_missing(pollutant, pollutant_names, 3)
plot_missing(fact, fact_names, 3)  

#al armar 2 listas distintas no tengo las mismas estaciones ¡ojo!

############################################################

# 2. Limpieza de dias donde haya alta proporción de datos faltantes ####

############################################################

source("fuction/medias_diarias.R")

l <- medias_diarias(pollutant, 3)

#saveRDS(l, file = "medias_diarias_todas.Rds")


# Grafica Missing (2) ####
# grafica booleana de dónde faltan datos 

tabla[,3:length(tabla)] %>% 
  is.na %>%
  melt %>%
  ggplot(data = .,
         aes(x = Var2,
             y = Var1)) +
  geom_raster(aes(fill = value)) +
  scale_fill_grey(name = "",
                  labels = c("Present","Missing")) +
  theme_minimal() + 
  theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
  labs(x = "",
       y = "Observations / Date")



############################################################

## 3. Visualizacion de datos faltantes por mes ####

############################################################


g <- list()

for(i in 1:length(l)) {
  tabla <- l[[i]]
  #¿cuantos datos faltan por mes?
  tabla$date <- as.Date(tabla$date, tz = "GMT" )
  NA_mes <- aggregate( is.na(tabla[,3:length(tabla)]) , list(format(tabla$date, "%Y-%B")), FUN = sum ) 
  Count_mes <- aggregate( tabla[,3:length(tabla)] , list(format(tabla$date, "%Y-%B")), FUN = NROW ) 
  tabla_na_mes <- cbind(NA_mes[1], round(NA_mes[-1]/Count_mes[-1] * 100, digits = 0))
  names(tabla_na_mes)[1] <- "date"
  
  tabla_na_mes_MELT <- melt(tabla_na_mes, by= date)
  tabla_na_mes_MELT$date <- as.Date(paste(tabla_na_mes_MELT$date, "-1", sep=""), format = "%Y-%B-%d", tz= "GMT" )
  tabla_na_mes_MELT$value <- 100 - tabla_na_mes_MELT$value
  
  g[[i]] <- ggplot(data = tabla_na_mes_MELT, aes(x = variable, y = date)) +
    geom_raster(aes(fill = value)) +
    scale_fill_distiller(palette = "Purples" , name = "", trans="reverse") +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "",
         y = "", 
         title = paste("% Data Medida - ", pollutant_names[i]))
  png(paste("missing_", pollutant_names[i] , ".png", sep="") , width = 1150, height = 500)
  print(g[[i]])
  dev.off()

}

rm(g)  # g guarda todos los plots con el % de data medida en cada estacion en cada mes



############################################################

## 4. Base de datos faltantes por dia ####

############################################################

g <- list()
k <- list()

for(i in 1:length(pollutant)) {
  tabla <- pollutant[[i]]
  
  #¿cuantos datos faltan por mes? en %
  NA_mes <- aggregate( is.na(tabla[,3:length(tabla)]) , list(format(tabla$date, "%Y-%m")), FUN = sum )
  Count_mes <- aggregate( tabla[,3:length(tabla)] , list(format(tabla$date, "%Y-%m")), FUN = NROW ) 
  tabla_na_mes <- cbind(NA_mes[1], round(NA_mes[-1]/Count_mes[-1] * 100, digits = 0))
  names(tabla_na_mes)[1] <- "date"
  
  #¿cuantos datos faltan por dia? en %
  NA_dia <- aggregate( is.na(tabla[,3:length(tabla)]) , list(format(tabla$date, "%Y-%m-%d")), FUN = sum ) 
  NA_dia[,2:length(NA_dia)] <- round(NA_dia[,2:length(NA_dia)]/24 * 100, digits = 0)
  names(NA_dia)[1] <- "date"
  
  g[[i]] <- tabla_na_mes
  k[[i]] <- NA_dia
  
}

saveRDS(g, file="NA_mensuales.Rds")
saveRDS(k, file="NA_diarios.Rds")


rm(g,k)

############################################################

## 5. Limpieza de meses donde haya alta proporción de datos faltantes ####

############################################################



# # CRITERIO LIMPIEZA para medias mensual  # # #

# B) Faltan más del 30% del mes  
# Se construye la media mensual con las medias diarias. 

# # # # # # # # # # # # # # # # # # # # # # # # #


# A continuación:
# Genera medias mensuales a partir de una lista con todas las medias diarias de las concentraciones


x = l
k <- list()

for(i in 1:length(x)) {
  tabla <- x[[i]]
  #¿cuantos datos faltan por mes?
  tabla$date <- as.Date(tabla$date, tz = "GMT" )
  NA_mes <- aggregate( is.na(tabla[,3:length(tabla)]) , list(format(tabla$date, "%Y-%B")), FUN = sum ) 
  Count_mes <- aggregate( tabla[,3:length(tabla)] , list(format(tabla$date, "%Y-%B")), FUN = NROW ) 
  tabla_na_mes <- cbind(NA_mes[1], round(NA_mes[-1]/Count_mes[-1] * 100, digits = 0))
  names(tabla_na_mes)[1] <- "date"
  
  # codificar 
  tabla_na_mes[tabla_na_mes == 0] <- 1
  tabla_na_mes[tabla_na_mes > 30] <- 0    #criterio = más de 30% NA al mes es removido el mes
  tabla_na_mes[tabla_na_mes != 0] <- 1
  names(tabla_na_mes)[1] <- "date"
  
  # agregar en media diaria
  mean_mes <- aggregate( tabla[,3:length(tabla)] , list(format(tabla$date, "%Y-%B")), FUN = mean ) 
  names(mean_mes)[1] <- "date"
  
  # obtener tabla con fechas que cumplan criterio 
  tabla_mean_mes <- mean_mes[,2:length(mean_mes)] * tabla_na_mes[,2:length(tabla_na_mes)]
  tabla_mean_mes[tabla_mean_mes == 0] <- NA
  k[[i]] <- data.frame( date = mean_mes[,1], tabla_mean_mes) 
}


saveRDS(k, file="medias_mensuales_todas.Rds")  #TODAAAAAS (no las 60!)
