# Sol Represa
# AQ Valencia
# 07/08/2019

# Objetivo: Análisis de datos faltantes
# Este archivo presenta modificaciones respecto a AQ-Valencia-4
# porque no coincidía el listado de estaciones con coordenadas de aquellas con datos.
# No pude obtener coordenadas de estas estaciones <<<<

library(ggplot2)
library(reshape2)


source("AQ-Valencia-3.R") # Todas las estacioones con datos
st =3

sitios <- read.csv("/home/usuario/Sol/AQ-Valencia/estaciones-valencia-todas.csv", sep = ",") #todas las estaciones con coordenadas


missing_total <- data.frame()

# generar data.frame de la lista pollutant
for (i in 1: length(pollutant)){
  tabla <- pollutant[[i]]
  tabla_na <- data.frame( Estaciones = names(tabla[,st:length(tabla)]), 
                          N = colSums(is.na(tabla[,st:length(tabla)])))
  tabla_na$Propor_NA <- round(tabla_na$N/nrow(tabla), digits = 3 )
  tabla_na$Porcent_NA <- tabla_na$Propor_NA * 100
  salida <- data.frame(contaminante = pollutant_names[i], 
                       estaciones = tabla_na$Estaciones, 
                       Porc_NA = tabla_na$Porcent_NA,
                       Porc_COMP = 100 - tabla_na$Porcent_NA)
  missing_total <- rbind(missing_total, salida)
}

# tabla va a contener todas las estaciones de las q tengo coordendas
tabla <- missing_total[which(missing_total$estaciones %in% sitios$Estacion),]
tabla$estaciones <- as.character(tabla$estaciones)
tabla$estaciones <- factor(tabla$estaciones)  #60
# hay 9 estaciones de las que NO tengo datos!
a <- sitios[-which(sitios$Estacion %in% missing_total$estaciones),]
a$Estacion # <<<< QUITAR del mapa! 


# tabla va a contener todas las estaciones de las q tengo coordendas
tabla2 <- missing_total[-which(missing_total$estaciones %in% sitios$Estacion),]
tabla2$estaciones <- as.character(tabla2$estaciones)
tabla2$estaciones <- factor(tabla2$estaciones)  #20


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Grafica con valores faltantes de las 60 estaciones por contaminantes

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

ggplot(data = tabla, aes(y = contaminante, x = estaciones)) +
  geom_raster(aes(fill = Porc_COMP)) +
  scale_fill_distiller(palette = "Purples" , name = "", trans="reverse", limits = c(100,0)) +
  theme_minimal() + 
  theme(axis.text.x  = element_text(angle=45, hjust= 1)) + 
  labs(x = "",
       y = "",
       title = "Porcentaje de datos")




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

### 2) Medias diarias para analizar % de datos faltantes por mes ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


source("fuction/medias_diarias.R")

l <- medias_diarias(pollutant, 3)


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
  
  tabla_na_mes_MELT <-tabla_na_mes_MELT[which(tabla_na_mes_MELT$variable %in% sitios$Estacion),]
  
  tabla_na_mes_MELT$date <- as.Date(paste(tabla_na_mes_MELT$date, "-1", sep=""), format = "%Y-%B-%d", tz= "GMT" )
  tabla_na_mes_MELT$value <- 100 - tabla_na_mes_MELT$value
  
  g[[i]] <- ggplot(data = tabla_na_mes_MELT, aes(x = variable, y = date)) +
    geom_raster(aes(fill = value)) +
    scale_fill_distiller(palette = "Purples" , name = "", trans="reverse") +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, hjust=1)) + 
    labs(x = "",
         y = "", 
         title = paste("% Data Medida - ", pollutant_names[i]))
  png(paste("missing_", pollutant_names[i] , ".png", sep="") , width = 980, height = 330)
  print(g[[i]])
  dev.off()
  
}

rm(g)  # g guarda todos los plots con el % de data medida en cada estacion en cada mes

