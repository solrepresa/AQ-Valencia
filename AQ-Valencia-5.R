# Sol Represa
# AQ Valencia
# 11/12/2018
# modificado 07/08/2019

library(dplyr)
library(sqldf)
library(lubridate)

# Objetivo: seleccionar estación que presente mayor numero de mediciones y variables


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 0 - Cargar variables

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

med_pol_dia <- readRDS("medias_diarias_todas.Rds")  # medias diarias con 50% de datos diarios
med_pol_mes <- readRDS("medias_mensuales_todas.Rds")  # medias mensuales con + del 30% de datos

pollutant_names <- c("CO","NO", "NO2", "NOx","O3","PM10",
                     "PM2.5","SO2")


sitios <- read.csv("/home/usuario/Sol/AQ-Valencia/estaciones-valencia-todas.csv", sep = ",") # las 60 estaciones con coordenadas

############################################################

# 1 - Qué % de datos hay de las distintas variables en cada estación

############################################################

source("C:/Users/narep/Desktop/SOL/AQ-Valencia/fuction/plot_missing.R")

plot_missing(med_pol_dia, pollutant_names, 2)  # medias diarias con 50% de datos diarios
plot_missing(med_pol_mes, pollutant_names, 2)  # medias mensuales con + del 30% de datos

# Faltan muchos datos


############################################################

# 2 - Cuáles estaciones tienen mayor cantidad de mediciones para todos los contaminantes

############################################################


x = med_pol_dia

missing_total <- data.frame()
for (i in 1: length(x)){
  tabla <- x[[i]]
  tabla_na <- data.frame( Estaciones = names(tabla[,3:length(tabla)]), 
                          N = colSums(is.na(tabla[,3:length(tabla)])))
  tabla_na$Propor_NA <- round(tabla_na$N/nrow(tabla), digits = 3 )
  tabla_na$Porcent_NA <- tabla_na$Propor_NA * 100
  salida <- data.frame(Contaminante = pollutant_names[i], 
                       Estacion = tabla_na$Estaciones, 
                       Porc_NA = tabla_na$Porcent_NA,
                       Porc_COMP = 100 - tabla_na$Porcent_NA)
  missing_total <- rbind(missing_total, salida)
}


# Grafica Missing (3) ####
# grafica booleana - qué contaminante se miden en cada estacion

missing_total %>% 
  filter(Porc_COMP > 30 ) %>% 
  group_by(Estacion, Contaminante) %>%
  ggplot(data = .,
         aes(x = Estacion,
             y = Contaminante)) +
  geom_raster() +
  theme_minimal() + 
  theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
  labs(x = "", y = "")


#####################################################################

###  Seleccionar estaciones #############

#####################################################################


x = med_pol_dia    # esta base tiene limpios los datos que no cumplen menso de 12hs diarios

missing_tot_anual <- data.frame()
for (i in 1: length(x)){
  tabla <- x[[i]]
  tabla$date <- as.Date(tabla$date)
  tabla <- melt(tabla, id = "date")
  tabla$year <- year(tabla$date)
  tabla_year_na <- tabla %>% 
    group_by(variable, year) %>% 
    summarise(N_NA = sum(is.na(value)),
              N = n())
  tabla_year_na$Propor_NA <- round(tabla_year_na$N_NA/tabla_year_na$N, digits =3)
  tabla_year_na$Porcent_NA <- tabla_year_na$Propor_NA * 100
  salida <- data.frame(Contaminante = pollutant_names[i], 
                       Estacion = tabla_year_na$variable, 
                       Year = tabla_year_na$year, 
                       Porc_NA = tabla_year_na$Porcent_NA,
                       Porc_COMP = 100 - tabla_year_na$Porcent_NA)
  missing_tot_anual <- rbind(missing_tot_anual, salida)
}


# obtenemos lista de estaciones
estaciones_comp <- missing_tot_anual %>% 
  filter(Porc_COMP > 75 ) %>%    #### 75% de datos completos al año 
  group_by(Estacion, Year)

estaciones_comp_anual <- estaciones_comp %>%
  group_by(Contaminante, Estacion) %>%
  summarize( n = n()) %>%
  filter( n > 6)                  #### mas de 6 años de datos

estaciones_utiles <- estaciones_comp_anual %>% 
  group_by(Estacion) %>% 
  summarise(n = n()) %>%
  filter( n > 3)                    ### que midan varios contaminantes (más de 3)

estaciones_utiles  <- merge(estaciones_utiles, estaciones_comp_anual, by="Estacion")
estaciones_utiles <- estaciones_utiles[,-c(2,4)]

# Estan todas dentro de las 60 con coordenadas? <<< SI ! :)
a <- estaciones_utiles[which(estaciones_utiles$Estacion %in% sitios$Estacion),]


############################################################

# 3 - Guardar el vector con las estaciones utiles 

############################################################


# 3.1 Guardar el csv para despues acceder a SQL ####
#write.csv(estaciones_utiles, "estaciones_utiles.csv", row.name =FALSE)



# 3.2 Agregar en el data set ####

db <- dbConnect(SQLite(), dbname= "Generalitat.sqlite")   #abrir database

#dbRemoveTable(db, "Estaciones_utiles")   

dbWriteTable(conn = db, 
             name = "Estaciones_utiles", 
             estaciones_utiles, 
             append = T, 
             row.names = F)   # Write to database

dbListTables(db)   # Chequeamso que se guardo en el database

dbDisconnect(db)   # desconectar DB


############################################################

# 4 - Guardar el vector con las estaciones utiles 

############################################################

