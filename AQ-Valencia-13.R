### Sol Represa
### 01/02/2019
### AQ - Valencia

# Objetivo: Generar lista con las mediciones meteorologicas


library(sqldf)
library(dplyr)


# # # # # # # # # # # # # # # # # # # # # 

# 0. Abrir estaciones de interés de base de datos ####

# # # # # # # # # # # # # # # # # # # # # 

estaciones <- read.csv("estaciones_utiles.csv")
fact_names <- c("H.Rel.", "Temp.", "Veloc", "Vel_Max", "Direc.")


date_initial = "2008-01-01 00:00:00"
date_final = "2018-07-31 23:00:00"


db <- dbConnect(SQLite(), dbname= "Generalitat.sqlite") # Abro base de datos

#Lista de tablas
#dbListTables(db)  

meteo <- list()
fecha <- data.frame(id = seq(1, 92760), 
                    date = seq(from= as.POSIXct(date_initial, tz = "GMT" ), 
                               to= as.POSIXct(date_final, tz = "GMT" ), by = "hour"))  #serie completa
j = 1
n = length(levels(estaciones$Estacion))
esta_meteo <- c()

for( i in 1: n ){
  variables <- dbListFields(db,  levels(estaciones$Estacion)[i])  # Show variables from table
  if(any(fact_names == variables)){
      p1 <- dbReadTable(db, levels(estaciones$Estacion)[i])  # Lee Tabla 
      tabla <- cbind(fecha, p1[,-c(1,2)])
      meteo[[j]] <- tabla
      esta_meteo <- c(esta_meteo, levels(estaciones$Estacion)[i])
      j = j + 1
  }
}

print(esta_meteo)

#saveRDS(meteo, file ="estac_ut_con_meteo.Rds")
#saveRDS(esta_meteo, file = "meteo_names.Rds")

dbDisconnect(db)



