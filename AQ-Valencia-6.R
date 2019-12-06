# Sol Represa
# AQ Valencia
# 11/12/2018


library(sqldf)
library(dplyr)



# Objetivo: Generar lista desde la base de datos con los contaminantes de interes y
# estaciones seleccionadas

# script basado es AQ-Valencia-3.R
# como diferencia, acá se trabajará solo con algunas estaciones del conjunto



############################################################

# Generar lista solo con las estaciones de interés

############################################################


# Abro base de datos
db <- dbConnect(SQLite(), dbname= "Generalitat.sqlite") 
#src_dbi(db)
alltables  <- dbListTables(db)  

# Parámetros de mi data set
date_initial = "2008-01-01 00:00:00"
date_final = "2018-07-31 23:00:00"
pollutant <- c("CO", "NO", "NO2", "NOx","O3","PM10",
               "PM2.5","SO2" )

estaciones <- dbGetQuery( db, "SELECT * FROM Estaciones_utiles" )  # abrir tabla Estaciones

for (j in pollutant){  
  estaciones_cont <- estaciones %>% filter(Contaminante == j) 
  
  if(!nrow(estaciones_cont) == 0 ){         # Control: si no se encuentra ese contaminante en la base de datos
    tabla <- data.frame(id = seq(1, 92760), 
                        date = seq(from= as.POSIXct(date_initial, tz = "GMT" ), 
                                   to= as.POSIXct(date_final, tz = "GMT" ), by = "hour"))  #serie completa
    for (i in 1: nrow(estaciones_cont)){
      sql_sentence = paste("SELECT [", j, "] FROM ", estaciones_cont[i,1], sep="" )
      p1 = dbGetQuery( db, sql_sentence )
      names(p1) <- estaciones_cont[i,1] 
      tabla <- cbind(tabla, p1)
    }
    assign(j, tabla)  #cambiar nombre de tabla en memoria
  }
}


dbDisconnect(db)
rm(estaciones, estaciones_cont, p1, alltables, sql_sentence, db, tabla)


pollutant <- list(NO, NO2, NOx, O3, PM10,
                  PM2.5, SO2)
pollutant_names <- c("NO", "NO2", "NOx","O3","PM10",
                     "PM2.5","SO2")
