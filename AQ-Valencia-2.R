# Sol Represa
# AQ Valencia
# 20/11/2018



# Objetivo: 
# generar base de datos en SGLite
# para poder consultar eficientemente 
# los datos de las estaciones terrestres


library(sqldf)


###############################################################

## 1) Crear Base de datos "Generalitat.sqlite" ####

###############################################################

# Parto de los archivos de estaciones a generar una unica base con SQLite


directorio = "C:\\Users\\narep\\Desktop\\SOL\\AQ-Valencia\\datos"

db <- dbConnect(SQLite(), dbname= "Generalitat.sqlite")   #opens a connection to the database

id <- dir(directorio, pattern = ".csv") 

for (i in 1:length(id)){
  estacion.csv <- read.csv(paste(directorio, "\\",
                                 id[i], sep = ""), 
                           header=TRUE, sep=",", dec= ".", 
                           stringsAsFactors = FALSE, encoding = "UTF-8",
                           na.strings = "NA")    # Read csvs
  estacion.csv$date <- as.POSIXct(estacion.csv$date, format= "%Y-%m-%d %H:%M:%S", tz = "GMT")
  dbWriteTable(conn = db, 
               name = substring(id[i], 1, nchar(id[i])-4), 
               estacion.csv, 
               append = T, 
               row.names = F)   # Write to database
}

#Agregar tabla de contaminantes medidos por estaciones: estaciones_contaminantes.csv

est.cont <- read.csv("C:/Users/narep/Desktop/SOL/AQ-Valencia/estaciones_contaminantes.csv")
dbWriteTable(conn = db, 
             name = "Estaciones", 
             est.cont, 
             append = T, 
             row.names = F)   # Write to database


dbListTables(db)                 # The tables in the database

dbListFields(db, "X03009006")       # The columns in a table
dbReadTable(db, "X03009006")        # The data in a table

dbDisconnect(db)   # desconectar DB





