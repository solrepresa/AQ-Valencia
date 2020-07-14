####### Calidad del Aire en Comunitat Valenciana
####### 10/10/2018 Valencia, Spain
####### Sol Represa
####### Archivo 2


# Objetivo: 
# - Limpiar los txt de las estaciones de monitoreo
# - Generar bases de datos con las variables por estaciones y por variable medida


library(lubridate)
library(ggplot2)



############################################################

# 0 - Nombre de estaciones ####

###########################################################

# La estaciones en Valencia se fueron moviendo
# en cada emplazamiento recibieron distinto nombre
# A continuación una lista de las estaciones: 


id <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\datos\\MHEST\\estaciones", pattern = ".txt") 

estaciones <- data.frame()
for (i in 1:length(id)){
  datos <- read.csv(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\datos\\MHEST\\estaciones\\",
                            id[i], sep = ""), colClasses = "character", 
                      header=FALSE, sep=";", stringsAsFactors = TRUE, encoding = "UTF-8")
  estaciones <- rbind(estaciones, datos)
}

estaciones <- estaciones[!duplicated(estaciones$V2),] #datos no repetidos

estaciones <- estaciones[order(estaciones$V2),]


# 

############################################################

# 1 - Limpiar los .txt de las bases de estaciones de monitoreo continuo ####

###########################################################

## ATENCION: hay ficheros que en la fila 3 tienen una fila blanco y otros q no
## en gral. son los ficheros del 2011 y algunos de 2008.
## Manualmente agregué la fila en blanco en todos los ficheros que faltaban.
## Estos ficheros también presentan una columna con "V" que son NA


id <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\datos\\MHEST", pattern = ".txt") 

for (i in 1:length(id)){
  file <- file(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\datos\\MHEST\\",
                     id[i], sep = "")) 
  txt <- readLines(file)  #leo el archivo
  encabezado <- readLines(file, n=3)
    # si linea 3 no es blanca, 
  if( encabezado[3] != "\t"){
    # incorporar linea blanca
    tx1 <- c(encabezado[-3], "\t", txt[(3):length(txt)]) 
    writeLines(tx1, file, sep="\n")
  }
  # Elimina los "V" 
  txt <- readLines(file)  #leo el archivo
  tx2  <- gsub(pattern = "V", replace = "", x = txt)
  writeLines(tx2, file, sep="\n")
  
  # Elimina los "flag" 
  txt <- readLines(file)  #leo el archivo
  tx3  <- gsub(pattern = "flag", replace = "", x = txt)
  writeLines(tx3, file, sep="\n")

  # Corregir los "eloc.máx" 
  txt <- readLines(file)  #leo el archivo
  tx2  <- gsub(pattern = "eloc.máx.", replace = "Vel_Max", x = txt)
  writeLines(tx2, file, sep="\n")
  
  # Corregir los "eloc." 
  txt <- readLines(file)  #leo el archivo
  tx2  <- gsub(pattern = "eloc.", replace = "Veloc", x = txt)
  writeLines(tx2, file, sep="\n")

  close(file)
}  




##########################################################################

# 2- Generar base con las variables medidas en las estaciones por año ####

###########################################################################

id <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\datos\\MHEST", pattern = ".txt") 

# Genera una data.frame con las estaciones, año de resgitro y variables que mide

data <- data.frame()
for (i in 1:length(id)){
  archivo <- paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\datos\\MHEST\\",
                   id[i], sep = "")
  datos <- read.delim(archivo, 
                      header=TRUE, sep="\t", dec=c(",", "."), skip = 3, 
                      stringsAsFactors = FALSE, row.names = NULL)
  datos <- datos[,!sapply(datos, function(x)all(is.na(x)))]
  estacion <- paste("X", substring(id[i], 6, 13), sep="")
  year <- substring(id[i], 14, 17)
  registro <- names(datos)
  unidades <- t(datos[1,])
  
  a <- data.frame(estacion, year, registro[-c(1,2)], unidades[-c(1,2)])
  data <- rbind(data, a)
}
rm(a,datos, estacion, registro, year)
names(data) <- c("Estacion", "Fecha", "Variable", "Unidades")

data[[1]] <- as.character(data[[1]])
data[[2]] <- as.character(data[[2]])
data[[3]] <- as.character(data[[3]])

# Para ver los niveles
# levels(data$Variable)
# levels(data[[3]])

## 3) Graficas para ver cuando hay registros :) 


data$Estacion <- factor(data$Estacion, 
                        levels = c( "X03009006", "X03014004", "X03014006", "X03014008", 
                                    "X03014009", "X03014010", "X03014012", "X03014013", "X03014014", 
                                    "X03031002", "X03065006", "X03065007", "X03066003",
                                    "X03089001", "X03099002", "X03105001", "X03122005", "X03133002",
                                    "X12005005", "X12009007", "X12028001", "X12032001", "X12040008", 
                                    "X12040009", "X12040010", "X12040015", "X12046001",
                                    "X12080007", "X12084003", "X12093004", "X12099001", "X12120001", 
                                    "X12124001", "X12126003", "X12127002", "X12129001",
                                    "X12138001", "X12138002", "X12140002", "X12141002", "X46010001", 
                                    "X46017002", "X46028001", "X46062001", "X46064001",
                                    "X46077006", "X46078004", "X46095001", "X46099001", "X46102002", 
                                    "X46109001", "X46111001", "X46116001", "X46131002",
                                    "X46136001", "X46181002", "X46184002", "X46190005", "X46202001", 
                                    "X46202002", "X46202003", "X46220003", "X46220009",
                                    "X46220010", "X46242001", "X46244003", "X46248001", "X46250030", 
                                    "X46250031", "X46250033", "X46250034", "X46250043",
                                    "X46250046", "X46250047", "X46250048", "X46250049", "X46250050", 
                                    "X46250051", "X46256001", "X46258001", "X46258004",
                                    "X46263999"),
                        labels = c( "Alcoi - Verge dels Lliris", "Renfe", "Alacant - El Pla",                
                                    "Alacant - Florida Babel", "Alacant - Rabassa", "Alacant  - Parc Mar Prov",           
                                    "Alacant  - AP ISM", "Alacant - AP T Frutero", "Alacant - AP D Pesquera",           
                                    "Benidorm", "Elx - Agroalimentari", "Elx - Parc de Bombers",           
                                    "Elda - Lacy", "Monover" , "Orihuela",                        
                                    "El Pinos",  "Sant Vicent del Raspeig", "Torrevieja",                      
                                    "L'Alcora", "Almassora - C. P. Ochando", "Benicassim" ,                     
                                    "Burriana", "Castelló - Penyeta" ,"Castello - Ermita" ,              
                                    "Castello - Grau" , "Castello - Patronat d'Esports","Cirat",                           
                                    "Morella", "Onda",  "Coratxar",                        
                                    "Sant Jordi" ,  "Torre Endomenech", "Vall d'Alba PM",                  
                                    "La Vall d'Uixo",   "Vallibona"  , "Vilafranca",                      
                                    "Vinaros Planta",  "Vinaros Plataforma" , "Viver",                           
                                    "Zorita"  , "Albalat dels Tarongers",  "Alzira",                          
                                    "Algar de Palancia"," Beniganim" , "UM Benimuslem",                   
                                    "Buñol - Cemex", "Burjassot - Facultats" , "Caudete de las Fuentes",          
                                    "Cortes de Pallas", "Quart de Poblet", "Cheste UM",                       
                                    "Chiva_UM", "L'Eliana" , "Gandia",                          
                                    "UM Godelleta",  "Oliva Unidad Movil (campaña)", "Ontinyent",                       
                                    "Paterna - CEAM", "La Pobla de Vallbona - La Vereda", "La Pobla Cap Horta",              
                                    "La Pobla Maravisa", "Sagunt - Port", "Sagunt - Nord",                   
                                    "Sagunt - CEA",  "Torrebaja" , "Torrent - El Vedat",                
                                    "Turis UM", "Valencia - Pista de Silla" , "Valencia - Nuevo Centro",         
                                    "Aragon", "Valencia - Linares", "Valencia - Vivers",               
                                    "Valencia - Politecnic",  "Valencia - Avd. Francia" , "Valencia - Moli del Sol",         
                                    "Valencia-Conselleria Meteo.",  "Valencia - Bulevard Sud", "Valencia - Albufera",               
                                    "Vilamarxant", "Villar del Arzobispo", "Villar - Tejeria 2",                
                                    "Zarra - EMEP" ))



#Ver todos los periodos de registros 
ggplot(data=data) + 
  geom_tile(aes(x=Fecha, y=Estacion), colour="grey", fill="hotpink4") + 
  theme_light() +
  ggtitle("Periodo de funcionamiento") 



#ver los fichero q tienen PM2.5
ggplot(data=data[which(data$Variable == "PM2.5"),]) + 
  geom_tile(aes(x=Fecha, y=Estacion), colour="grey", fill="lightcyan4") + 
  ggtitle(expression("Monitoreo de PM"[2.5])) + theme_light()


## Graficas explroatorias ##
variables = c("Direc.", "CO", "SO2", "O3", "NO", "NO2", "NOx", "Temp.", 
              "H.Rel.", "PM10", "PM2.5", "PST", "TCAB", "Veloc", "Vel_Max")

for(v in 1: length(variables)){
  
  variable <- variables[v]
  
  pdf(paste("monitoreo_",variable,".pdf", sep=""), paper="a4")
 
  data %>%
    filter (Variable == variable) %>%
    ggplot() +
    geom_tile(aes(x=Fecha,y=Estacion), colour="grey", fill="lightcyan4") + 
    ggtitle(paste("Monitoreo de ", variable , sep="")) + 
    theme_light()
  
  dev.off() 
  
}



## 4) Armar data.frame unicos con los txt de igual estacion

# Igual al punto 2

id <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\datos\\MHEST", pattern = ".txt") 

data <- data.frame()
for (i in 1:length(id)){
  archivo <- paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\datos\\MHEST\\",
                   id[i], sep = "")
  datos <- read.delim(archivo, 
                      header=TRUE, sep="\t", dec=c(",", "."), skip = 3, 
                      stringsAsFactors = FALSE, row.names = NULL, na.strings = c(" ", "NA"))
  datos <- datos[,!sapply(datos, function(x)all(is.na(x)))]

  estacion <- paste("X", substring(id[i], 6, 13), sep="")
  year <- substring(id[i], 14, 17)
  registro <- names(datos)
  unidades <- t(datos[1,])
  
  a <- data.frame(estacion, year, registro[-c(1,2)], unidades[-c(1,2)])
  data <- rbind(data, a)
}
rm(a,datos, estacion, registro, year, unidades, archivo)
names(data) <- c("Estacion", "Fecha", "Variable", "Unidades")


data[[1]] <- as.character(data[[1]])
data[[2]] <- as.character(data[[2]])
data[[3]] <- as.character(data[[3]])
data[[4]] <- as.character(data[[4]])

# Cuando hay una fila \tab se crea columna con nombre X, X.1, X.2 ..etc
# busqué resolverlo:

data$Variable <- gsub(pattern = "Xileno", replace = "xileno", x = data$Variable)
data$Variable <- gsub(pattern = "X...", replace = NA, x = data$Variable)
data$Variable <- gsub(pattern = "X..", replace = NA, x = data$Variable)
data$Variable <- gsub(pattern = "X", replace = NA, x = data$Variable)
data$Variable <- gsub(pattern = "xileno", replace = "Xileno", x = data$Variable)

data <- data[complete.cases(data),]

#write.csv(data, "data_estaciones.csv", row.names = FALSE)   #cuidado! no pisar


############################################################

# 3- Generar bases de datos por Variables ####

###########################################################


# GENERAL

variables = c("Direc.", "CO", "SO2", "O3", "NO", "NO2", "NOx", "Temp.", "H.Rel.", "PM10", "PM2.5")


data <- read.csv("data_estaciones.csv", header=TRUE,  
                 stringsAsFactors = FALSE)
id <- dir("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\datos\\MHEST", pattern = ".txt") 


for(v in 1:length(variables)){

  variable <- variables[v]
  
  if(!nrow(data[data$Variable == variable,]) == 0){ #CONTROL: si la variable está mal escrita la salta
    list <- which(data$Variable == variable)
    list <- data[list,]
    
    
    # Dar INICIO al bucle:
    i=1
    fichero <- paste("MHEST", substr(list[i,1], 2, 9), list[i,2], ".txt", sep="")
    datos <- read.delim(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\datos\\MHEST\\", 
                              fichero, sep = ""), 
                        header=TRUE, sep="\t", dec=c(",", "."), 
                        skip = 3, stringsAsFactors = FALSE)
    datos <- datos[-1,]  #elimino fila de unidades
    varex <- names(datos)  # Corregir el formato de hora
    if (is.element("HORA", varex)){                  #exists() busca una variable definida
      fecha <- paste(datos[,1], datos[,2], sep=" ")
      fecha <- strptime(fecha, format="%d/%m/%Y %H", tz="GMT")
    }else{
      fecha <- strptime(datos[,1], format="%d/%m/%Y %H:%M", tz="GMT")
    }
    datos <- data.frame(fecha, datos[, variable])   #extraigo variable de inter?s
    names(datos) <- c("date", variable)
    datos[, variable] <- as.numeric(as.character(datos[,variable]))
    tabla <- datos
    
    for (i in 2:nrow(list)){
      fichero <- paste("MHEST", substr(list[i,1], 2, 9) ,list[i,2], ".txt", sep="")
      datos <- read.delim(paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\datos\\MHEST\\", 
                                fichero, sep = ""), 
                          header=TRUE, sep="\t", dec=c(",", "."), 
                          skip = 3, stringsAsFactors = FALSE)
      varex <- names(datos)  
      # Corregir el formato de hora
      if (is.element("HORA", varex)){                  #exists() busca una variable definida
        fecha <- paste(datos[,1], datos[,2], sep=" ")
        fecha <- strptime(fecha, format="%d/%m/%Y %H", tz="GMT")
      }else{
        fecha <- strptime(datos[,1], format="%d/%m/%Y %H:%M", tz="GMT")
      }
      datos <- data.frame(fecha, datos[,variable])   #extraigo variable de inter?s
      names(datos) <- c("date", variable)
      datos[,variable] <- as.numeric(as.character(datos[, variable]))
      if (list[i,1] == list[i-1,1]){
        tabla <- rbind(tabla, datos)
        
      }else{
        assign(paste(variable,"_", list[i-1,1], sep=""), tabla)
        write.csv(tabla, file=paste(variable,"_", substr(list[i-1,1],2,9), ".csv", sep=""), row.names = FALSE)
        tabla <- datos}
    }
    assign( paste(variable,"_", list[i,1], sep=""), tabla)  #guardar ?ltima tabla
    write.csv(tabla, file=paste(variable,"_", substr(list[i,1], 2, 9), ".csv", sep=""), row.names = FALSE)

  }
}

