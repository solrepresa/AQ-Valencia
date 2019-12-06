# Sol Represa
# AQ Valencia
# 11/12/2018

# Objetivo: 
# Generar medias diarias y cuántos datos faltan


library(data.table)
library(pvclust)



############################################################

## 0 - Cargar datos 

###########################################################

source("AQ-Valencia-6.R") #estaciones seleccionadas

# pollutant tiene los datos horarios medidos
# fact tiene los factores ambientales


############################################################

# 1 - Generar medias diarias

############################################################

## Medias 
source("fuction/medias_diarias.R")

medias_dia <- medias_diarias(pollutant, 3)

medias_dia <- lapply(medias_dia, function(x) x[367:nrow(x),])  #me quedo con los datos a partir del 2009


#saveRDS(medias_dia, file ="medias_dia_est_utiles.Rds")


## Recorte temporal en pollutant

pollutant <- lapply(pollutant, function(x) x[8785:nrow(x),])  #me quedo con los datos a partir del 2009

#saveRDS(pollutant_names, file = "pollutant_names.Rds")
#saveRDS(pollutant, file = "pollutant_ut.Rds")


rm(list = ls())

############################################################

## 2. Base de datos faltantes por dia ####


medias_dia <- readRDS("medias_dia_est_utiles.Rds")
pollutant_names <- readRDS("pollutant_names.Rds")
pollutant <- readRDS("pollutant_ut.Rds")


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

# saveRDS(g, file="NA_mensuales_ut.Rds")
# saveRDS(k, file="NA_diarios_ut.Rds")


rm(g,k)



#####################################################################

### 5) Obtener lista con nombres y contaminantes medidos por estación ####

#####################################################################

est_util <- read.csv("estaciones_utiles.csv")
est_util <- est_util[-1]
est_util$Nombre <- factor(est_util$Estacion, 
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


estaciones <- unique(as.character(est_util$Estacion))
Nombre <- unique(as.character(est_util$Nombre))

salida <- data.frame()
for (i in 1:length(estaciones)){
 contaminante <- as.character(est_util[est_util$Estacion == estaciones[i],2])
 contaminante <- contaminante[order(contaminante)]
 contaminante <- paste(contaminante, collapse =" ")
 data <- c(estaciones[i], Nombre[i], contaminante)
 salida <- rbind(salida, data, stringsAsFactors=FALSE)
 names(salida) <- c("Estacion", "Nombre", "Contaminante")
}

#write.csv(salida, "estaciones_ut_conts.csv")

