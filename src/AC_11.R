### Calidad del Aire en Comunitat Valenciana
### 06/02/2019 Valencia, Spain
### Sol Represa
### Archivo 10?


library(dplyr)



datos <- read.csv("V_PM25_MODIS.csv", stringsAsFactors = FALSE)

datos$estacion <- factor(datos$estacion )
datos$date <- as.Date(datos$date)
datos$mes <- factor(month(datos$date))
datos$wday <- factor(wday(datos$date))


estacion <- read.csv("C:\\Users\\narep\\Desktop\\SOL\\AQ-Valencia\\estaciones_usos.csv", 
                  stringsAsFactors = FALSE)
estacion$Cod <- factor(estacion$Cod)
estacion <- estacion[,-c(4,6,7,9, 11)]
names(estacion)[1] <- "estacion"


estacion <- estacion %>% 
  filter(estacion == "X03014009" | estacion == "X12005005" |
           estacion == "X03066003" | estacion == "X12009007" |
           estacion == "X12028001" | estacion == "X12040008" |
           estacion == "X12140002" | estacion == "X12141002" |
           estacion == "X46010001" | estacion == "X46077006" | 
           estacion == "X46095001" | 
           estacion == "X46250046" | estacion == "X46250048" |
           estacion ==  "X46258001" )


data <- merge(datos, estacion , by = "estacion" )
data <- data[complete.cases(data),]

