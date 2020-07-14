### Calidad del Aire en Comunitat Valenciana
### 01/03/2019 Valencia, Spain
### Sol Represa
### Archivo 25



### Objetivo: Revisar archivos MERRA generados
# para ver si tengo las series completas de todas las variables que me interesan


library(dplyr)
library(ggplot2)
library(reshape2)


# # # # # # # # # # # # # # # # # # # # # # # # #

## MISSING de la serie de tiempo

# # # # # # # # # # # # # # # # # # # # # # # # #


# Los files procesados

setwd("/media/usuario/Elements SE/MERRA/raster_res") 

id <- list.files(path = getwd(),
                 pattern = "*.tif",
                 full.names = FALSE)


archivo <- substr(id, 12, 26)
fecha <- substr(id, 28, 35)
SDS <- substr(id, 37, nchar(id)-4)

base_MERRA <- data.frame(archivo, fecha, SDS)
base_MERRA$fecha <- as.Date(base_MERRA$fecha, format = "%Y%m%d", tz="GMT" )

datos <- base_MERRA %>% group_by(archivo, fecha) %>% summarise()

datos <- as.data.frame(datos)
datos <- dcast(datos, fecha ~ archivo)


# Completo la serie de tiempo
fecha <- as.data.frame(seq.Date(as.Date(ISOdate(2008,01,01)), as.Date(ISOdate(2018,12,30)), by = "day", tz = "GMT")) 
names(fecha) <- "fecha"

missing_data <- merge(datos, fecha, by = "fecha", all = TRUE)


## Funcion de datos Missing  ####

# Me genera con una tabla donde
# 0 = NA
# 1 = hay dato

miss <- function(tabla, date){
  miss <- data.frame(date) #creo un data.frame con fechas
  for(j in 2:length(tabla)){ #recorro todas las columnas j de datos
    for(i in 1:nrow(tabla)){ #recorro todas las filas i
      if(is.na(tabla[i,j])){
        miss[i,j] <- 0
      }else{
        miss[i,j] <- 1
      }
    }
  }
  names(miss) <- names(tabla)
  return(miss)
}

miss_data <- miss(missing_data, missing_data$fecha)
miss_data <- melt(miss_data, id.vars = "fecha" )



# # # # # # # # # # # # # # # # # # # # # # # # #

# Plot para visualizar los datos que tengo procesados ####


# plot barras

ggplot(miss_data, aes(x = fecha, y = value)) + 
  geom_bar(stat="identity") + 
  theme_bw() + ylab("") + 
  scale_y_continuous(breaks =  c(0, 1)) + 
  theme(axis.text.y = element_text(colour = "white" )) +
  facet_wrap(~variable) + 
  ggtitle("Datos procesados")


# para ver de a una variable
miss_data %>% filter( variable == "inst3_3d_asm_Np") %>%
  ggplot(aes(x = fecha, y = value)) + 
  geom_bar(stat="identity") + 
  theme_bw() + ylab("") + 
  scale_y_continuous(breaks =  c(0, 1)) + 
  theme(axis.text.y = element_text(colour="white")) +
  ggtitle("Datos procesados")


# # # # # # # # # # # # # # # # # # # # # # # # #

## Lista de archivos que faltan

# # # # # # # # # # # # # # # # # # # # # # # # #

falta <- miss_data %>% filter( value == "0")

falta$value = 1

ggplot(falta, aes( x = fecha, y = value)) + geom_point() +
  theme(axis.text.y = element_text(colour="white")) +
  facet_wrap(~variable) + 
  ggtitle("Datos que faltan")


falta %>% filter( variable == "inst3_3d_asm_Np") %>%
  ggplot(aes(x = fecha, y = value)) + 
  geom_bar(stat="identity") + 
  theme_bw() + ylab("") + 
  scale_y_continuous(breaks =  c(0, 1)) + 
  theme(axis.text.y = element_text(colour="white")) +
  ggtitle("Datos faltantes")


falta_var <- falta %>% filter( variable == "inst3_3d_asm_Np")





# # # # # # # # # # # # # # # # # # # # # # # # #

# Ver SDS faltantes 

# # # # # # # # # # # # # # # # # # # # # # # # #


sds_datos <- base_MERRA %>% group_by(SDS, fecha) %>% summarise()
sds_datos <- as.data.frame(sds_datos)
sds_datos <- dcast(sds_datos, fecha ~ SDS)


missing_data_sds <- merge(sds_datos, fecha, by = "fecha", all =TRUE)

# Ver los miss
miss_data_sds <- miss(missing_data_sds, missing_data_sds$fecha)
miss_data_sds <- melt(miss_data_sds, id.vars = "fecha")


ggplot(miss_data_sds, aes(x = fecha, y = value)) + 
  geom_bar(stat="identity") + 
  theme_bw() + ylab("") + 
  scale_y_continuous(breaks =  c(0, 1)) + 
  theme(axis.text.y = element_text(colour="white")) +
  facet_wrap(~variable) + 
  ggtitle("Datos procesados")



# # # # # # # # # # # # # # # # # # # # # # # # #

#  Eliminar errores de la base de datos

# # # # # # # # # # # # # # # # # # # # # # # # #


## Eliminar los archivos de "inst3_3d_asm_Nv" que descargue por error
files_rm <- list.files(path = getwd(),
                 pattern = "inst3_3d_asm_Nv",
                 full.names = FALSE)

#file.remove(files_rm)


# # # # # # # # # # # # # # # # # # # # # # # # #

# Analizar variables para el modelo

# # # # # # # # # # # # # # # # # # # # # # # # #

# Quedarme solo con las variables de interes
sds_lista <- base_MERRA %>% 
  filter(SDS == "PS" | SDS == "RH" | SDS == "T" | SDS == "U" | SDS == "V" |
           SDS == "BCSMASS" | SDS ==  "DMSSMASS" | SDS ==  "DUSMASS" | SDS ==  "SO4SMASS" |
           SDS ==  "SSSMASS25" | SDS == "PRECTOT" | SDS ==  "SPEED" | SDS ==  "CLDHGH" |
           SDS == "CLDLOW" | SDS ==  "H1000" )

sds_lista$fecha <- as.factor(as.character(sds_lista$fecha))
sds_lista$SDS <- as.character(sds_lista$SDS)

# Agrupar por fecha para ver si tengo todos los archivos
sds_lista <- sds_lista  %>% 
  group_by(fecha) %>% summarise(n = n())

# Tengo toda la base completa :D




# # # # # # # # # # # # # # # # # # # # # # # # #

# Variables de interes de MERRA

SDS <- vector( mode = "list", length = 5)
names(SDS) <- c("inst3_3d_asm_Np", "tavg1_2d_aer_Nx", "tavg1_2d_slv_Nx", "tavg1_2d_flx_Nx", "avg1_2d_rad_Nx" ) 

SDS[[1]] <- c("PS", "T", "RH", "U", "V")
#"inst3_3d_asm_Nv"
SDS[[2]] <- c("BCSMASS", "DMSSMASS", "DUSMASS", "DUSMASS25", "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS", "SSSMASS25")
SDS[[3]] <- "H1000" 
SDS[[4]] <- c("PBLH" , "SPEED", "SPEEDMAX", "USTAR", "PRECTOT")
SDS[[5]] <- c("ALBEDO", "CLDHGH", "CLDLOW")
