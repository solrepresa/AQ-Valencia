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

setwd("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\MERRA\\raster_res") 

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
  theme(axis.text.y = element_text(colour="white")) +
  facet_wrap(~variable) + 
  ggtitle("Datos procesados")




# # # # # # # # # # # # # # # # # # # # # # # # #

## Lista de archivos que faltan

# # # # # # # # # # # # # # # # # # # # # # # # #

a <- miss_data %>% filter( value == "0")

a$value = 1

ggplot(a, aes( x = fecha, y = value)) + geom_point() +
  theme(axis.text.y = element_text(colour="white")) +
  facet_wrap(~variable) + 
  ggtitle("Datos que faltan")




prueba <- a %>% filter( variable == "tavg1_2d_slv_Nx")


# # # # # # # # # # # # # # # # # # # # # # # # #

# Variables de interes de MERRA

SDS <- vector( mode = "list", length = 5)
names(SDS) <- c("tavg1_2d_aer_Nx", "tavg1_2d_slv_Nx", "tavg1_2d_flx_Nx", "avg1_2d_rad_Nx", "inst3_3d_asm_Np") #"inst3_3d_asm_Nv"

SDS[[1]] <- c("BCSMASS", "DMSSMASS", "DUSMASS", "DUSMASS25", "OCSMASS", "SO2SMASS", "SO4SMASS", "SSSMASS", "SSSMASS25")
SDS[[2]] <- "H1000" 
SDS[[3]] <- c("PBLH" , "SPEED", "SPEEDMAX", "USTAR", "PRECTOT")
SDS[[4]] <- c("ALBEDO", "CLDHGH", "CLDLOW")
SDS[[5]] <- c("PS", "T", "RH", "U", "V")