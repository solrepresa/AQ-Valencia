### Calidad del Aire en Comunitat Valenciana
### 06/03/2019 Valencia, Spain
### Sol Represa
### Archivo 26



library(caret)
library(MASS) #stepAIC()
library(lubridate)
library(reshape2)
library(ggplot2)
library(dplyr)

data <- readRDS("variables_estacion_aod_modelo.rds")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 1) Genero una base unificada con todas las estaciones ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

tabla <- data[[1]]
tabla$mes <- month(tabla$fecha)
tabla$year <- year(tabla$fecha)
tabla$wday <- wday(tabla$fecha)
tabla <- data.frame(tabla[4:34], tabla$mes, tabla$year, tabla$wday, tabla$PM2.5, tabla$NO, tabla$NO2)
names(tabla)[32:37] <- c("mes", "year", "wday","PM25", "NO", "NO2")

for(i in 2:length(data)){ 
  datos <- data[[i]]
  if(length(datos$PM2.5) != 0){
    datos$mes <- month(datos$fecha)
    datos$year <- year(datos$fecha)
    datos$wday <- wday(datos$fecha)
    datos <- data.frame(datos[4:34], datos$mes, datos$year, datos$wday, datos$PM2.5, datos$NO, datos$NO2)
    names(datos)[32:37] <- c("mes", "year", "wday","PM25", "NO", "NO2")
    tabla <- rbind(tabla, datos)
  }
}


names(tabla)[7] <- "NDVI" #lo tengo q corregir en file AC_24.R

tabla <- tabla[,c(35, 1:34, 36:37)]
tabla <- tabla[,-8] #falta en toda una estacion el NDVI
# tabla$DEM <- log(tabla$DEM)



# HAY valores con PM2.5 = 0
# Los elimino: 
tabla[which(tabla$PM25 == 0), 1] <- 0.08  # 1/3 del valor minimo


#tabla[,2:37] <- data.frame(scale(tabla[,2:37]))
tabla <- tabla[,-c(6,7)] # Son cero para todas las estaciones




# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## 2) Analizar correlaciones entre variables  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Es necesario descartar las variables q correlacionan
# para no tener un analisis espurio.



source("/home/usuario/Sol/AQ-Valencia/fuction/cor_mtest.R")


g <- list()


## Algoritmo para generar plot de correlación:

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))  # Rampa de colores
method = "spearman"                        # "spearman" "kendall" o "pearson"

tabla_cor <- cor( tabla, 
                    use = "pairwise.complete.obs")
p.mat <- cor_mtest(tabla, method = method)  
  
# Grafica correlacion
png(paste("corrplot_", method , ".png", sep="") , width = 480, height = 450)
corrplot(tabla_cor,
           method = "color", 
           #title = paste(Correlacion, method, sep=" ") ,
           col = col(200),
           diag = FALSE, # tl.pos="d", 
           type = "upper", 
           order = "original",
           addCoef.col = "black", # Add coefficient of correlation
           p.mat = p.mat, 
           cl.cex = 0.6, #cambia tamaño letra leyenda
           tl.cex = 0.6,  #cambia tamaño letra estaciones
           number.cex = 0.5, #cambia tamaño letra correlacion
           number.font = 1,
           sig.level = 0.05, 
           insig = "pch", 
           mar = c(0,0,1,0) )

dev.off()



#### Sacado de http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}



## Plot para ver correlacion entre las variables

tabla_cor <- round(tabla_cor, digits = 2)
tabla_cor <- get_upper_tri(tabla_cor)
tabla_m <- melt(tabla_cor)
tabla_m <- tabla_m[tabla_m$value != 0,]

tabla_m %>% filter( value > 0.6 | value < -0.6 ) %>%  ### Como criterio, se edscartaron correlaciones > a 0.6
  ggplot(aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill = value )) + 
  scale_fill_gradient2(low = "#4477AA", high = "#BB4444", mid = "white", 
                      midpoint = 0, limit = c(-1,1), space = "Lab", 
                      name = "Pearson\nCorrelation") +
  labs(x = "", y = "", title = "") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_fixed() #+ 
  #geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) 





# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Analizar Missing >> 20%   ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

miss <- colSums(is.na(tabla ))
miss_prec <- miss/nrow( tabla )*100
print(miss_prec[miss_prec > 20])   #falta el 49% de los datos de pm25 y el 92% de los datos de AOD


tabla <- tabla[complete.cases(tabla),]



# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## Crear variables categoricas para fecha   ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 


tabla$year <- factor(tabla$year)
tabla$mes <- factor(tabla$mes)
tabla$wday <- factor(tabla$wday)

date <- data.frame(tabla$year, tabla$mes, tabla$wday)
names(date) <- c("year", "mes", "wday")

model_date <- dummyVars( ~ year + mes + wday , data = date)

tabla_year <- predict(model_date, date)

tabla <- cbind(tabla, as.data.frame(tabla_year))
tabla <- tabla[,-c(30,31,32)]


# Prueba: Agregamos variables al cuadrado
#tabla_cuo <- tabla[,2:35]^2
#tabla_cuo <- data.frame(tabla_cuo)
#names(tabla_cuo) <- paste(names(tabla)[2:length(tabla)], "_sq", sep="")
#tabla <- cbind(tabla, tabla_cuo)






# # # # # # # # # # # # # # # # # # # # # # 

## Normalizacion ####

# Sometimes we need to normalize data in order to compare different variables that are not in the same scale
# Imagine that we have the age and the salary of a person
# If we don't normalize these variables the weight in some predictive models could be very different
# # # # # # # # # # # # # # # # # # # # # # 



# Es necesario convertir a matriz
tabla <- as.matrix(tabla)
dimnames(tabla) <- NULL

tabla <- scale(tabla)

summary(tabla)

# # # # # # # # # # # # # # # # # # # # # # 

## DIVISION de DATA SET ####

# # # # # # # # # # # # # # # # # # # # # # 


## 1) Division de datos = entrenamiento y test

set.seed(123) #seteamos para obtener resultados reproducibles

i_entrena <- createDataPartition(y = tabla[,1], 
                                 p = 0.8, 
                                 list = FALSE)
entrena <- tabla[i_entrena,]
test <- tabla[-i_entrena,]




# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## Variables para modelo  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 


y_train <- entrena[,1]
x_train <- entrena[,2:ncol(entrena)]

y_test <- test[,1]
x_test <- test[,2:ncol(entrena)]


save(y_test, y_train, x_train, x_test, file = "dataset-AC_26.RData")

