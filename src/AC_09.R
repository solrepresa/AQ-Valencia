### Calidad del Aire en Comunitat Valenciana
### 06/02/2019 Valencia, Spain
### Sol Represa
### Archivo 9



## Objetivo: 
# - Evaluar el modelo lineal solos con AOD y PM2.5 datos diarios


library(lattice)
library(lubridate)
library(fitdistrplus)
library(nlme)
library(caret)


datos <- read.csv("V_PM25_MODIS.csv", stringsAsFactors = FALSE)

datos$estacion <- factor(datos$estacion)
datos$date <- as.Date(datos$date)



# # # # ¿Dónde hay NA? # # # # # # # # # #

# NA que me van a complicar la vida :)
# Antes 219856 - despues 14411
datos <- datos[complete.cases(datos),]   # pierdo 221 149 registros 

# # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # #

## 1- Chequear la distribución

# # # # # # # # # # # # # # # # # # # #

## Boxplot

boxplot(mean ~ factor(estacion),
        varwidth = TRUE, xlab = "Station",
        main = "Boxplot of concentration conditional on station", 
        ylab = expression(paste("PM"[2.5], " concentration" )), data = datos)


### Diagrama de dispersión

xyplot(aod ~ mean | factor(estacion), 
       #type = "l",
       xlab = "Estimated PM2.5", col = 1,
       ylab = "AOD",
       strip = function(bg = 'white', ...)  #para dar color blanco a lattice
         strip.default(bg = 'white', ...),
       data = datos)


## Grafico de Cullen y Frey
descdist(datos$mean)

## Test de distribucion
shapiro.test(sample(datos$mean, size = 5000)) #datos no normales
ks.test(x= datos$mean ,y='pnorm',alternative='two.sided')


# TRANSFORMACION => distribucion lognormal ?

datos$logPM <- log(datos$mean)


datos <- datos[-which(datos$logPM == -Inf ),]
# Fueron descartados 7 valores
# 3965  4083  4390  5931  5932  7260 12204

lim <- boxplot(datos$logPM)  # tengo outliers

# Quito valores extremos >> distribución normal!
datos <- subset(datos, datos$logPM >= lim$stats[1]) #mayores al q1 = remuevo 19 datos
datos <- subset(datos, datos$logPM <= lim$stats[5]) # menores a q5 = remuevo 23 datos


# Nuevamente pruebas:
shapiro.test(sample(datos$logPM, size = 5000))
ks.test(x= datos$logPM ,y='pnorm',alternative='two.sided')


# Chequear la distribución  => distribucion no es normal :/
descdist(datos$logPM)



# # # # # # # # # # # # # # # # # # # #

## 2- lm ####

# # # # # # # # # # # # # # # # # # # #

## 0) Algoritmo para extraer porción de los datos ##

#set.seed(131)
#data <- createDataPartition(datos$logPM , p = 0.8, list = FALSE)
#plot(density(datos[data,7]))
#rug(datos[data,7])


## 1) Modelo pasando por el origen ##

#reg_0 <- lm( datos$logPM ~ 0 + datos$aod)  
#summary(reg_0)      # Adjusted R-squared: 0.7386  (sin log daba 0.6028)
#layout(matrix(1:4,2,2)) 
#plot(reg_0)
#hist(reg_0$residuals, xlab = "Residuals", main = "")


## 2) Modelo lineal 

reg_lm <- lm( datos$mean ~ datos$aod) 
summary(reg_lm)    # Adjusted R-squared: 0.1542 (sin log daba 0.1387) 
#coef(reg_lm)
#anova(reg_lm)

layout(matrix(1:4,2,2)) 
plot(reg_lm)

reg_resid <- reg_lm$residuals
shapiro.test(sample(reg_resid, size = 5000)) # si p < 0.05 entonces los residuos no son normales
hist(reg_resid, breaks = 50) 


#PLOT modelo
data <- data.frame( datos$logPM, reg_lm$fitted.values, datos$aod)
data <- data.frame( exp(datos$logPM), exp(reg_lm$fitted.values) )

names(data) <- c("logPM", "modelado", "aod")
ggplot(data, aes( x= logPM, y= modelado )) + 
  geom_point() + 
  xlim(-0.3,6) + 
  ylim(-0.3,6)



## Calculo de RMSE ##

sqrt( sum( (data$modelado - data$logPM)^2 , na.rm = TRUE ) / nrow(data) )  # RMSE 0.7301418
#¿ por q da bajo con un modelo tan feo?


# # # # # # # # # # # # # # # # # # # #

# para este set de datos NO FUNCIONA la distribucion gamma

# # # # # # # # # # # # # # # # # # # #

a <- datos[which(datos$logPM >= 0),]

glm(log(datos$mean) ~ 0 + datos$aod, family = Gamma)  # AIC = 40640  #gaussian
glm(log(datos$mean) ~ datos$aod, family = Gamma)  # AIC = 31730



# # # # # # # # # # # # # # # # # # # #

# Limpieza del set de datos

# # # # # # # # # # # # # # # # # # # #

data$rstudent <- rstudent(reg_lm)
plot(data$rstudent )


data <- data[which(data$rstudent < 3),]  #7 datos
data <- data[which(data$rstudent > -3),]  #16 datos

reg_lm2 <- lm( data$logPM ~ data$aod) 
plot(reg_lm2)
summary(reg_lm2)    # Adjusted R-squared:  0.1625  


# # # # # # # # # # # # # # # # # # # #

# Analisis de autocorrelacion 

acf(reg_lm$residuals )

# # # # # # # # # # # # # # # # # # # #



