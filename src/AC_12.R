### Calidad del Aire en Comunitat Valenciana
### 06/02/2019 Valencia, Spain
### Sol Represa
### Archivo 12



## Objetivo: 
# - Evaluar el modelo lineal solos con AOD y PM2.5 datos mensuales


library(lattice)
library(lubridate)
library(fitdistrplus)
library(nlme)
library(caret)


datos <- read.csv("V_PM25_MODIS.csv", stringsAsFactors = FALSE)

datos$estacion <- factor(datos$estacion )
datos$date <- as.Date(datos$date)


datos <- datos %>%
  group_by( estacion, month = floor_date(date, "month")) %>%
  summarise( PM25 = mean(mean, na.rm = TRUE),
             AOD = mean( aod, na.rm = TRUE))

datos <- datos[complete.cases(datos),]   # pierdo 466 registros 


# # # # # # # # # # # # # # # # # # # #

## 1- Chequear la distribución

# # # # # # # # # # # # # # # # # # # #

descdist(datos$PM25) #lognormal

## Test de distribucion
shapiro.test(sample(datos$mean, size = 5000)) #datos no normales
ks.test(x= datos$mean ,y='pnorm',alternative='two.sided')


datos$logPM <- log(datos$PM25)

#which(datos$logPM == -Inf )
datos <- subset(datos, datos$logPM >= lim$stats[1]) #mayores al q1 
datos <- subset(datos, datos$logPM <= lim$stats[5]) # menores a q5 

shapiro.test(datos$logPM) # caaaaaaasiii

# Chequear la distribución  => distribucion caaasi normal :/
descdist(datos$logPM)



# # # # # # # # # # # # # # # # # # # #

## 2- lm ####

# # # # # # # # # # # # # # # # # # # #


reg_lm <- lm( datos$logPM ~ datos$AOD) 
summary(reg_lm)    # Adjusted R-squared: 0.04054  (sin log daba 0.04096 ) 
#coef(reg_lm)
#anova(reg_lm)

layout(matrix(1:4,2,2)) 
plot(reg_lm)

reg_resid <- reg_lm$residuals
shapiro.test(reg_resid) # si p < 0.05 entonces los residuos no son normales
hist(reg_resid, breaks = 50) 


#PLOT modelo
data <- data.frame( datos$logPM, reg_lm$fitted.values, datos$AOD)

names(data) <- c("logPM", "modelado", "AOD")
ggplot(data, aes( x= logPM, y= modelado )) + 
  geom_point() + 
  xlim(0, 4) + 
  ylim(0, 4)


data <- data.frame( datos$PM25, exp(reg_lm$fitted.values) )
names(data) <- c("PM25", "modelado")

ggplot(data, aes( x= PM25, y= modelado )) + 
  geom_point() +
  xlim(0, 26) + 
  ylim(0, 26)

