# Sol Represa
# AQ Valencia
# 19/12/2018


# Objetivo: Analisis de Datos faltantes
# ¿Hay una falta de datos sesgada?


library(ggplot2)
library(dplyr)
library(lubridate)

###################################################

## 1) Abrir bases de datos

####################################################

medias_dia <- readRDS("medias_dia_est_utiles.Rds")
pollutant_names <- readRDS("pollutant_names.Rds")
pollutant <- readRDS("pollutant_ut.Rds")
NA_mensual <- readRDS("NA_mensuales_ut.Rds")
NA_dia <- readRDS("NA_diarios_ut.Rds")

NA_dia <- lapply(NA_dia, function(x) x[366:nrow(x),])  #me quedo con los datos a partir del 2009


###################################################

## 2) Analisis de correlacion entre Na y valores

####################################################

i=2

tabla_NA <- NA_dia[[i]]
tabla_NA$date <- as.Date(tabla_NA$date)

tabla_pol_dia <- medias_dia[[i]]
tabla_pol_dia$date <- as.Date(tabla_pol_dia$date)

j=8
tabla <- cbind( tabla_NA[1] , tabla_NA[j], tabla_pol_dia[j]) 
names(tabla) <- c("date", "Missing", "Valores")

tabla %>%
  mutate( year = year(date) ) %>%
  mutate( month = month (date)) %>%
  ggplot(aes(x = date)) + 
  geom_area(aes(y = Missing)) +
  geom_point(aes(y= Valores*4), col = "green") +
  scale_y_continuous(sec.axis = sec_axis(~./4, name = "Valores")) +
  theme_bw()




