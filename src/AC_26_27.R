
# Este fue el script con el q finalmente arme el modelo
# Revisar y corregir 26 y 27

library(lubridate)
library(caret)
library(chron)
library(dplyr)
library(ggplot2)
library(nlme)
library(MuMIn)
library(corrplot)
library(colorRamps)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 1. Carga de datos  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 


data <- readRDS("/home/usuario/Sol/aire_comunitat/variables_estacion_aod_modelo.rds")

tabla <- data[[1]]   #15704 por estacion, no completos
tabla$mes <- month(tabla$fecha)
tabla$jday <- format(tabla$fecha, "%j")
tabla$wkend <- is.weekend(tabla$fecha)
tabla <- data.frame(tabla[3:34], tabla$mes, tabla$jday, tabla$wkend, tabla$PM2.5, tabla$NO, tabla$NO2)
names(tabla)[33:38] <- c("mes", "jday", "wkend","PM25", "NO", "NO2")

for(i in 2:length(data)){ 
  datos <- data[[i]]
  if(length(datos$PM2.5) != 0){
    datos$mes <- month(datos$fecha)
    datos$jday <- format(datos$fecha, "%j")
    datos$wkend <- is.weekend(datos$fecha)
    datos <- data.frame(datos[3:34], datos$mes, datos$jday, datos$wkend, datos$PM2.5, datos$NO, datos$NO2)
    names(datos)[33:38] <- c("mes", "jday", "wkend","PM25", "NO", "NO2")
    tabla <- rbind(tabla, datos)
  }
}

tabla <- tabla[,c(36, 1:35, 37:38)]
tabla <- tabla[, -9] # sacamos NDVI porq una estcion entera no tiene mediciones :/


### Analisis de correlacion

tabla <- tabla[,-c(18,19)]  #BCSMASS -- OCSMASS - SO2MASS correlacionan
tabla <- tabla[,-16]  # "DUSMASS25" -"DUSMASS"  correlacionan
tabla <- tabla[,-c(19,20)]  # "DMSSMASS" - "SSSMASS25"- "SSSMASS"  correlacionan
tabla <- tabla[,-c(20,22)]  # "SPEEDMAX"  "SPEED"     "USTAR"  correlacionan
tabla <- tabla[,-c(7,8)] # Son cero para todas las estaciones
tabla <- tabla[,-27] # NO correlaciona con NO2
tabla <- tabla[,-19] # Albedo correlaciona fuerte con varias
tabla <- tabla[, -5]  #elimino clc_2 porq tiene mucha correlacion con clc_1

tabla$DEM <- log(tabla$DEM)
tabla[which(tabla$PM25 == 0), 1] <- 0.08  # LD/3
#tabla[which(tabla$PM25 > 100), 1] <- NA  # limpieza de valor extremo


# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 2. Crear variables categoricas para fecha  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

tabla$jday <- factor(tabla$jday)
#tabla$year <- factor(tabla$year)
tabla$mes <- factor(tabla$mes)
tabla$wkend <- factor(tabla$wkend)

tabla$Codigo <- factor(tabla$Codigo)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 3. Hay suficientes replicas?   ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

replications(PM25 ~ Codigo * jday, tabla)

# Para la estaci?n X03014006 (n = 8), X46220010 (n=0)y X12040010 (n = 75) no tengo casi datos
# Me quedo con estaciones que tengan m?s de 3 mil registros totales

tabla <- tabla %>% filter( Codigo ==  "X12005005" | Codigo == "X12009007" | 
                       Codigo == "X12040008" | Codigo == "X12140002" |
                       Codigo == "X12141002" | Codigo == "X46010001" | 
                       Codigo == "X46077006" | Codigo == "X46102002" |
                       Codigo == "X46250030" | Codigo == "X46250046" |
                       Codigo == "X46250048" | Codigo == "X46258001")

tabla$Codigo <- factor(tabla$Codigo)
replications(PM25 ~ Codigo * jday, tabla)


# Se observa que algunos a?os no tengo datos de todos los meses (final de 2018)
# Se observa que algunos dias de las semanas faltan en a?o|meses

# Opci?n: se puede trabajar como dias festivos - dias laborales
# Incluso faltan weekday por estacion/a?o faltan datos en algunas estaciones


####

tabla <- tabla[complete.cases(tabla),]

tabla.center <- scale( tabla[, c(3:21,25)])
tabla.center <- as.data.frame(tabla.center)
tabla[, c(3:21, 25)]<- tabla.center

##

base = tabla

rm(data, datos, tabla.center)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 4. Explorar datos ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# no existe mas year como variable <<< volver a crearla
base %>% filter( year == "2016") %>% filter( mes == "4") %>%
  ggplot() + 
  geom_point(aes(x = aod, y = PM25 )) + 
  theme_bw() + 
  facet_wrap(~Codigo)




# Evaluar correlacion de nuevo
corr <- cor(base[,  c(3:21, 25)], method = "pearson", use = "complete.obs")
p.mat <- cor.mtest(base[,  c(3:21, 25)])
corrplot(corr, 
         method = "color",
         type = "lower",
         #insig = "p-value", 
         sig.level = 0.01, 
         diag = FALSE, 
         p.mat = round(p.mat$p, 3),
         tl.cex = 0.5)

# se me fue pblh!

#ARMAR FORMULA:

formula(base)
base <- groupedData(PM25 ~ DEM + CLC_1 + PS + RH + T + U + 
                      V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                      PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                      aod +  1|Codigo/jday, data = base) 
formula(base)


#SAVE
#write.csv(base, "variables_estacion_aod_modelo.csv", row.names = FALSE)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 5. Generar set de prueba  #####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 


#con datos altos
base <- read.csv("/home/usuario/Sol/aire_comunitat/variables_estacion_aod_modelo.csv")

set.seed(123) #seteamos para obtener resultados reproducibles

i_entrena <- createDataPartition(y = base$PM25, 
                                 p = 0.8, 
                                 list = FALSE)
entrena <- base[i_entrena,]
test <- base[-i_entrena,]


rm(i_entrena)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 6. Modelos Mixtos ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Hay efecto mixto?

GLMM <- lme(PM25 ~ aod , 
                data = entrena,
                random = ~1|Codigo/jday,
                method = "REML",
            control = list(opt = "optim")) 
plot(GLMM)

GLM <- glm(PM25 ~ aod, 
               data = entrena)

AICc(GLMM, GLM)  #Con efecto mixto obtenemos un mejor resultado

GLMM.1 <- lme(PM25 ~ aod , 
            data = entrena,
            random = ~1|jday,
            method = "REML",
            control = list(opt = "optim")) 

AICc(GLMM, GLMM.1) # es mejor el GLMM

# ?Que variables incluyo?

GLMM.tot <- lme(PM25 ~ DEM + CLC_1 + CLC_3 + PS + RH + T + U + 
                  V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                  PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                  aod +  NO2, 
                data = entrena,
                random = ~1|Codigo/jday,
                method = "REML") #maxima verosimilitud restringida para dise?os desbalanceados

GLMM.tot.1 <- lme(PM25 ~ DEM + CLC_1 + CLC_3 + PS + RH + T + U + 
                  V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                  PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                  aod +  NO2, 
                data = entrena,
                random = ~1|jday,
                method = "REML") #maxima verosimilitud restringida para dise?os desbalanceados

AICc(GLMM.tot, GLMM.tot.1)

plot(GLMM.tot)
#qqnorm(GLMM.tot, ~resid(.)|Codigo, id = 0.01, adj = -0.5, layout = c(4,2))
hist(resid(GLMM.tot))

plot(GLMM.tot.1)
hist(resid(GLMM.tot.1))



# los residuos dan feos. Tengo heteroscedacia 
# Si el modelo fuera fiable, entonces puedo comenzar a simplificar
# pero como tengo muuuucha heteroscedacia tengo q modelar la variacion


# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 5) Modelado de la variancia

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 


GLMM.tot <- lme(PM25 ~ DEM + CLC_1 + CLC_3 + PS + RH + T + U + 
                  V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                  PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                  aod +  NO2, 
                data = tabla,   #no converge
                random = ~1|jday,
                method = "ML",
                control = list(opt = "optim")) #maxima verosimilitud restringida para dise?os desbalanceados

plot(GLMM.tot)


# 6. 1. Con que modelo de variancia? ####

# 1) Funci?n de varianza seg?n los errores ajustados:
FuncVar.Ajustados.Pow <- varPower(form = ~fitted(.))

# 2) Funci?n de varianza para homogeneidad entre niveles de orden:
FuncVar.Ord <- varIdent(form = ~1|jday)

# 3) Funci?n de varianza seg?n los ajustados por orden, exponencial:
FuncVar.Ajustados.Exp.Ord <- varExp(form = ~fitted(.)|jday)

# 4) Funci?n de varianza seg?n los ajustados por orden, potencial:
FuncVar.Ajustados.Pow.Ord <- varPower(form= ~fitted(.)|jday)

# 5) Funci?n de varianza combinada, para homogeneidad entre niveles de orden y seg?n los ajustados general
FuncVar.Comb <- varComb(varIdent(form = ~1|jday), varPower(form = ~fitted(.)))



# IMPORTANTE: quitar las variables q correlacionan porq tira error de convergencia.
# Si sigo teniendo problemas de convergencia, incorporo las variables de a poco.

GLMM.1 <- lme(PM25 ~ DEM + CLC_1 + CLC_3 + PS + RH + T + U + 
                V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                aod +  NO2, 
                  data = entrena,
                  random = ~1|jday,    # ~1|Codigo/jday,
                  weights = FuncVar.Ajustados.Pow ,  #no converge
                  method = "ML",
              control = list(opt = "optim")) 

GLMM.2 <- lme(PM25 ~ DEM + CLC_1 + CLC_3 + PS + RH + T + U + 
                V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                aod +  NO2, 
                  data = entrena,
                  random = ~1|jday,  # ~1|Codigo/jday,
                  weights = FuncVar.Ord ,  #converge
                  method = "ML",
              control = list(opt = "optim"))

GLMM.3 <- lme(PM25 ~ DEM + CLC_1 + CLC_3 + PS + RH + T + U + 
                V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                aod +  NO2, 
              data = entrena,
              random = ~1|jday,  #~1|Codigo/jday,
              weights = FuncVar.Ajustados.Exp.Ord,  #converge? no
              method = "ML",
              control = list(opt = "optim"))

GLMM.4 <- lme(PM25 ~ DEM + CLC_1 + CLC_3 + PS + RH + T + U + 
                V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                aod +  NO2,  
              data = entrena,
              random = ~1|jday,   #~1|Codigo/jday,
              weights = FuncVar.Ajustados.Pow.Ord,  # converge? no
              method = "ML",
              control = list(opt = "optim"))

GLMM.5 <- lme(PM25 ~ DEM + CLC_1 + CLC_3 + PS + RH + T + U + 
                V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                aod +  NO2, 
              data = entrena,
              random = ~1|jday,  #~1|Codigo/jday,
              weights = FuncVar.Comb,  # converge? no
              method = "ML",
              control = list(opt = "optim"))

AIC(GLMM.tot, GLMM.2)
# es mejor el GLMM.2


# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 6. 2. Achicamos modelos.. Cuales variables sirven? ####

GLMM.V2.1 <- lme(PM25 ~ DEM + CLC_1 + CLC_3 + PS + RH + T + U + 
                   V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                   PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                   aod + NO2,  
               data = entrena,
               random = ~1|Codigo/jday,
               weights = FuncVar.Ord,  
               method = "REML",
               control = list(opt = "optim")) 

GLMM.V2.2 <- lme(PM25 ~ CLC_1 + CLC_3 + PS + RH + T + U + 
                   V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                   PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                   aod + NO2,   # sin dem
                 data = entrena,
                 random = ~1|Codigo/jday,
                 weights = FuncVar.Ord, #converge
                 method = "REML",
                 control = list(opt = "optim")) 

GLMM.V2.3 <- lme(PM25 ~ PS + RH + T + U + 
                   V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                   PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                   aod + NO2,   # sin DEM, CLC_1 ni CLC_3 
                 data = entrena,
                 random = ~1|Codigo/jday,
                 weights = FuncVar.Ord  , #converge
                 method = "REML",
                 control = list(opt = "optim")) 

GLMM.V2.4 <- lme(PM25 ~ DEM + CLC_1 + CLC_3 + PS + RH + T +  
                   BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                   PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                   aod + NO2,  # no converge
                 data = entrena,
                 random = ~1|Codigo/jday,
                 weights = FuncVar.Ord,  
                 method = "REML",
                 control = list(opt = "optim")) 


AIC(GLMM.V2.1, GLMM.V2.2, GLMM.V2.3)  #es mejor el completo


GLMM.V2.5 <- lme(PM25 ~ DEM + CLC_1 + CLC_3 + PS + RH + T + U + 
                   V + #BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                   #PRECTOT + SPEED + 
                   CLDHGH + CLDLOW + H1000 + 
                   aod + NO2,  #no converge
                 data = entrena,
                 random = ~1|Codigo/jday,
                 weights = FuncVar.Ord,  
                 method = "REML",
                 control = list(opt = "optim")) 

plot(GLMM.V2.1)


## Pruebo cambiar la representaci?n del efecto mixto

GLMM.V5.1 <- lme(PM25 ~ DEM + CLC_1 + CLC_3 + PS + RH + T + U + 
                   V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                   PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                   aod + NO2,  
                 data = entrena,
                 random = ~1|Codigo + 1|jday,  #con warnings
                 weights = FuncVar.Ord,  
                 method = "REML",
                 control = list(opt = "optim")) 

GLMM.V5.2 <- lme(PM25 ~ DEM + CLC_1 + CLC_3 + PS + RH + T + U + 
                   V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                   PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                   aod + NO2,  
                 data = entrena,
                 random = ~1|jday,
                 weights = FuncVar.Ord,  
                 method = "REML",
                 control = list(opt = "optim")) 


AIC(GLMM.V2.1, GLMM.V5.1, GLMM.V5.2) # es mucho mejor el 2.1, luego el 5.2


# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 6. 3. Evaluacion del modelo ####

hist(GLMM.V2.1$residuals)
shapiro.test(sample(GLMM.V2.1$residuals, size = 5000 ))  #los errores son normales

entrena$fit <- predict(GLMM.V2.1)
plot(entrena$fit, entrena$PM25)


RMSE = function(m, o){   # m = model, o = observed
  sqrt(mean((m - o)^2))
}

MAE <- function(m, o) { mean(abs(m - o)) }


RMSE(entrena$fit, entrena$PM25) # 3.31 con todo el set de datos
MAE(entrena$fit, entrena$PM25) # 1.75 con todo el set de datos


# prueba con modelo V5.2
prueba <- data.frame(predict(GLMM.V5.2), entrena$PM25)
names(prueba) <- c("fit", "PM25")
RMSE(prueba$fit, prueba$PM25)   # 5.29
MAE(prueba$fit, prueba$PM25)   # 3.62



#You can use the predict() function to make predictions from that model on new data

saveRDS(final_model, "./final_model.rds")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 7. Random Forest ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Random Forest  - library(randomForest)  ####
model1 <- randomForest(PM25 ~ DEM + CLC_1 + PS + RH + T + U + 
                         V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                         PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                         aod, 
                       data = entrena,
                       ntree = 100)

# Mean of squared residuals: 5.304161
# % Var explained: 89.1

# model1$rsq[length(model1$rsq)]   #0.89
# model1$mse[length(model1$mse)] #5.3



pred_rf <- predict(model1, test)

my_data <-  as.data.frame(cbind(predicted = pred_rf,
                                observed = test$PM25))


ggplot(my_data, aes(predicted, observed)) +
  geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method = lm)+ ggtitle('Linear Regression ') +
  ggtitle("Linear Regression: Prediction vs Test Data") +
  xlab("Predecited") +
  ylab("Observed") +
  theme(plot.title = element_text(color="darkgreen",size = 18,hjust = 0.5),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, hjust=.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


RMSE = function(m, o){   # m = model, o = observed
  sqrt(mean((m - o)^2))
}

MAE <- function(m, o) { mean(abs(m - o)) }


RMSE(mydata$predicted, mydata$observed) # con el set de entrenamiento
MAE(mydata$predicted, mydata$observed) # con el set de entrenamiento


datos_rf <- data.frame(ID = row.names(test), PM25_modelado = pred_rf)
#write.csv(datos_rf, file = "modelo_rf.csv", row.names = F)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Random forest con 10 fold cv - library(caret) ####

train.control <- trainControl(method = "repeatedcv", 
                              number = 10,
                              repeats = 10,
                              search = "random") # Set up repeated k-fold cross-validation

model_rf <- train(PM25 ~ DEM + CLC_1 + PS + RH + T + U + 
                    V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                    PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                    aod, 
                  data = entrena,
                  method = 'rf', 
                  trControl = train.control,
                  #tuneGrid = data.frame(mtry = 1:34),
                  ntree = 10,
                  importance = TRUE)




saveRDS(model_rf, file = "modelo_rf_10tree.rds")

# mtry  RMSE      Rsquared   MAE      
# 7    2.440691  0.8928053  0.7642050

plot(varImp(model_rf))
plot(model_rf)


pred_rf <- predict(model_rf, test)

my_data <-  as.data.frame(cbind(predicted = pred_rf,
                                observed = test$PM25,
                                Error = abs(pred_rf - test$PM25)))

ggplot(my_data, aes(predicted, observed, colour = Error)) +
  geom_point(alpha = 0.8) + 
  geom_smooth(method = lm)+ ggtitle('Linear Regression ') +
  ggtitle("Random Forest: ntree = 10, mtry = 7") +
  xlab("Predicted") +
  ylab("Observed") +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, hjust=.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) + 
  theme_bw() +
  #coord_fixed() +
  scale_colour_gradientn(colours = matlab.like(10))




# # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Todos los modelos para comparar ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Linear Regression model
model_lm <- caret::train(PM25 ~ DEM + CLC_1 + PS + RH + T + U + 
                           V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                           PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                           aod, 
                         data = entrena,
                         method = "lm")  #preProcess= c("center", "scale")

# Linear Regression with Stepwise Selection
model_step <- caret::train(PM25 ~ DEM + CLC_1 + PS + RH + T + U + 
                             V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                             PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                             aod, 
                           data = entrena,
                           method = "leapSeq", # "leapBackward","leapSeq" or "leapForward"
                           tuneGrid = data.frame(nvmax = 1:24),
                           trControl = train.control)

# Linear Regression with Backwards Selection
model_back <- caret::train(PM25 ~ DEM + CLC_1 + PS + RH + T + U + 
                             V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                             PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                             aod, 
                           data = entrena,
                           method = "leapBackward", 
                           tuneGrid = data.frame(nvmax = 1:24),
                           trControl = train.control)

# random forest
model_rf <- caret::train(PM25 ~ DEM + CLC_1 + PS + RH + T + U + 
                           V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                           PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                           aod, 
                         data = entrena,
                         method = 'rf',
                         trControl = train.control,
                         importance = TRUE,
                         ntree = 100)  #num.threads = 7 #<- parametro para modificar cores

# kNN
model_knn <- caret::train(PM25 ~ DEM + CLC_1 + PS + RH + T + U + 
                            V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                            PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                            aod, 
                          data = entrena, 
                          trControl = train.control, 
                          method = "knn",
                          tuneLength = 12)

# Decision Tree
model_dt <- caret::train(PM25 ~ DEM + CLC_1 + PS + RH + T + U + 
                           V + BCSMASS + DMSSMASS + DUSMASS + SO4SMASS + SSSMASS25 + 
                           PRECTOT + SPEED + CLDHGH + CLDLOW + H1000 + 
                           aod,  
                         data = entrena, 
                         trControl = train.control, 
                         method = "rpart")



