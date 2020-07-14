### Calidad del Aire en Comunitat Valenciana
### 06/03/2019 Valencia, Spain
### Sol Represa
### Archivo 27


library(keras)
library(cloudml)



# Datos modelo
load("dataset.RData")


gcloud_install()

cloudml_train("train.R")






# # # # # # # # # # # # # # # # # # # # # # 

## Prueba con modelo lineal sencillo 

# # # # # # # # # # # # # # # # # # # # # # 

# Modelo completo
#full_model <- lm( log(PM25) ~ ., data = tabla )

# Modelo de regresion Stepwise
#step_model <- stepAIC(full_model, direction = "both", trace = FALSE)

#summary(step_model) #0.43





# # # # # # # # # # # # # # # # # # # # # # 

# PARALELIZACIÓN DE PROCESO

# # # # # # # # # # # # # # # # # # # # # # 


# library(doMC)
# registerDoMC(cores = 4)



# # # # # # # # # # # # # # # # # # # # # # 

## Modelos

# # # # # # # # # # # # # # # # # # # # # # 

# Cross - validation

train.control <- trainControl(method = "repeatedcv", 
                              number = 3,
                              repeats = 10,
                              search = "random",
                              allowParallel = TRUE) # Set up repeated k-fold cross-validation


# Variacion: LOOCV
# k may be set to the total number of observations in the dataset



# ¿Qué modelo?

# VER: http://topepo.github.io/caret/train-models-by-tag.html#Linear_Regression
# 'svmLinear' >> Support Vector Machines with Linear Kernel // requiere: kernlab



set.seed(99) #seteamos para obtener resultados reproducibles

# Linear Regression model
model_lm <- caret::train(PM25 ~ . , 
                         data = entrena,
                         method = "lm",
                         preProcess= c("center", "scale"))

# Linear Regression with Stepwise Selection
model_step <- caret::train(PM25 ~ . , 
                           data = entrena,
                           method = "leapSeq", # "leapBackward","leapSeq" or "leapForward"
                           tuneGrid = data.frame(nvmax = 1:60),
                           preProcess = c("center", "scale"),
                           trControl = train.control)

# Linear Regression with Backwards Selection
model_back <- caret::train(PM25 ~ . , data = tabla,
                           method = "leapBackward", 
                           tuneGrid = data.frame(nvmax = 1:60),
                           preProcess = c("center", "scale"),
                           trControl = train.control)

# random forest
model_rf <- caret::train(PM25 ~ . , 
                         data = entrena,
                         method = 'rf', 
                         preProcess = c("center", "scale"),
                         trControl = train.control,
                         #tuneGrid = data.frame(mtry = 1:34),
                         ntree = 10,
                         num.threads = 7) #<- parametro para modificar cores

# kNN
model_knn <- caret::train(PM25 ~ ., 
                          data = entrena, 
                          trControl = train.control, 
                          method = "knn", 
                          preProcess = c("center", "scale"),
                          tuneLength = 12)

# Decision Tree
model_dt <- caret::train(PM25 ~ ., 
                         data = entrena, 
                         trControl = train.control, 
                         method = "rpart",
                         preProcess = c("center", "scale"))



# # # # # # # # # # # # # # # # # # # # # # 

## Predicciones

# # # # # # # # # # # # # # # # # # # # # # 

pred_lm <- predict(model_lm, test)
pred_step <- predict(model_step, test)
pred_back <- predict(model_back, test)
pred_rf <- predict(model_rf, test)
pred_knn <- predict(model_knn, test)
pred_dt <- predict(model_dt, test)



# # # # # # # # # # # # # # # # # # # # # # 

## Resultados

# # # # # # # # # # # # # # # # # # # # # # 


pred = predict(my_lm, test[, 1:4])

parametros <- function(pred, obs){
  SSE = sum((obs - pred)^2)    # sum of squared errors
  SST = sum((obs - mean(obs))^2) # total sum of squares, remember to use training data here
  R_square = 1 - SSE/SST
  
  message('R_squared on the test data:')
  print(round(R_square, 2))
  
  SSE = sum((obs - pred)^2)
  RMSE = sqrt(SSE/length(pred))
  
  message("Root mean square error on the test data: ")
  print(round(RMSE, 2))
  
}


message("Linear Regression: Performance del modelo de entrenamiento")
model_lm$results[c("RMSE","Rsquared")] %>% round(2)

message("Stepwise: Performance del modelo de entrenamiento")
model_step$results[c("RMSE","Rsquared")] %>% round(2)

message("BackForward: Performance del modelo de entrenamiento")
model_back$results[c("RMSE","Rsquared")] %>% round(2)


message("Random Forest: Performance del modelo de entrenamiento")
model_rf$results[c("RMSE","Rsquared")] %>% round(2)


message("k-NN: Performance del modelo de entrenamiento")
model_knn$results[c("RMSE","Rsquared")] %>% round(2)

message("Decision Tree: Performance del modelo de entrenamiento")
model_dt$results[c("RMSE","Rsquared")] %>% round(2)





# # # # # # # # # # # # # # # # # # # # # # 

## Plots

# # # # # # # # # # # # # # # # # # # # # # 



my_data <-  as.data.frame(cbind(predicted = pred_lm,
                                observed = test$PM25))

# Plot predictions vs test data
ggplot(my_data,aes(predicted, observed)) +
  geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Linear Regression ') +
  ggtitle("Linear Regression: Prediction vs Test Data") +
  xlab("Predecited") +
  ylab("Observed") +
  theme(plot.title = element_text(color="darkgreen",size = 18,hjust = 0.5),
        axis.text.y = element_text(size = 12),
        
        axis.text.x = element_text(size = 12,hjust=.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))




# # # # # # # # # # # # # # # # # # # # # # 

## Guardar modelos

# # # # # # # # # # # # # # # # # # # # # # 

datos_lm <- data.frame(ID = row.names(test), PM25_modelado = pred_lm)
datos_step <- data.frame(ID = row.names(test), PM25_modelado = pred_step)
datos_back <- data.frame(ID = row.names(test), PM25_modelado = pred_back)
datos_rf <- data.frame(ID = row.names(test), PM25_modelado = pred_rf)
datos_knn <- data.frame(ID = row.names(test), PM25_modelado = pred_knn)
datos_dt <- data.frame(ID = row.names(test), PM25_modelado = pred_dt)



write.csv(datos_lm, file = "modelo_lm.csv", row.names = F)
write.csv(datos_step, file = "modelo_step.csv", row.names = F)
write.csv(datos_back, file = "modelo_back.csv", row.names = F)
write.csv(datos_rf, file = "modelo_rf.csv", row.names = F)
write.csv(datos_knn, file = "modelo_knn.csv", row.names = F)
write.csv(datos_dt, file = "modelo_dt.csv", row.names = F)




step_model
plot(step_model)

step_model$results   # con valores cuadrados
step_model$bestTune

summary(step_model$finalModel)

coef(step_model$finalModel,33)


## 3) Predecimos


plot(test$PM25, pred)
abline( 0, 1, col = "red")
abline( h = 0)
abline( v = 0)

a <- lm(pred ~ test$PM25)
summary(a)








# # # # # # # # # # # # # # # # # # # # # # 

## Random Forest
# "There is no performance penalty for having more trees, it just takes longer"

# # # # # # # # # # # # # # # # # # # # # # 

library(randomForest)

set.seed(101) #seteamos para obtener resultados reproducibles
mod_random <- randomForest(PM25 ~ ., 
                           data = entrena,
                           ntree = 500,    
                           mtry = 60,  
                           replace = T)  


print(mod_random)   # % Var explained: 53.33  # Mean of squared residuals: 19.88794
varImpPlot(mod_random)
importance(mod_random)


# Grafico del error OOB en cada iteracion << para decir cantidad de variables 
tuneRF(x = entrena,       # data set
       y = entrena$PM25,  
       mtryStart  = 1,   # cantidad de variables inicial 
       stepFactor = 2,   # incremento de variables
       ntreeTry   = 100, # cantidad arboles a ejecutar en cada iteracion
       improve    = .01  # mejora minina del OOB para seguir iteraciones
)


pp <- predict(mod_random, test)

plot(test$PM25, pp)





# # # # # # # # # # # # # # # # # # # # # # 

## Bayesian Generalized Linear Model  ####

# # # # # # # # # # # # # # # # # # # # # # 

library(arm)   # Bayesian Generalized Linear Model

set.seed(99) #seteamos para obtener resultados reproducibles
train.control <- trainControl(method = "repeatedcv", 
                              number = 10,
                              repeats = 10) # Set up repeated k-fold cross-validation

arm_model <- caret::train(PM25 ~ . , 
                          data = entrena,
                          method = 'bayesglm', 
                          trControl = train.control)

arm_model
arm_pred <- predict(arm_model, test)   # RMSE = 4.7
plot(test$PM25, arm_pred)   












library(keras)

sptensor( )

c(train_data, train_labels) %<-% data$train


history <- model %>% fit(x_train, y_train,
                         batch_size = 128,  # feed 128 samples at time to the model
                         epochs = 10,  #  traverse de imput dataset 10 times
                         validation_split = 0.2) # hould 20% of data for validation

plot(history)

model %>% evaluate(x_test, y_test)
model %>% predict_classes(x_test[1:100,])

#layer_convolusion_2d()




# # # # # # # # # # # # # # # # # # # # # # 

### TOOLS

# # # # # # # # # # # # # # # # # # # # # # 


# work with GPU



## Para traer todos los modelos
library(tfruns)
training_run(".R")

ls_runs()
ls_runs(eval_acc > 0.95, order = eval_acc)
view_run()
compare_runs()


# # # # # # # # # # # # # # # # # # # # # # 

# cloudml

library(cloudml)
cloudml_train( " .R")
cloudml_train( ".R", master_type = "standard_gpu")
cloudml_train( ".R", master_type = "standard_p100")