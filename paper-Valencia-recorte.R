# Paper Valencia
# Autor: Sol Represa
# Fecha: 04/07/2020

# RECORTE



library(caret)
library(randomForest)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 6. Generar set de prueba  #####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# con datos altos >> Tabla corregida con nueva variables de MERRA
# tiene 9 veces más datos q la anterior

base <- read.csv("C:/Users/solre/Desktop/prueba/variables_estacion_aod_IDW_modelo_paper.csv")


set.seed(123) #seteamos para obtener resultados reproducibles

i_entrena <- createDataPartition(y = base$PM25, 
                                 p = 0.7, 
                                 list = FALSE)
entrena <- base[i_entrena,]
test <- base[-i_entrena,]


rm(i_entrena, base)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

set.seed(123)

# Ver tamaño memoria
# memory.size()
# memory.limit()
# memory.limit(size = 2500)

# Random forest con 10 fold cv - library(caret) ####

train.control <- trainControl(method = "cv", #"repeatedcv", 
                              number = 10,
                              # repeats = 10,
                              search = "random") # Set up repeated k-fold cross-validation

model_rf <- train(PM25 ~ ., 
                  data = entrena,
                  method = 'rf', 
                  trControl = train.control,
                  #tuneGrid = data.frame(mtry = 1:34),
                  ntree = 100,
                  importance = TRUE)


#>>> GUARDAR
saveRDS(model_rf, file = "modelo_rf_100tree_IDW.rds")


