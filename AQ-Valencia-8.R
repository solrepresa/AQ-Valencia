# Sol Represa
# AQ Valencia
# 25/01/2019
# Modificado 8/8/19 La Plata, Argentina


# Objetivo: Correlacion y PCA

library(psych)
library(dplyr)
library(ggplot2)
library(reshape2)
library(pvclust)

medias_dia <- readRDS("medias_dia_est_utiles.Rds")
pollutant_names <- readRDS("pollutant_names.Rds")
pollutant <- readRDS("pollutant_ut.Rds")

sitios <- read.csv("estaciones_utiles.csv")
sitios <- data.frame( Estacion = levels(sitios$Estacion), 
                      Abrev = c("Pla", "Raba", "Beni",  "Elx", "Elda", "Alcora",  "Alma", "Ben", "Bur", "Peny", "Grau",
                    "Pobla", "SJ", "VCid", "Viver", "Zorita", "Alba", "Buniol", "Caude", "Quart", "Sagunt",
                    "Pista", "Poli", "Moli", "Villar"))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## 1 - Analisis de correlacion  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


source("/home/usuario/Sol/AQ-Valencia/fuction/cor_mtest.R")

#x = medias_dia
x = pollutant

g <- list()


## Algoritmo para generar plot de correlación:

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))  # Rampa de colores
method = "spearman"                        # "spearman" "kendall" o "pearson"

for(i in 1:length(x)){
  tabla <- x[[i]]
  tabla <- tabla[-c(1,2)]    # cuando uso POLLUTANT, si uso medias_dia bloquear! <<<<<<<<<<<<<<<<<<<
  # tabla <- tabla[-1]       # paa cuando uso "medias_dia"
  
  names(tabla) <- sitios[which(sitios$Estacion %in% names(tabla)), 2]
  
  tabla_cor <- cor( tabla[, 1:length(tabla)] , 
                    use= "pairwise.complete.obs")
  p.mat <- cor_mtest(tabla[, 1:length(tabla)], method = method)  
  
  # Grafica correlacion
  png(paste("corrplot_", pollutant_names[i],"_", method , ".png", sep="") , width = 480, height = 450)
  corrplot(tabla_cor,
           method = "color", 
           title = paste(pollutant_names[i], method, sep=" ") ,
           col = col(200),
           diag=FALSE, # tl.pos="d", 
           type="upper", 
           order="original",
           addCoef.col = "black", # Add coefficient of correlation
           p.mat = p.mat, 
           cl.cex = 0.6, #cambia tamaño letra leyenda
           tl.cex = 0.6,  #cambia tamaño letra estaciones
           number.cex = 0.5, #cambia tamaño letra correlacion
           number.font = 1,
           sig.level = 0.05, 
           insig = "pch", 
           mar=c(0,0,1,0) )
  
  dev.off()
}


### ATENCION nombres gráficos:
# 1- datos horarios
# 2- medias diarias



# Grafica correlacion 2  # # # # # # # # # # # # # # # #

for(i in 1:length(x)){
  tabla <- x[[i]]
  tabla <- tabla[-c(1,2)]    # cuando uso POLLUTANT, si uso medias_dia bloquear! <<<<<<<<<<<<<<<<<<<
  # tabla <- tabla[-1]       # paa cuando uso "medias_dia"
  
  names(tabla) <- sitios[which(sitios$Estacion %in% names(tabla)), 2] # Cambiar nombres
  
  tabla_cor <- cor( tabla[, 1:length(tabla)] , 
                    use= "pairwise.complete.obs")
  
  tabla_cor[tabla_cor == 1] <- NA
  
  # Grafica histograma de correlacion
  #png(paste("hist_corrplot_", pollutant_names[i],"_", method , ".png", sep="") , width = 480, height = 450)
  #hist(tabla_cor)
  #dev.off()
  print(c(pollutant_names[i], round(mean(tabla_cor, na.rm = TRUE), 3), 
          "(", round(min(tabla_cor, na.rm = TRUE ), 3), ",", round(max(tabla_cor, na.rm = TRUE), 3), ")"))
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #





############################################################

## 2) Hirarquical Cluster en matriz ####

#https://www.statmethods.net/advstats/cluster.html


# Hierarchical Clustering with p-value

# based on multiscale bootstrap resampling. 
# Clusters that are highly supported by the data will have large p values.


###########################################################


## 2.1 Hirarquical Cluster en matriz de corelación ####

x = pollutant

l <- list()
for(i in 1:length(x)){   #problemas con i=7 por falta de datos
  tabla <- x[[i]]
  tabla <- tabla[-c(1,2)]    # cuando uso POLLUTANT, si uso medias_dia bloquear! <<<<<<<<<<<<<<<<<<<
  
  names(tabla) <- sitios[which(sitios$Estacion %in% names(tabla)), 2] # Cambiar nombres
  
  mydata <- cor( tabla[,1:length(tabla)] , 
                 use= "pairwise.complete.obs")
  

  set.seed(1)
  # Dendrogramas con linkage complete y average
  fit <- pvclust(mydata, method.hclust="average",
                 method.dist="euclidean")
  
  # Generar gráfica
  png(paste("cluster_r_", pollutant_names[i] , ".png", sep="") , width = 600, height = 500)
  plot(fit) # dendogram with p values
  pvrect(fit, alpha=.95)   # add rectangles 
  dev.off()
  
  # para ver cuantos datos se usaron
  #valores[i,1] <- pollutant_names[i] 
  #valores[i,2] <- dim(tabla)[1]
  
  # guardar cluster formados
  l[[i]] <- pvpick(fit)$clusters
  
}


#saveRDS(l, file="cluster_estaciones.Rds")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# CLUSTERs generados sobre matriz de correlacion

nombres <- c( "X03014006","X03014009", "X03031002", "X03065006", "X03066003", "X12005005",
              "X12009007", "X12028001", "X12032001", "X12040008", "X12040010", "X12093004",
              "X12099001", "X12129001", "X12140002", "X12141002", "X46010001", "X46077006", 
              "X46095001", "X46102002", "X46220010", "X46250030", "X46250046", "X46250048",
              "X46258001")

NO = c("A", "-", "C","A", "A", "A", "A", "A", "A", "D", "A", "E", "B", "B", "F", 
      "-", "G", "H" , "I", "A", "A", "A", "-", "-","J")

NO2 = c("A", "-", "A", "A", "A", "A" ,"A", "A", "A", "A", "A", "B", "C", "D", "A",
       "-", "A", "A", "A", "A", "A", "A", "-", "-", "A")

O3 = c("A", "A", "A", "A", "A", "A", "-", "-", "A", "A", "A", "B", "A", "A", "A", "A",
       "A", "A", "A", "A", "A", "A", "A", "A", "A")

SO2 = c("A", "A", "-", "A", "A", "A", "A", "A", "A", "-", "A", "B", "-", "A", "A", "C",
        "A", "A", "A", "A", "A", "-", "A", "A", "A")

PM10 = c("-", "C", "-", "-", "A", "-", "B", "B", "-", "B", "-", "-", "-", "-", "A", "A", 
         "-", "A", "A", "-", "B", "D", "B", "B", "A")

PM25 = c("-", "B", "-", "-", "B", "C", "B", "B", "-", "B", "-", "-", "-", "-", "A", "A",
          "B", "A", "A", "-", "B", "-", "B", "B", "A")

cluster <- data.frame(nombres, NO, NO2, O3, PM10, PM25)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



## 2.1 Hirarquical Cluster sobre datos####

x = pollutant

l <- list()
for(i in 1:length(x)){   #problemas con i=7 por falta de datos
  tabla <- x[[i]]
  tabla <- tabla[-c(1,2)]    # cuando uso POLLUTANT, si uso medias_dia bloquear! <<<<<<<<<<<<<<<<<<<
  names(tabla) <- sitios[which(sitios$Estacion %in% names(tabla)), 2] # Cambiar nombres

  #remove columnas vacías
  mydata <- tabla[!sapply(tabla, function(x) all(x == ""))]
  
  #mydata <- complete.cases( tabla[,2:length(tabla)])
  
  #mydata <- apply(mydata, 2, scale) # no siempre escalara es deseable.
  
  set.seed(1)
  # Dendrogramas con linkage complete y average
  fit <- pvclust(mydata, method.hclust="average",
                 method.dist="euclidean")
  
  # Generar gráfica
  png(paste("cluster_datos_", pollutant_names[i] , ".png", sep="") , width = 600, height = 500)
  plot(fit) # dendogram with p values
  pvrect(fit, alpha=.95)   # add rectangles 
  dev.off()
  
  # para ver cuantos datos se usaron
  #valores[i,1] <- pollutant_names[i] 
  #valores[i,2] <- dim(tabla)[1]
  
  # guardar cluster formados
  l[[i]] <- pvpick(fit)$clusters
  
}





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## 3 - Analisis de PCA  ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# 3.1) PCA - Analisis de componentes


x = pollutant
pollutant_pca <- list()

for(i in 1:length(x)){
  tabla <- x[[i]]
  tabla <- tabla[-c(1,2)]   
  tabla <- tabla[!sapply(tabla, function(x) all(x == ""))] #por si tengo columnas vacias

  ### Pruebas estadisticas 
  
  # Test de Esfericidad de Barlett 
  # cortest.bartlett(tabla_cor, n=nrow(tabla)) # el p <0,05 solo si Mardia dio NORMAL
  
  # KMO 
  # KMO(tabla_cor) # KMO > 0,9 Adecuado 
  
  # KMO high proportion of variance in your variables that might be caused by underlying factors
  
  ### PCA
  
  pca <- princomp(na.omit(tabla[1:length(tabla)]), cor=TRUE) #PCA en la matriz de covarianza
  plot (pca, type="l", main="Gráfica de sedimentación")

  png(paste("PCA_", pollutant_names[i] , ".png", sep="") , width = 320, height = 220)
  qplot(c(1:length(pca$sdev)), round(pca$sdev^2/sum(pca$sdev^2),2)) + 
    geom_line() + 
    xlab("PC") + 
    ylab("Varianza explicada") +
    ggtitle(pollutant_names[i]) +
    ylim(0,1) + 
    theme_bw()
  dev.off()
  
  png(paste("PCA_cum_", pollutant_names[i] , ".png", sep="") , width = 320, height = 220)
  qplot(c(1:length(pca$sdev)), cumsum(round(pca$sdev^2/sum(pca$sdev^2),2)))  + 
    geom_hline( yintercept = 0.7, col="red", linetype="dashed" ) + 
    geom_line() + 
    xlab("PC") + 
    ylab("Varianza acumulada") + 
    ggtitle(pollutant_names[i]) +
    ylim(0, 1.1) + 
    theme_bw()
  dev.off()
  
  pollutant_pca[[i]] <- pca$loadings
  
}



## 3.2 ) Grafica de pesos ####

# Analisis de representación de las estaciones por componentes


i=1

PC <- c(10, 8, 8, 1, 4, 5, 11)  #los obtuve de analizar las graficas creadas en punto anterior
x = pollutant
orden <- c("RC1", "RC2",  "RC3",  "RC4",  "RC5",  "RC6",  "RC7",  "RC8",  "RC9",  "RC10" )

for( i in 1:length(pollutant)){
  tabla <- x[[i]]
  tabla <- tabla[-c(1,2)]  
  names(tabla) <- sitios[which(sitios$Estacion %in% names(tabla)), 2] # Cambiar nombres
  
  tabla <- tabla[!sapply(tabla, function(x) all(x == ""))] #por si tengo columnas vacias
  
  set.seed(123)
  vari <- principal(na.omit(tabla[1:length(tabla)]), nfactors= PC[i], 
                    rotate="varimax", covar = FALSE)
  
  
  pesos <- with(vari, unclass(loadings))
  pesos <- as.data.frame(pesos) #convierto para trabajar con ggplot
  pesos <- add_rownames(pesos, "indicadores")   #recupera la row.name como columna
  pesos_m <- reshape2::melt(pesos, value.name= "Carga")
  pesos_m$Carga <- round(pesos_m$Carga,2)
  
  pesos_m$variable <- factor(pesos_m$variable, levels = orden[1:(length(pesos)-1)])
  
  pesos_m[which(pesos_m$Carga < 0.4 ), 3] = NA
  
  png(paste("PCA_CP_", pollutant_names[i] , ".png", sep="") , width = 420, height = 370)
  ggplot(data = pesos_m, aes(x = variable, y = indicadores)) + 
    geom_tile(aes(fill = Carga)) +
    scale_fill_continuous(low="thistle2", high ="darkred", na.value = "transparent") + 
    xlab("") + 
    ylab("") + 
    #scale_x_discrete( labels =  orden[1:(length(pesos)-1)]) +
    geom_text(aes(label= Carga*100)) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(title= pollutant_names[i])
  dev.off()
  
}






###########################################################################

# A continuacion scrip que compara 2 funciones en 2 paquetes de R     # # # # #
# ATENTI: ver relacion entre funciones de R                           # # # # #

i=1

# "princomp()" es de paquete stat meintras "principal()" es de psych

# estas 2 expresiones son iguales!
pca <- princomp(na.omit(tabla[1:length(tabla)]), cor = FALSE) #PCA en la matriz de covarianza
pca15 <- principal(na.omit(tabla[1:length(tabla)]), nfactors=21, 
                   rotate="none", use="none", covar = TRUE )

# para verlo podemos hacer:
a <- pca$loadings[1:3,1:5]
b <- pca15$loadings[1:3,1:5]
a/b #hay un factor escalar 

# y también:
pca$sdev
pca15$loadings


# estas 2 expresiones también son iguales =
pca <- princomp(na.omit(tabla[1:length(tabla)]), cor=TRUE) #PCA en la matriz de covarianza
pca15 <- principal(na.omit(tabla[1:length(tabla)]), nfactors=21, 
                   rotate="none", use="none", covar = FALSE)

###########################################################################







tabla[35065:43848]
