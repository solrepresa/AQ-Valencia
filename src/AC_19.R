### Calidad del Aire en Comunitat Valenciana
### 14/02/2019 Valencia, Spain
### Sol Represa
### Archivo 19


## Objetivo: trabajar con shape rutas + shape lineas ferrocarril

library(rgdal)
library(rgeos)
library(GISTools)
library(lubridate)



## Las bases de datos fueron descargadas de: http://centrodedescargas.cnig.es 
## Se eliminaron campos utilizando QGIS para aligerar los shapefiles

ali <- "C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\RUTAS\\via\\AL_VIARIA\\rt_tramo_vial.shp"
cas <- "C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\RUTAS\\via\\CS_VIARIA/rt_tramo_vial.shp"
val <- "C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\RUTAS\\via\\RT_VIARIA/rt_tramo_vial.shp"

ls <- c(ali, cas, val)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 1) Separar shape por categorias de calles + genera rmd con clave de clases

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

clases_rutas <-list()

for( j in 1:length(ls)){
  tabla <- readOGR(ls[j]) #"+proj=longlat +ellps=GRS80 +no_defs "
  
  clases <- levels(tabla$claseD)
  clases_rutas[[j]] <- clases
  
  for( i in 1:length(clases)){
    clase_tabla <- tabla[ which(tabla$claseD == clases[i]), ]
    clase_tabla$clase <- clases[i]
    clase_tabla <- spTransform(clase_tabla, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
    
    nombre <- paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\RUTAS\\salida\\ruta_nivel_",i,"_", names(ls)[j], sep="" )  # atencion de cambiar nombres
    maptools::writeSpatialShape(clase_tabla , nombre)
    
  }
  
  rm(tabla, clase_tabla)
  
}

saveRDS(clases_rutas, "clases_rutas.rds") 
# Atencion! en ali la clase 6 corresponde a urbano, no a carril_bici


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 2) Unir shapes ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

fs <- list.files(path="C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\RUTAS\\salida", pattern = ".shp", full.names = TRUE)
fs_clases <- substr(fs, 70,70)

clases_rutas <- readRDS("clases_rutas.rds")
clases_rutas <- clases_rutas[[2]]
clases_rutas[2] <- "Autovia"
clases_rutas[4] <- "Carr_Conv"
clases_rutas[5] <- "Carr_mult"
clases_rutas[6] <- "Carril_bici"


ls <- c("ali", "cas", "val")


for( k in 2:5){   # Atencion! en ali la clase 6 corresponde a urbano, no a carril_bici
  
  lista <- fs[fs_clases == k]
  a <- readOGR( dsn = lista[1]) # doy inicio al bucle
  proj4string(a) <- CRS("+proj=longlat +ellps=GRS80")
  a <- spTransform(a, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
  
  df_a <- data.frame(a)
  
  for( l in 2: length(lista)){
    b <- readOGR(dsn = lista[l])
    proj4string(b) <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
    b <- spTransform(b, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
    
    a <- gUnion(a, b)  # unir lineas
    
    df_b <- data.frame(b)
    df_a <- rbind(df_a, df_b)
  }
  
  spdf <- SpatialLinesDataFrame(a, df_a)
  writeOGR(spdf , 
           layer = ".",
           dsn = paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\RUTAS\\", clases_rutas[k], ".shp", sep = ""), 
           driver = "ESRI Shapefile")
}



# Caso clase 6

k = 7  # Atencion! en ali la clase 6 es urbano

fs_ciudad <- substr(fs, 71, 73)
fs_ind <- data.frame(fs_clases, fs_ciudad)

lista <- fs[fs_ind$fs_clases == k & fs_ind$fs_ciudad != "ali"]
a <- readOGR( dsn = lista[1]) #
proj4string(a) <- CRS("+proj=longlat +ellps=GRS80")
a <- spTransform(a, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

df_a <- data.frame(a)

for( l in 2: length(lista)){
  b <- readOGR(dsn = lista[l])
  proj4string(b) <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
  b <- spTransform(b, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
  
  a <- gUnion(a, b) 
  
  df_b <- data.frame(b)
  df_a <- rbind(df_a, df_b)
}



# >>>>>>>>>>>  La excepcion 

k = 6
excep <- fs[fs_ind$fs_clases == k & fs_ind$fs_ciudad == "ali"]
c <- readOGR(dsn = excep)
proj4string(c) <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
c <- spTransform(c, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

a <- gUnion(a, c) 

df_c <- data.frame(c)
df_a <- rbind(df_a, df_c)


spdf <- SpatialLinesDataFrame(a, df_a)
writeOGR(spdf , 
         layer = ".",
         dsn = paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\RUTAS\\", clases_rutas[k], ".shp", sep = ""), 
         driver = "ESRI Shapefile")



