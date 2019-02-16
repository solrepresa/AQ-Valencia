### Calidad del Aire en Comunitat Valenciana
### 15/02/2019 Valencia, Spain
### Sol Represa
### Archivo 20


## Objetivo: trabajar con shape lineas ferrocarril

library(rgdal)
library(rgeos)
library(GISTools)
library(lubridate)



## Las bases de datos fueron descargadas de: http://centrodedescargas.cnig.es 
## Se eliminaron campos utilizando QGIS para aligerar los shapefiles

ali <- "C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\RUTAS\\tren\\AL_FFCC\\rt_tramofc_linea.shp"
cas <- "C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\RUTAS\\tren\\CS_FFCC\\rt_tramofc_linea.shp"
val <- "C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\RUTAS\\tren\\RT_FFCC\\rt_tramofc_linea.shp"

ls <- c(ali, cas, val)
names_ls <- c("ali", "cas", "val")



# # # # # # # # # # # # # # # # # # # # # # # # 

# 1) Separar shape por categorias de calles + genera rmd con clave de clases

# # # # # # # # # # # # # # # # # # # # # # # # 

clases_tren <-list()

for( j in 1:length(ls)){
  tabla <- readOGR(ls[j]) #"+proj=longlat +ellps=GRS80 +no_defs "
  
  clases <- levels(tabla$tipo_lineD)
  clases_tren[[j]] <- clases
  
  for( i in 1:length(clases)){
    clase_tabla <- tabla[ which(tabla$tipo_lineD == clases[i]), ]
    clase_tabla$clase <- clases[i]
    clase_tabla <- spTransform(clase_tabla, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
    
    nombre <- paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\RUTAS\\salida\\tren\\tren_nivel_",i,"_", names_ls[j], sep="" )  # atencion de cambiar nombres
    maptools::writeSpatialShape(clase_tabla , nombre)
    
  }
  
  rm(tabla, clase_tabla)
  
}

saveRDS(clases_tren, "clases_tren.rds") 
# Atencion! en ali la clase 6 corresponde a urbano, no a carril_bici


# # # # # # # # # # # # # # # # # # # # # # # # 

# 2) Unir shapes

# # # # # # # # # # # # # # # # # # # # # # # # 

fs <- list.files(path="C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\RUTAS\\salida\\tren", pattern = ".shp", full.names = TRUE)
fs_clases <- substr(fs, 82, 82)
fs_ciudad <- substr(fs, 84, 86)

fs_cond <- data.frame(fs_clases, fs_ciudad)

clases_tren <- readRDS("clases_tren.rds")
clases_tren <- clases_tren[[1]]
clases_tren[1] <- "Tranvia"

ls <- c("ali", "cas", "val")


k = 1 # tranvia 
lista <- fs[fs_cond$fs_clases == k & fs_cond$fs_ciudad == ls[1] ]
a <- readOGR( dsn = lista) 
proj4string(a) <- CRS("+proj=longlat +ellps=GRS80")
a <- spTransform(a, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
writeOGR(a , 
         layer = ".",
         dsn = paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\RUTAS\\", clases_tren[k], ".shp", sep = ""), 
         driver = "ESRI Shapefile")


k = 1 # metro solo esta en castellon (?)
lista <- fs[fs_cond$fs_clases == k & fs_cond$fs_ciudad == ls[2] ]
a <- readOGR( dsn = lista) 
proj4string(a) <- CRS("+proj=longlat +ellps=GRS80")
a <- spTransform(a, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
writeOGR(a , 
         layer = ".",
         dsn = paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\RUTAS\\Metro.shp", sep = ""), 
         driver = "ESRI Shapefile")


k = 2 # tren
lista <- fs[fs_cond$fs_clases == k & fs_cond$fs_ciudad != ls[3] ]
a <- readOGR( dsn = lista[1]) 
proj4string(a) <- CRS("+proj=longlat +ellps=GRS80")
a <- spTransform(a, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

df_a <- data.frame(a)
  
b <- readOGR(dsn = lista[2])
proj4string(b) <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
b <- spTransform(b, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
  
a <- gUnion(a, b)  #unir el segundo
    
df_b <- data.frame(b)
df_a <- rbind(df_a, df_b)


lista <- fs[fs_cond$fs_clases == 1 & fs_cond$fs_ciudad == ls[3] ] # juntar con Valencia
c <- readOGR(dsn = lista)
proj4string(c) <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
c <- spTransform(c, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

a <- gUnion(a, c)  # Unir el tercero

df_c <- data.frame(c)
df_a <- rbind(df_a, df_c)

spdf <- SpatialLinesDataFrame(a, df_a)
writeOGR(spdf , 
           layer = ".",
           dsn = paste("C:\\Users\\narep\\Desktop\\SOL\\aire_comunitat\\variables\\RUTAS\\", clases_tren[k], ".shp", sep = ""), 
           driver = "ESRI Shapefile")

