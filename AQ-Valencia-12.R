# AQ-Valencia-10
# Sol Represa
# 23/01/2019


# Objetivo:
# Categorizar las estaciones de monitoreo continuo 
# de la calidad del aire ubicadas en la Comunidad Valenciana.
# Se generó buffer en los sitios que se utilizó para cortar un shape con datos de uso de suelo.
# Como información de usos de suelo se uso el mapa de ocupación Corine Land Cover 2012
# Eso se realizó en QGIS. 


library(ggplot2)
library(dplyr)
library(data.table)

estaciones <- read.csv("estaciones_usos.csv", sep =",", dec=".")
estaciones_clases <- read.csv("estaciones_clases.csv", sep =",", dec=".")

levels(estaciones_clases$Cod) <- c("Pla", "Raba", "Beni",  "Elx", "Elda", "Alcora",  "Alma", "Ben", "Bur", "Peny", "Grau",
                                "Pobla", "SJ", "VCid", "Viver", "Zorita", "Alba", "Buniol", "Caude", "Quart", "Sagunt",
                                "Pista", "Poli", "Moli", "Villar")



# estaciones <- read.csv("C:/Users/narep/Desktop/SOL/AQ-Valencia/estaciones_clases.csv", sep =",", dec=",")
# names(estaciones)[7] <- "Area"  #para estaciones_clases.csv
# names(estaciones)[6] <- "Clases"  #para estaciones_clases.csv

# esta_area <- estaciones %>% group_by(Cod) %>% summarise( area_tot = round(sum(Area), 2))
# estaciones <- merge(estaciones, esta_area, all =TRUE)
# estaciones$Porc_clase <-  round(estaciones$Area * 100 / estaciones$area_tot, 1)
# estaciones <- estaciones[,-2]
# estaciones$Clases <- factor(estaciones$Clases, 
#            labels = c("A - Superficie artificial", "B - Zonas agricolas", 
#            "C - Bosques y areas seminaturales",
#            "D - Humedales", "E - Cuerpos de agua"))


# write.csv(salida, "estaciones_usos.csv", row.names = FALSE)
# write.csv(estaciones, "estaciones_clases.csv", row.names = FALSE)

# 1. Porcentaje de usos de suelo en cada estación

ggplot(data = estaciones_clases, aes(x= Cod, 
                                     y = Porc_clase, 
                                     fill = Clases)) + 
  geom_bar(stat = "identity", color = "white") +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()) + 
 # geom_text(aes(label = paste0(Porc_clase,"%")), size=2) +
  labs( y = "", x= "") 


# 2. Grafica de torta

ggplot(estaciones_clases %>% group_by(Cod, Clases, Porc_clase) %>% summarise(), 
       aes(x = factor(1) , y = Porc_clase, fill = Clases))+
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()) +
  facet_wrap(~Cod) +
  labs( y = "", x= "") 



# Para ver qué estaciones pertenecen a cada Clase
a <- estaciones_clases %>% 
  group_by(Cod, Porc_clase)  %>% 
  filter( Porc_clase > 60) %>%
  select(Cod, Clases, Porc_clase)

View(a)