### Calidad del Aire en Comunitat Valenciana
### 06/02/2019 Valencia, Spain
### Sol Represa
### Archivo 7

library(ggplot2)
library(dplyr)

# Objetivo:
# - Quedarnos con las estaciones que están incluidas en el estudio
# - Analisis de correlacion
# - Analisis de tendencia de AOD

# Se parte de las bases creadas en AC_3_bis.R
# "tierra_buff_MODIS_1km_25.csv"
# "tierra_MODIS_1km_25.csv"


#tabla <- read.table("tierra_buff_MODIS_1km_25.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)
tabla <- read.table("tierra_MODIS_1km_25.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)

tabla$date <- as.Date(tabla$date)
tabla$pollutant <- factor(tabla$pollutant)
tabla$estacion <- factor(tabla$estacion)


tabla_PM25 <- tabla %>% 
  filter(pollutant == "PM2.5") %>% 
  filter(estacion == "X03014009" | estacion == "X12005005" |
           estacion == "X03066003" | estacion == "X12009007" |
           estacion == "X12028001" | estacion == "X12040008" |
           estacion == "X12140002" | estacion == "X12141002" |
           estacion == "X46010001" | estacion == "X46077006" | 
           estacion == "X46095001" | # estacion == "X46220010" | # esta estacion da vacia
           estacion == "X46250046" | estacion == "X46250048" |
           estacion ==  "X46258001" )


tabla_PM10 <- tabla %>% 
  filter( pollutant == "PM10") %>%
  filter( estacion == "X03014009" | estacion == "X03066003" | estacion == "X12009007" | 
            estacion == "X12028001" | estacion == "X12040008" | estacion == "X12140002" | 
            estacion == "X12141002" | estacion == "X46077006" | estacion == "X46095001" | 
            # estacion == "X46220010" | # esta estacion da vacia
            estacion == "X46250030" | estacion == "X46250046" |
            estacion == "X46250048" | estacion ==  "X46258001")


# Nombres de estaciones
sitios <- read.csv("estaciones_utiles.csv")
sitios <- data.frame( Estacion = levels(sitios$Estacion), 
                      Abrev = c("Pla", "Raba", "Beni",  "Elx", "Elda", "Alcora",  "Alma", "Ben", "Bur", "Peny", "Grau",
                                "Pobla", "SJ", "VCid", "Viver", "Zorita", "Alba", "Buniol", "Caude", "Quart", "Sagunt",
                                "Pista", "Poli", "Moli", "Villar"))

levels(tabla_PM10$estacion) <- sitios[which(sitios$Estacion %in% levels(tabla_PM10$estacion)), 2]
levels(tabla_PM25$estacion) <- sitios[which(sitios$Estacion %in% levels(tabla_PM25$estacion)), 2]


#tabla_HREL <- tabla %>% filter(pollutant == "H.Rel.")

write.csv(tabla_PM10, "V_PM10_MODIS.csv", row.names = FALSE)
write.csv(tabla_PM25, "V_PM25_MODIS.csv", row.names = FALSE)
rm(tabla)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## 1 - Correlacionar datos de monitoreo continuo y MODIS ####
##     Todos los datos

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#tabla <- tabla_PM25
tabla <- tabla_PM10

cor.test(tabla$aod,tabla$mean, method = "kendall")   #
cor.test(tabla$aod,tabla$mean, method = "spearman") # 
cor.test(tabla$aod,tabla$mean, method = "pearson") # 




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## 2 - Plot de correlacion    ####

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


## 2.1 Plot correlacion TODOS los datos AOD vs PM

ggplot(data = tabla, aes(x = mean, y= aod, shape = estacion)) + 
  geom_point(na.rm = TRUE) + 
  theme_bw() +
  labs(y = "MODIS 550 nm AOD", x=expression(paste("PM"[10]," (" , mu,"g.m"^-3, ")")), 
       title = "Correlacion MODIS - Estaciones terrestres ")  +
  # geom_abline(aes(intercept=0, slope=1 )) +
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0)) + 
  geom_smooth(method='lm', se = FALSE, na.rm = TRUE, fullrange = TRUE) 




## 2.2 Plot de correlacion por estacion


# Calculate correlation for each group
cors_PM25 <-tabla_PM25[complete.cases(tabla_PM25),] %>% 
  group_by(estacion) %>%
  summarize(pearson = round(cor.test(mean, aod)$estimate, 2),
            pValue = cor.test(mean, aod)$p.value,
            spearman = round(cor.test(mean, aod, method = "spearman")$estimate, 2),
            pValue_s = cor.test(mean, aod, method = "spearman")$p.value)

cors_PM25[which(cors_PM25$pValue < 0.05), 3] <- "*"
cors_PM25[which(cors_PM25$pValue_s < 0.05), 5] <- "*"


cors_PM10 <-tabla_PM10[complete.cases(tabla_PM10),] %>% 
  group_by(estacion) %>%
  summarize(pearson = round(cor.test(mean, aod)$estimate, 2),
            pValue = cor.test(mean, aod)$p.value,
            spearman = round(cor.test(mean, aod, method = "spearman")$estimate, 2),
            pValue_s = cor.test(mean, aod, method = "spearman")$p.value)

cors_PM10[which(cors_PM10$pValue < 0.05), 3] <- "*"
cors_PM10[which(cors_PM10$pValue > 0.05), 3] <- ""

cors_PM10[which(cors_PM10$pValue_s < 0.05), 5] <- "*"
cors_PM10[which(cors_PM10$pValue_s > 0.05), 5] <- ""


## PM2.5

png("correlacion_PM25-AOD.png", width = 500, height = 500) 
tabla_PM25 %>%
  ggplot(aes( x = mean, y = aod)) + 
  geom_point(  na.rm = TRUE,  fill = "#999999") +
  #geom_abline(aes(intercept=0, slope=1 ), col = "#999999", alpha = 0.6) +
  geom_smooth(method = "lm", 
              na.rm = TRUE, 
              se = FALSE, col = "#0072B2") + 
  facet_wrap( ~estacion, ncol =  4 ) + 
  labs( x = expression(paste("PM"[2.5]," (" , mu,"g.m"^-3, ")")), 
        y = "MODIS 550 nm AOD",
        title = "") +
  theme_bw() +
  theme(axis.title = element_text(size = rel(0.8)),
        axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1)) +
  geom_text(data = cors_PM25, 
            aes(label = paste("r = ", pearson, pValue, sep="")), x= 120, y = 0.75) +
  geom_text(data = cors_PM25, 
            aes(label = paste("rho = ", spearman, pValue_s, sep="")), x= 120, y=0.55)

dev.off()


## PM10
png("correlacion_PM10-AOD.png", width = 500, height = 500) 
tabla_PM10 %>%
  ggplot(aes( x = mean, y = aod)) + 
  geom_point(  na.rm = TRUE,  fill = "#999999") +
  #geom_abline(aes(intercept=0, slope=1 ), col = "#999999", alpha = 0.6) +
  geom_smooth(method = "lm", na.rm = TRUE, se = FALSE, col = "#0072B2") + 
  facet_wrap( ~estacion, ncol =  4 ) +   #scales = "free 
  labs( x = expression(paste("PM"[10]," (" , mu,"g.m"^-3, ")")), 
        y = "MODIS 550 nm AOD",
        title = "") +
  theme_bw() +
  theme(axis.title = element_text(size = rel(0.8)),
        axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1)) +
  geom_text(data = cors_PM10, 
            aes(label = paste("r = ", pearson, pValue, sep="")), x= 280, y=0.8) +
  geom_text(data = cors_PM10, 
            aes(label = paste("rho = ", spearman, pValue_s, sep="")), x= 280, y=0.6)


dev.off()




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## 3 - Analisis de correlacion por estacion de monitoreo ####
##     PM2.5

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

tabla <- tabla_PM25

estaciones <- levels(tabla$estacion)
salida <- data.frame()

for( i in 1:length(estaciones)){
  a <- tabla %>%
    filter( estacion == estaciones[i])
  cor <- data.frame(estacion = estaciones[i],
                    cor_kendall = round(as.numeric(cor.test(a$aod,  a$mean, method = "kendall")$estimate), 2),
                    p_kendall = round(as.numeric(cor.test(a$aod,  a$mean, method = "kendall")$p.value), 4) ,
                    cor_spearman = round(as.numeric(cor.test(a$aod, a$mean, method = "spearman")$estimate), 2),
                    p_spearman = round(as.numeric(cor.test(a$aod, a$mean, method = "spearman")$p.value), 4) ,
                    cor_pearson = round(as.numeric(cor.test(a$aod, a$mean, method = "pearson")$estimate), 2),
                    p_cor = round(as.numeric(cor.test(a$aod, a$mean, method = "pearson")$p.value), 4),
                    N = as.numeric(cor.test(a$aod, a$mean, method = "pearson")$parameter) + 2)
  salida <- rbind(salida, cor)

}



#write.csv(salida, "correlacion_estacion.csv", row.names = FALSE)




