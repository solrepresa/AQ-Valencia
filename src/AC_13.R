####### Calidad del Aire en Comunitat Valenciana
####### 08/02/2019 Valencia, Spain
####### Sol Represa
####### Archivo 13




# Objetivo: - Conocer los horarios de las orbitas por satelite


library(lubridate)


orbitas <- read.csv("orbitas_R.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)


satelite <- data.frame()
for( i in 1:nrow(orbitas)){
  orb_file <- strsplit(as.character(orbitas[i,2]), "  ")[[1]]   # del string separa en pedacitos y convierte en vector
  orb_satelite <- substring(orb_file, 12, 13)  #tomar dato del satelite
  orb_hora <- substring(orb_file, 1, 11) #tomar dato de la fecha de las orbitas
  orb_hora  <- strptime(orb_hora, tz= "GMT", format = "%Y%j%H%M")
  
  
  satelite_data <- data.frame(orb_hora, orb_satelite)
  satelite <- rbind(satelite, satelite_data)
}


satelite$hora <- hour(satelite$orb_hora)
levels(satelite$orb_satelite) <- c( "Aura", "Terra")

# Histograma de paso del satelite 
ggplot(data = satelite) + 
  geom_histogram( aes( x = hora), binwidth = 1, center = 0, fill = "#777777", col = "#000000") +
  facet_grid( ~ orb_satelite) +
  theme_bw() + labs( x = "", y = "" ) + 
  scale_x_continuous(breaks = c(8, 9, 10, 11, 12, 13, 14, 15), 
                     labels = c("8 h", "9 h", "10 h", "11 h", "12 h", "13 h", "14 h", "15 h")) +
  theme(axis.text.x = element_text(vjust = 0.4, hjust = 0.5),
      legend.position = "none")

