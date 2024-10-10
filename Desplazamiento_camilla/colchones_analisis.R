#Datos

setwd("C:\\Users\\c-lop\\Documents\\GitHub\\Radioterapia_SS\\Desplazamiento_camilla")

datos<-read.csv("colchones.csv",header=TRUE)

str(datos)

desplazamientos_col <- c("vrt", "lng", "lat", "cabeceo", "balanceo", "rot")

for (col in desplazamientos_col) {
  datos[[paste0(col, "_num")]] <- as.numeric(gsub("n\\.d\\.|n\\.i\\.", NA, datos[[col]]))
}

#Paquetes

library(ggplot2)
library(tidyr)
library(dplyr)

#Gráficas desplazamientos

ids_pacientes <- unique(datos$id)

# Filtrar datos para un paciente específico

for (id_f in ids_pacientes) {
  datos_paciente <- datos %>% filter(id == {{ id_f }})

  p <- ggplot(datos_paciente, aes(x = fraccion, y = vrt_num)) +
    geom_line() +
    geom_point() +
    labs(title = paste0("Desplazamientos de camilla: ", id_f),
         x = "Fracción",
         y = "Desplazamiento camilla") +
    theme_minimal()
  
  ggsave(filename = paste0("grafico_desplazamientos_", id_f, ".jpg"), plot = p)
}


