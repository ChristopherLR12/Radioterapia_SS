####Paquetes####

library(ggplot2)
library(tidyr)
library(dplyr)
library(ggthemes)

####Datos####

setwd("C:\\Users\\c-lop\\Documents\\GitHub\\Radioterapia_SS\\Desplazamiento_camilla")

datos<-read.csv("colchones.csv",header=TRUE)

str(datos)

errores_col <- c("vrt", "lng", "lat", "cabeceo", "balanceo", "rot")

errores_col_num <- c("vrt_num", "lng_num", "lat_num", "cabeceo_num", "balanceo_num", "rot_num")

for (col in errores_col) {
  datos[[paste0(col, "_num")]] <- as.numeric(na_if(datos[[col]], "n.d."))
  datos[[paste0(col, "_num")]] <- as.numeric(na_if(datos[[col]], "n.i."))
}

####Gráficas errores####

ids_pacientes <- unique(datos$id)

# Filtrar datos para un paciente específico y graficar errores

for (id_f in ids_pacientes) {
  datos_paciente <- datos %>% filter(id == id_f)
  
  p_general_paciente <- ggplot(datos_paciente, aes(x = fecha)) +
    
    geom_line(aes(y = vrt_num, color = "Vrt", group = id_f), size = 1, na.rm = TRUE) +
    geom_point(aes(y = vrt_num, color = "Vrt", group = id_f), size = 2.5, na.rm = TRUE) +
    
    geom_line(aes(y = lng_num, color = "Lng", group = id_f), size = 1, na.rm = TRUE) +
    geom_point(aes(y = lng_num, color = "Lng", group = id_f), size = 2.5, na.rm = TRUE) +
    
    geom_line(aes(y = lat_num, color = "Lat", group = id_f), size = 1, na.rm = TRUE) +
    geom_point(aes(y = lat_num, color = "Lat", group = id_f), size = 2.5, na.rm = TRUE) +
    
    geom_line(aes(y = cabeceo_num, color = "Cabeceo", group = id_f), size = 1, linetype = "dashed", na.rm = TRUE) +
    geom_point(aes(y = cabeceo_num, color = "Cabeceo", group = id_f), size = 2.5, na.rm = TRUE) +
    
    geom_line(aes(y = balanceo_num, color = "Balanceo", group = id_f), size = 1, linetype = "dashed", na.rm = TRUE) +
    geom_point(aes(y = balanceo_num, color = "Balanceo", group = id_f), size = 2.5, na.rm = TRUE) +
    
    geom_line(aes(y = rot_num, color = "Rot", group = id_f), size = 1, linetype = "dashed", na.rm = TRUE) +
    geom_point(aes(y = rot_num, color = "Rot", group = id_f), size = 2.5, na.rm = TRUE) +
    
    scale_y_continuous(
      name = "Error en configuración [cm]",
      sec.axis = sec_axis(~., name = "Error en configuración [°]")
    ) +
    
    # Especificar los colores para cada línea
    scale_color_manual(
      values = c("Vrt" = "green", "Lng" = "blue", "Lat" = "red",
                 "Cabeceo" = "orange", "Balanceo" = "magenta", "Rot" = "gray"),
      breaks = c("Vrt", "Lng", "Lat", "Cabeceo", "Balanceo", "Rot")
    ) +
    
    labs(
      title = paste0("Errores en desplazamientos de camilla. ID: ", id_f),
      x = "Fecha",
      color = "Tipo de desplazamiento"
    ) +
    
    # Personalizar el tema
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.75),
      axis.title.y.left = element_text(color = "black"),
      axis.title.y.right = element_text(color = "black"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), 
      legend.background = element_rect(fill = "yellow", color = "black", size = 0.5), 
      legend.box.background = element_rect(color = "black") 
    )
  
  # Guardar el gráfico en un archivo
  ggsave(filename = paste0("grafico_errores_", id_f, ".jpg"), plot = p_general_paciente, width = 10, height = 8)
}

####Boxplot general por desplazamiento####

colores <- c("Vrt" = "green", 
             "Lng" = "blue", 
             "Lat" = "red", 
             "Cabeceo" = "orange", 
             "Balanceo" = "magenta", 
             "Rot" = "gray")

# Cambia el stack para usar factor y establecer los nombres adecuados

datos_long <- stack(datos[, errores_col_num])
datos_long$ind <- factor(datos_long$ind, 
                         levels = c("vrt_num", "lng_num", "lat_num", "cabeceo_num", "balanceo_num", "rot_num"),
                         labels = c("Vrt", "Lng", "Lat", "Cabeceo", "Balanceo", "Rot"))

p_box_tipo_des <- ggplot(datos_long, aes(x = ind, y = values, fill = ind)) +
  geom_boxplot() +
  scale_fill_manual(values = colores) +
  labs(title = "Errores por tipo de desplazamiento",
       x = "Desplazamientos",
       y = "Errores [cm/°]") +
  guides(fill = "none") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14)
  )

# Guardar el gráfico en un archivo
ggsave(filename = paste0("boxplot_general.svg"), plot = p_box_tipo_des, width = 10, height = 8)


####Gráficas de pasteles por tipo de dato####

# Inicializar un data frame vacío para almacenar los resultados
conteo_totales <- data.frame()

# Recuento de valores por cada desplazamiento
for (col in errores_col) {
  conteo <- datos %>%
    summarise(
      tipo = col,
      error = sum(!is.na(as.numeric(na_if(datos[[col]], "n.d."))) & !is.na(as.numeric(na_if(datos[[col]], "n.i."))), na.rm = TRUE),
      n.d. = sum(datos[[col]] == "n.d.", na.rm = TRUE),
      n.i. = sum(datos[[col]] == "n.i.", na.rm = TRUE)
    )
  
  # Agregar conteo al data frame total
  conteo_totales <- rbind(conteo_totales, conteo)
}

# Ordenar y renombrar los tipos de desplazamiento
conteo_totales$tipo <- factor(conteo_totales$tipo, 
                              levels = errores_col,  # Asegura el orden
                              labels = c("Vrt", "Lng", "Lat", "Cabeceo", "Balanceo", "Rot"))  # Renombra

# Convertir el data frame en formato largo para graficar
conteo_largo <- conteo_totales %>%
  pivot_longer(cols = c(error, n.d., n.i.), names_to = "tipo_valor", values_to = "conteo")

# Calcular los porcentajes para cada tipo
conteo_largo <- conteo_largo %>%
  group_by(tipo) %>%
  mutate(percentage = conteo / sum(conteo) * 100)

# Gráfico de pastel por cada tipo de desplazamiento con porcentaje y conteo
p_pasteles_valor_tipo <- ggplot(conteo_largo, aes(x = "", y = conteo, fill = tipo_valor)) +
  geom_bar(stat = "identity", width = 1) +
  facet_wrap(~ tipo) +  # Un pastel por cada desplazamiento
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("error" = "green", "n.d." = "yellow", "n.i." = "red")) +
  labs(
    title = "Distribución de datos por tipo de desplazamiento",
    fill = "Tipo de valor"
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%\n", conteo)), 
            position = position_stack(vjust = 0.5), size = 4) +  # Mostrar porcentaje y conteo
  theme_void() +  # Elimina los ejes para el gráfico de pastel
  theme(
    plot.title = element_text(hjust = 0.75, size = 16, face = "bold"),
    strip.text = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 12)
  )

# Guardar el gráfico en un archivo
ggsave(filename = paste0("pasteles_tipo_general.jpg"), plot = p_pasteles_valor_tipo, width = 10, height = 8)

####Gráfico pasteles general tipo de valor####

# Calcular los totales generales de error, n.d. y n.i.
conteo_general <- conteo_largo %>%
  group_by(tipo_valor) %>%
  summarise(conteo_total = sum(conteo))

# Calcular el porcentaje total para cada valor
conteo_general <- conteo_general %>%
  mutate(percentage = conteo_total / sum(conteo_total) * 100)

p_pastel_tipo_valor_general <- ggplot(conteo_general, aes(x = "", y = conteo_total, fill = tipo_valor)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("error" = "green", "n.d." = "yellow", "n.i." = "red")) +
  labs(
    title = "Distribución general de datos",
    fill = "Tipo de valor"
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%\n", conteo_total)), 
            position = position_stack(vjust = 0.5), size = 8) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  )

# Guardar el gráfico en un archivo
ggsave(filename = paste0("pastel_general.jpg"), plot = p_pastel_tipo_valor_general, width = 10, height = 8)

####Gráfico pasteles general tipo de imagen####

# Agrupar los objetos "MV_nolig", "MV_nolig_AP" y "MV_nolig_LAT" en una nueva categoría "MV_nolig_grupo"
datos$tipo_objeto <- ifelse(datos$tipo_objeto %in% c("MV_nolig", "MV_nolig_AP", "MV_nolig_LAT"), 
                            "MV_nolig_grupo", datos$tipo_objeto)

# Contar las frecuencias de cada tipo de objeto
df_conteo_objeto <- as.data.frame(table(datos$tipo_objeto))

# Calcular el porcentaje
df_conteo_objeto$porcentaje <- round(df_conteo_objeto$Freq / sum(df_conteo_objeto$Freq) * 100, 1)

# Crear las etiquetas con cantidad y porcentaje
df_conteo_objeto$etiqueta <- paste0(df_conteo_objeto$Var1, "\n", df_conteo_objeto$Freq, " (", df_conteo_objeto$porcentaje, "%)")

# Crear el gráfico de pastel con las etiquetas fuera del gráfico
p_pasteles_objeto <- ggplot(df_conteo_objeto, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5), size = 6) +  # Ajusta vjust para que las etiquetas estén fuera
  labs(title = "Distribución general de tipo de objeto", fill = "Tipo de objeto") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  )

# Guardar el gráfico en un archivo
ggsave(filename = paste0("pastel_objeto.jpg"), plot = p_pasteles_objeto, width = 10, height = 8)

