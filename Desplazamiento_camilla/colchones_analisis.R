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
  ggsave(filename = paste0("grafico_errores_", id_f, ".svg"), plot = p_general_paciente, width = 10, height = 8)
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
                         levels = c("rot_num", "balanceo_num", "cabeceo_num", "lat_num", "lng_num", "vrt_num"),
                         labels = c("Rot", "Balanceo", "Cabeceo", "Lat", "Lng", "Vrt"))

p_box_tipo_des <- ggplot(datos_long, aes(x = ind, y = values, fill = ind)) +
  geom_boxplot(width = 0.8, outlier.size = 3) +
  scale_fill_manual(values = colores) +
  labs(
    #title = "Errores por tipo de desplazamiento",
       x = "Desplazamientos",
       y = "Errores [cm/°]") +
  guides(fill = "none") +
  theme_minimal() +
  theme(
    #plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16) 
  ) +
  coord_flip()

# Guardar el gráfico en un archivo
ggsave(filename = paste0("boxplot_general.svg"), plot = p_box_tipo_des, width = 12, height = 10)


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
    #title = "Distribución de datos por tipo de desplazamiento",
    fill = "Tipo de valor"
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%\n", conteo)), 
            position = position_stack(vjust = 0.5), size = 4) +  # Mostrar porcentaje y conteo
  theme_void() +  # Elimina los ejes para el gráfico de pastel
  theme(
    #plot.title = element_text(hjust = 0.75, size = 16, face = "bold"),
    strip.text = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 12)
  )

# Guardar el gráfico en un archivo
ggsave(filename = paste0("pasteles_tipo_general.svg"), plot = p_pasteles_valor_tipo, width = 10, height = 8)

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
ggsave(filename = paste0("pastel_general.svg"), plot = p_pastel_tipo_valor_general, width = 10, height = 8)

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
  labs(#title = "Distribución general de tipo de objeto", 
       fill = "Tipo de objeto") +
  theme_void() +
  theme(
    #plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    #legend.title = element_text(size = 16)
  )

# Guardar el gráfico en un archivo
ggsave(filename = paste0("pastel_objeto.svg"), plot = p_pasteles_objeto, width = 10, height = 8)

####Magnitud lineal####

datos_filtrados <- datos %>%
  filter(!(is.na(vrt_num) & is.na(lng_num) & is.na(lat_num))) %>%
  mutate(magnitud_lineal = sqrt(ifelse(is.na(vrt_num), 0, vrt_num^2) +
                                  ifelse(is.na(lng_num), 0, lng_num^2) +
                                  ifelse(is.na(lat_num), 0, lat_num^2)))

for (id_f in ids_pacientes) {
  datos_paciente <- datos_filtrados %>% filter(id == id_f)
  
    p_magnitud_l <- ggplot(datos_paciente, aes(x = fecha)) +
      
      geom_line(aes(y = magnitud_lineal, color = "ml", group = id_f), size = 2, na.rm = TRUE) +
      geom_point(aes(y = magnitud_lineal, color = "ml", group = id_f), size = 4, na.rm = TRUE) +
      
      scale_y_continuous(
        name = "Magnitud del error lineal [cm]",
      ) +
      
      # Especificar los colores para cada línea
      scale_color_manual(
        values = c("ml" = "black")
      ) +
      
      labs(
        title = paste0("Magnitud del error lineal. ID: ", id_f),
        x = "Fecha"
      ) +
      
      # Personalizar el tema
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14), # Tamaño del título del eje X
        axis.title.y = element_text(size = 14, color = "black"), # Tamaño del título del eje Y
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12), # Tamaño de texto en eje X
        axis.text.y = element_text(size = 12), # Tamaño de texto en eje Y
        legend.position = "none",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18)
      )
    
    # Guardar el gráfico en un archivo
    ggsave(filename = paste0("grafico_magnitud_lineal_", id_f, ".svg"), plot = p_magnitud_l, width = 10, height = 8)
}

# Histograma para la magnitud lineal con curva de normalidad
p_hist_lineal <- ggplot(datos_filtrados, aes(x = magnitud_lineal)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "lightgray", color = "black", alpha = 0.7) +
  geom_density(lwd = 1.2) +
  labs(title = "Histograma de la magnitud del error lineal",
       x = "Magnitud del error lineal [cm]",
       y = "Densidad") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5,size = 20),
    axis.title.x = element_text(size = 14), # Tamaño del título del eje X
    axis.title.y = element_text(size = 14, color = "black"), # Tamaño del título del eje Y
    axis.text.x = element_text(vjust = 0.5, hjust = 1, size = 14), # Tamaño de texto en eje X
    axis.text.y = element_text(size = 14), # Tamaño de texto en eje Y
    legend.position = "none",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )

mean(datos_filtrados$magnitud_lineal)
sd(datos_filtrados$magnitud_lineal)

# Guardar
print(p_hist_lineal)
ggsave(filename = paste0("histo_magnitud_lineal.svg"), plot = p_hist_lineal, width = 10, height = 8)

####Magnitud angular####

datos_filtrados <- datos %>%
  filter(!(is.na(cabeceo_num) & is.na(balanceo_num) & is.na(rot_num))) %>%
  mutate(magnitud_angular = sqrt(ifelse(is.na(cabeceo_num), 0, cabeceo_num^2) +
                                   ifelse(is.na(balanceo_num), 0, balanceo_num^2) +
                                   ifelse(is.na(rot_num), 0, rot_num^2)))

for (id_f in ids_pacientes) {
  datos_paciente <- datos_filtrados %>% filter(id == id_f)
  
  # Verificar si hay datos después de filtrar NAs
    p_magnitud_a <- ggplot(datos_paciente, aes(x = fecha)) +
      
      geom_line(aes(y = magnitud_angular, color = "ma", group = id_f), size = 2, na.rm = TRUE) +
      geom_point(aes(y = magnitud_angular, color = "ma", group = id_f), size = 4, na.rm = TRUE) +
      
      scale_y_continuous(
        name = "Magnitud del error angular [°]",
      ) +
      
      # Especificar los colores para cada línea
      scale_color_manual(
        values = c("ma" = "blue")
      ) +
      
      labs(
        title = paste0("Magnitud del error angular. ID: ", id_f),
        x = "Fecha"
      ) +
      
      # Personalizar el tema
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14), # Tamaño del título del eje X
        axis.title.y = element_text(size = 14, color = "black"), # Tamaño del título del eje Y
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14), # Tamaño de texto en eje X
        axis.text.y = element_text(size = 14), # Tamaño de texto en eje Y
        legend.position = "none",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18)
      )
    
    # Guardar el gráfico en un archivo
    ggsave(filename = paste0("grafico_magnitud_angular_", id_f, ".svg"), plot = p_magnitud_a, width = 10, height = 8)
}

# Histograma para la magnitud angular con curva de normalidad
p_hist_lineal <- ggplot(datos_filtrados, aes(x = magnitud_angular)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(lwd = 1.2) +
  labs(title = "Histograma de la magnitud del error angular",
       x = "Magnitud del error angular [°]",
       y = "Densidad") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5,size = 20),
    axis.title.x = element_text(size = 14), # Tamaño del título del eje X
    axis.title.y = element_text(size = 14, color = "black"), # Tamaño del título del eje Y
    axis.text.x = element_text(vjust = 0.5, hjust = 1, size = 14), # Tamaño de texto en eje X
    axis.text.y = element_text(size = 14), # Tamaño de texto en eje Y
    legend.position = "none",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )

mean(datos_filtrados$magnitud_angular)
sd(datos_filtrados$magnitud_angular)

# Guardar
ggsave(filename = paste0("histo_magnitud_angular.svg"), plot = p_hist_lineal, width = 10, height = 8)

####Summary####

summary(datos$vrt_num)
sd(datos$vrt_num, na.rm = TRUE)

summary(datos$lng_num)
sd(datos$lng_num, na.rm = TRUE)

summary(datos$cabeceo_num)
sd(datos$cabeceo_num, na.rm = TRUE)

summary(datos$lat_num)
sd(datos$lat_num, na.rm = TRUE)

summary(datos$balanceo_num)
sd(datos$balanceo_num, na.rm = TRUE)

summary(datos$rot_num)
sd(datos$rot_num, na.rm = TRUE)

####Pruebas de normalidad para magnitudes####

datos_filtrados_lineal <- datos %>%
  filter(!(is.na(vrt_num) & is.na(lng_num) & is.na(lat_num))) %>%
  mutate(magnitud_lineal = sqrt(ifelse(is.na(vrt_num), 0, vrt_num^2) +
                                  ifelse(is.na(lng_num), 0, lng_num^2) +
                                  ifelse(is.na(lat_num), 0, lat_num^2)))

datos_filtrados_angular <- datos %>%
  filter(!(is.na(cabeceo_num) & is.na(balanceo_num) & is.na(rot_num))) %>%
  mutate(magnitud_angular = sqrt(ifelse(is.na(cabeceo_num), 0, cabeceo_num^2) +
                                   ifelse(is.na(balanceo_num), 0, balanceo_num^2) +
                                   ifelse(is.na(rot_num), 0, rot_num^2)))

# Prueba de normalidad para magnitud lineal
shapiro_lineal <- shapiro.test(datos_filtrados_lineal$magnitud_lineal)
print(shapiro_lineal)

# Prueba de normalidad para magnitud angular
shapiro_angular <- shapiro.test(datos_filtrados_angular$magnitud_angular)
print(shapiro_angular)


# Instalar y cargar el paquete 'MASS' si no lo tienes instalado
if(!require(MASS)) install.packages("MASS")
library(MASS)

# Ajustar una distribución gamma a la variable 'magnitud_lineal' en tu conjunto de datos
ajuste_gamma <- fitdistr(datos_filtrados_lineal$magnitud_lineal, "gamma")
print(ajuste_gamma)

# Obtener los parámetros ajustados
shape <- ajuste_gamma$estimate["shape"]
rate <- ajuste_gamma$estimate["rate"]

# Realizar la prueba de Kolmogorov-Smirnov para ver si los datos se ajustan a una distribución gamma
ks.test(datos_filtrados_lineal$magnitud_lineal, "pgamma", shape = shape, rate = rate)

# Graficar histograma
hist(datos_filtrados_lineal$magnitud_lineal, breaks = 30, probability = TRUE, 
     main = "Histograma con curva de densidad Gamma ajustada", 
     xlab = "Magnitud Lineal")

# Añadir la curva de densidad gamma
curve(dgamma(x, shape = shape, rate = rate), col = "red", lwd = 2, add = TRUE)

#Chi
ajuste_chi <- fitdistr(datos_filtrados_lineal$magnitud_lineal, "chi-squared", start = list(df = 2))
print(ajuste_chi)

# Obtener el parámetro de grados de libertad (df) ajustado
df <- ajuste_chi$estimate["df"]

# Realizar la prueba de Kolmogorov-Smirnov para la distribución Chi-cuadrado
ks.test(datos_filtrados_lineal$magnitud_lineal, "pchisq", df = df)

# Graficar el histograma
hist(datos$magnitud_lineal, breaks = 30, probability = TRUE, 
     main = "Histograma con curva de densidad Chi-cuadrado ajustada", 
     xlab = "Magnitud Lineal")

# Añadir la curva de densidad Chi-cuadrado ajustada
curve(dchisq(x, df = df), col = "blue", lwd = 2, add = TRUE)

####Uso de V####

# Filtrar por ID único y contar el uso de barras de indexación
datos_unicos <- datos %>%
  distinct(id, .keep_all = TRUE) %>%  # Mantener solo registros únicos por ID
  group_by(uso_v) %>%                 # Agrupar por uso de barras
  summarise(conteo = n()) %>%         # Contar los pacientes
  ungroup() %>%
  mutate(porcentaje = conteo / sum(conteo) * 100,  # Calcular porcentaje
         etiqueta = paste0("N: ", conteo, "\n", round(porcentaje, 1), "%"))  # Crear etiqueta

# Crear la gráfica de pastel
p_uso_v <- ggplot(datos_unicos, aes(x = "", y = conteo, fill = factor(uso_v, labels = c("No utiliza", "Utiliza")))) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y") +
  geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5), size = 8, color = "black") +  # Etiquetas con conteo y porcentaje
  labs(#title = "Proporción de pacientes que utilizan barras de indexación", 
    fill = "Uso de barras de indexación V") +
  theme_void() +                        # Eliminar fondo y ejes
  scale_fill_manual(values = c("red", "green")) +  # Colores
  theme(
    #plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  )

# Guardar el gráfico en un archivo
ggsave(filename = paste0("pastel_uso_v.svg"), plot = p_uso_v, width = 10, height = 8)

#Histogramas

# Histogramas de magnitud lineal
ggplot(datos_filtrados_lineal, aes(x = magnitud_lineal, fill = factor(uso_v, labels = c("No utiliza", "Utiliza")))) +
  geom_histogram(aes(y = ..density..), binwidth = 1, position = "identity", alpha = 0.5, color = "black") +
  geom_density(alpha = 0.2, adjust = 1.5) +  # Añadir curva de densidad
  labs(title = "Histograma de Magnitud Lineal", x = "Magnitud Lineal", y = "Densidad", fill = "Uso de Barras de Indexación") +
  theme_minimal()

# Histogramas de magnitud angular
ggplot(datos_filtrados_angular, aes(x = magnitud_angular, fill = factor(uso_v, labels = c("No utiliza", "Utiliza")))) +
  geom_histogram(aes(y = ..density..), binwidth = 1, position = "identity", alpha = 0.5, color = "black") +
  geom_density(alpha = 0.2, adjust = 1.5) +  # Añadir curva de densidad
  labs(title = "Histograma de Magnitud Angular", x = "Magnitud Angular", y = "Densidad", fill = "Uso de Barras de Indexación") +
  theme_minimal()

#Boxplots

# Boxplot de magnitud lineal
p_box_v_lineal <- ggplot(datos_filtrados_lineal, aes(x = factor(uso_v, labels = c("No utiliza", "Utiliza")), y = magnitud_lineal, fill = factor(uso_v, labels = c("No utiliza", "Utiliza")))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Boxplot de la magnitud del error lineal respecto al uso de V", x = "Uso de barras de indexación V", y = "Magnitud del error lineal [cm]", fill = "Uso de barras de indexación V") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "green")) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    axis.title.x = element_text(size = 14), # Tamaño del título del eje X
    axis.title.y = element_text(size = 14, color = "black"), # Tamaño del título del eje Y
    axis.text.x = element_text(vjust = 0.5, hjust = 1, size = 12), # Tamaño de texto en eje X
    axis.text.y = element_text(size = 12), # Tamaño de texto en eje Y
    legend.position = "none",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )

# Guardar el gráfico en un archivo
ggsave(filename = paste0("pastel_box_v_lineal.svg"), plot = p_box_v_lineal, width = 10, height = 8)

# Calcular el promedio y desviación estándar para magnitud lineal
resumen_magnitud <- datos_filtrados_lineal %>%
  group_by(uso_v) %>%
  summarise(
    promedio_lineal = mean(magnitud_lineal, na.rm = TRUE),
    sd_lineal = sd(magnitud_lineal, na.rm = TRUE),
  ) %>%
  mutate(uso_v = factor(uso_v, labels = c("No utiliza", "Utiliza")))  # Etiquetar las categorías

# Mostrar el resumen de estadísticas
print(resumen_magnitud)

# Boxplot de magnitud angular
p_box_v_angular <- ggplot(datos_filtrados_angular, aes(x = factor(uso_v, labels = c("No utiliza", "Utiliza")), y = magnitud_angular, fill = factor(uso_v, labels = c("No utiliza", "Utiliza")))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Boxplot de la magnitud del error angular respecto al uso de V", x = "Uso de barras de indexación V", y = "Magnitud del error angular [°]", fill = "Uso de barras de indexación V") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "green")) +
theme(
  plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
  axis.title.x = element_text(size = 14), # Tamaño del título del eje X
  axis.title.y = element_text(size = 14, color = "black"), # Tamaño del título del eje Y
  axis.text.x = element_text(vjust = 0.5, hjust = 1, size = 12), # Tamaño de texto en eje X
  axis.text.y = element_text(size = 12), # Tamaño de texto en eje Y
  legend.position = "none",
  legend.text = element_text(size = 16),
  legend.title = element_text(size = 18)
)

# Guardar el gráfico en un archivo
ggsave(filename = paste0("pastel_box_v_angular.svg"), plot = p_box_v_angular, width = 10, height = 8)

# Calcular el promedio y desviación estándar para magnitud angular
resumen_magnitud <- datos_filtrados_angular %>%
  group_by(uso_v) %>%
  summarise(
    promedio_angular = mean(magnitud_angular, na.rm = TRUE),
    sd_angular = sd(magnitud_angular, na.rm = TRUE),
  ) %>%
  mutate(uso_v = factor(uso_v, labels = c("No utiliza", "Utiliza")))  # Etiquetar las categorías

# Mostrar el resumen de estadísticas
print(resumen_magnitud)

#Prueba t student

# Filtrar datos para los dos grupos
grupo_utiliza <- datos %>% filter(uso_v == 1)  # Pacientes que utilizan barras
grupo_no_utiliza <- datos %>% filter(uso_v == 0)  # Pacientes que no utilizan barras

# Prueba t para magnitud lineal
t_test_lineal <- t.test(magnitud_lineal ~ uso_v, data = datos_filtrados_lineal, var.equal = FALSE)
print(t_test_lineal)

# Prueba t para magnitud angular
t_test_angular <- t.test(magnitud_angular ~ uso_v, data = datos_filtrados_angular, var.equal = FALSE)
print(t_test_angular)

####SBRT####

# Filtrar por ID único y contar por tipo de plan
datos_unicos <- datos %>%
  distinct(id, .keep_all = TRUE) %>%
  group_by(tipo) %>%                 # Agrupar por tipo de plan
  summarise(conteo = n()) %>%
  ungroup() %>%
  mutate(porcentaje = conteo / sum(conteo) * 100,
         etiqueta = paste0("N: ", conteo, "\n", round(porcentaje, 1), "%"))  # Crear etiqueta

# Crear la gráfica de pastel
p_tipo_plan <- ggplot(datos_unicos, aes(x = "", y = conteo, fill = factor(tipo))) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y") +
  geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5), size = 8, color = "black") +
  labs(fill = "Tipo de Plan") +
  theme_void() +                       
  scale_fill_manual(values = c("red", "green")) +  # Colores para SBRT y no SBRT
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  )

#Guardar el gráfico en un archivo
ggsave(filename = paste0("pastel_tipo_plan.svg"), plot = p_tipo_plan, width = 10, height = 8)


# Boxplot de magnitud lineal
p_box_sbrt_lineal <- ggplot(datos_filtrados_lineal, aes(x = factor(tipo), y = magnitud_lineal, fill = factor(tipo))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Boxplot de la magnitud del error lineal", x = "Tipo de tratamiento", y = "Magnitud del error lineal [cm]", fill = "Tipo de tratamiento") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "green")) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14, color = "black"),
    axis.text.x = element_text(vjust = 0.5, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )

# Guardar el gráfico en un archivo
ggsave(filename = paste0("pastel_box_sbrt_lineal_tipo.svg"), plot = p_box_sbrt_lineal, width = 10, height = 8)

# Boxplot de magnitud angular
p_box_sbrt_angular <- ggplot(datos_filtrados_angular, aes(x = factor(tipo), y = magnitud_angular, fill = factor(tipo))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Boxplot de la magnitud del error angular", x = "Tipo de tratamiento", y = "Magnitud del error angular [°]", fill = "Tipo de tratamiento") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "green")) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14, color = "black"),
    axis.text.x = element_text(vjust = 0.5, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )

# Guardar el gráfico en un archivo
ggsave(filename = paste0("pastel_box_v_angular_tipo.svg"), plot = p_box_sbrt_angular, width = 10, height = 8)


# Calcular el promedio y desviación estándar para magnitud lineal
resumen_magnitud_lineal <- datos_filtrados_lineal %>%
  group_by(tipo) %>%
  summarise(
    promedio_lineal = mean(magnitud_lineal, na.rm = TRUE),
    sd_lineal = sd(magnitud_lineal, na.rm = TRUE)
  )

# Mostrar el resumen de estadísticas para magnitud lineal
print(resumen_magnitud_lineal)

# Calcular el promedio y desviación estándar para magnitud angular
resumen_magnitud_angular <- datos_filtrados_angular %>%
  group_by(tipo) %>%
  summarise(
    promedio_angular = mean(magnitud_angular, na.rm = TRUE),
    sd_angular = sd(magnitud_angular, na.rm = TRUE)
  )

# Mostrar el resumen de estadísticas para magnitud angular
print(resumen_magnitud_angular)


# Filtrar datos para los dos grupos
grupo_sbrt <- datos %>% filter(tipo == "SBRT")  # Pacientes en el plan SBRT
grupo_no_sbrt <- datos %>% filter(tipo == "Fx")  # Pacientes en otros planes

# Prueba t para magnitud lineal
t_test_lineal <- t.test(magnitud_lineal ~ tipo, data = datos_filtrados_lineal, subset = tipo %in% c("SBRT", "Fx"), var.equal = FALSE)
print(t_test_lineal)

# Prueba t para magnitud angular
t_test_angular <- t.test(magnitud_angular ~ tipo, data = datos_filtrados_angular, subset = tipo %in% c("SBRT", "Fx"), var.equal = FALSE)
print(t_test_angular)

