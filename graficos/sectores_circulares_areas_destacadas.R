library(googlesheets4)
library(dplyr)

# 1. Autorizar el acceso
gs4_deauth()

url_sheet <- "https://docs.google.com/spreadsheets/d/1Kwl4KByOv8q2kXMsgaO3d5QI3vUQ40RCZJgJHhg5bmE/edit?pli=1&gid=580479207#gid=580479207"

# 2. Leer la segunda fila de encabezados (Fila 2, desde S hasta AA) para los nombres de las columnas
nombres_columnas <- read_sheet(url_sheet, range = "tp!S2:AA2", col_names = FALSE) %>% 
  unlist() %>% 
  as.character()

# 3. Leer los datos de la hoja "tp" a partir de la fila 3 y solo de ese rango de columnas
datos <- read_sheet(url_sheet, range = "tp!S3:AA", col_names = nombres_columnas)

# 2. Calculamos el porcentaje
porcentaje_destacados <- datos %>%
  # Creamos una columna temporal que sume las filas de las columnas seleccionadas
  mutate(destaca_en_alguna = rowSums(select(., p70_sesgo:p70_transp)) > 0) %>%
  # Calculamos el promedio de TRUEs (que equivale al porcentaje en formato decimal)
  summarise(porcentaje = mean(destaca_en_alguna) * 100)

# Ver el resultado
print(porcentaje_destacados)

library(ggplot2)

# 1. Preparar los datos (calculando porcentajes y posiciones para los textos)
datos_grafico <- datos %>%
  mutate(destaca_en_alguna = rowSums(select(., p70_sesgo:p70_transp)) > 0) %>%
  mutate(Estado = ifelse(destaca_en_alguna, "Destaca en algún área", "No destaca en ninguna")) %>% 
  count(Estado) %>% 
  mutate(
    Porcentaje = n / sum(n) * 100,
    # Calculamos la posición del texto en el centro de cada sector
    pos_texto = cumsum(Porcentaje) - 0.5 * Porcentaje
  )

# 2. Generar el gráfico de sectores circulares
ggplot(datos_grafico, aes(x = "", y = Porcentaje, fill = Estado)) +
  # El gráfico de torta nace de una barra apilada (geom_col) transformando las coordenadas
  geom_col(width = 1, color = "white") +
  coord_polar("y", start = 0) +
  
  # Usamos position_stack(vjust = 0.5) para que el texto se centre SIEMPRE
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 6, fontface = "bold") +
  
  # Estética limpia para gráficos circulares (elimina ejes innecesarios)
  theme_void(base_size = 14) + 
  theme(
      plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(t = 20)),
    legend.position = "bottom",
    legend.box.margin = margin(b = 20)
  ) +
  
  labs(
    title = "Distribución de países en IA Responsable",
    fill = ""
  ) +
  scale_fill_manual(values = c("Destaca en algún área" = "#2ca02a", "No destaca en ninguna" = "#d62728"))

