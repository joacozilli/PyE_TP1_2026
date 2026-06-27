library(googlesheets4)
library(ggplot2)
library(dplyr)

url <- "https://docs.google.com/spreadsheets/d/1Kwl4KByOv8q2kXMsgaO3d5QI3vUQ40RCZJgJHhg5bmE/edit#gid=580479207"
gs4_deauth()
datos <- read_sheet(url, sheet = "tp", skip = 1)

# 1. Calculamos las estadísticas
mediana_areas <- median(datos$areas_mng, na.rm = TRUE)
ric <- IQR(datos$areas_mng, na.rm = TRUE)

# 2. Preparamos los datos contando las frecuencias por cantidad de áreas
datos_conteo <- datos %>%
  filter(!is.na(areas_mng)) %>% # Filtramos nulos por seguridad
  count(areas_mng, name = "cantidad_paises")

# 3. Graficamos el diagrama de bastones
ggplot(datos_conteo, aes(x = areas_mng, y = cantidad_paises)) +
  
  # CAMBIO CLAVE: geom_segment dibuja los bastones
  geom_segment(aes(xend = areas_mng, yend = 0), 
               color = "steelblue", 
               linewidth = 1.5) +
  
  # Opcional: añadimos un punto al final de cada bastón para mejorar la estética
  geom_point(color = "steelblue", size = 3) +
  
  # Línea de la mediana
  geom_vline(
    xintercept = mediana_areas,
    color = "red",
    linewidth = 1,
    lwd = 1
  ) +
  
  labs(
    title = "Distribución de países según cantidad de áreas temáticas con marcos normativos",
    x = "Cantidad de áreas con marcos normativos",
    y = "Cantidad de países"
  ) +
  
  theme_minimal()