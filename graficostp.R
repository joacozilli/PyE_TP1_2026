#
library(googlesheets4)
library(ggplot2)
library(dplyr)

# 
url <- "https://docs.google.com/spreadsheets/d/1Kwl4KByOv8q2kXMsgaO3d5QI3vUQ40RCZJgJHhg5bmE/edit#gid=580479207"

# Acceso público
gs4_deauth()

# LEER DATOS (sin comillas en la variable url)
datos <- read_sheet(url,sheet = "tp", range = "A2:AL140")


# Grafico para la cantidad de paises por region
ggplot(datos %>% filter(!is.na(GIRAI_region)), aes(x = GIRAI_region)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Cantidad de países por región",
       x = "Región",
       y = "Cantidad de países") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1))

#
datos$sec_ag_ordenado <- factor(datos$sec_ag,
                                levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))

# Gráfico de barras ordenado
ggplot(datos %>% filter(!is.na(sec_ag)), aes(x = sec_ag_ordenado)) +
  geom_bar(fill = "lightcoral") +
  labs(title = "Nivel de desarrollo en gobernanza de IA",
       x = "Nivel de desarrollo",
       y = "Cantidad de países") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))


# Boxplot
ggplot(datos, aes(x = GIRAI_region, y = areas_ag)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Distribución de áreas con buen desempeño en capacidades de IA (areas_ag) por región",
       x = "Región GIRAI",
       y = "Cantidad de áreas (areas_ag)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(datos, aes(x = GIRAI_region, y = areas_ag)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, color = "gray30") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  labs(title = "Distribución de areas_ag por región",
       subtitle = "Los puntos rojos indican la media",
       x = "Región GIRAI",
       y = "Cantidad de áreas (areas_ag)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#
