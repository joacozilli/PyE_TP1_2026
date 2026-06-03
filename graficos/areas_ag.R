library(googlesheets4)
library(ggplot2)
library(dplyr)

# 
url <- "https://docs.google.com/spreadsheets/d/1Kwl4KByOv8q2kXMsgaO3d5QI3vUQ40RCZJgJHhg5bmE/edit#gid=580479207"

# Acceso público
gs4_deauth()

# LEER DATOS 
datos <- read_sheet(url,sheet = "tp", skip = 1)

ggplot(datos, aes(x = GIRAI_region, y = areas_ag)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Cantidad de áreas con acciones gubernamentales para abordar la IA por región",
       x = "Región GIRAI",
       y = "Cantidad de áreas (areas_ag)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
