library(googlesheets4)
library(ggplot2)
library(dplyr)

# 
url <- "https://docs.google.com/spreadsheets/d/1Kwl4KByOv8q2kXMsgaO3d5QI3vUQ40RCZJgJHhg5bmE/edit#gid=580479207"

# Acceso público
gs4_deauth()

# LEER DATOS 
datos <- read_sheet(url,sheet = "tp", skip = 1)



mediana_areas <- median(datos$areas_mng, na.rm = TRUE)

print(mediana_areas)

ric <- IQR(datos$areas_mng, na.rm = TRUE)

print(ric)

ggplot(datos, aes(x = areas_mng)) +
  geom_bar(fill = "steelblue") +
  
  geom_vline(
    xintercept = mediana_areas,
    color = "red",
    linewidth = 1
  ) +
  
  labs(
    title = "Cantidad de áreas temáticas con marcos normativos sobre IA",
    x = "Cantidad de áreas",
    y = "Cantidad de países"
  ) +
  
  theme_minimal()
