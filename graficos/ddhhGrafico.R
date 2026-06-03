library(googlesheets4)
library(ggplot2)
library(dplyr)

# 
url <- "https://docs.google.com/spreadsheets/d/1Kwl4KByOv8q2kXMsgaO3d5QI3vUQ40RCZJgJHhg5bmE/edit#gid=580479207"

# Acceso público
gs4_deauth()

# LEER DATOS 
datos <- read_sheet(url,sheet = "tp", skip = 1)

#---------------------------------------------------------------
#Grafico para ddhh 
# Analisis univariado de una continua.
ggplot(datos, aes(x = ddhh)) +
  geom_histogram(bins = 15, fill = "lightblue", color = "white") +
  labs(title = "Distribución del puntaje de Derechos Humanos en IA (ddhh)",
       x = "Puntaje ddhh (0-100)",
       y = "Cantidad de países") +
  theme_minimal()
#