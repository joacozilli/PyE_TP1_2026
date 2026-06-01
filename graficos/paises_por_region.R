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
# Grafico para la cantidad de paises por region  (primer grafico)
ggplot(datos %>% filter(!is.na(GIRAI_region)),
       aes(x = reorder(GIRAI_region, -table(GIRAI_region)[GIRAI_region]))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Cantidad de países por región",
       x = "Región",
       y = "Cantidad de países") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))


