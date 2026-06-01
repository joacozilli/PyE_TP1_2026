library(googlesheets4)
library(ggplot2)
library(dplyr)

# 
url <- "https://docs.google.com/spreadsheets/d/1Kwl4KByOv8q2kXMsgaO3d5QI3vUQ40RCZJgJHhg5bmE/edit#gid=580479207"

# Acceso público
gs4_deauth()

# LEER DATOS 
datos <- read_sheet(url,sheet = "tp", skip = 1)


tabla_dim <- datos %>%
  count(GIRAI_region, `Dimensión mejor puntuada`) %>%
  group_by(GIRAI_region) %>%
  mutate(
    porcentaje = n / sum(n) * 100
  )


print(tabla_dim)


ggplot(tabla_dim,
       aes(x = GIRAI_region,
           y = porcentaje,
           fill = `Dimensión mejor puntuada`)) +
  
  geom_bar(stat = "identity", position = "dodge") +
  
  labs(
    title = "Dimensión mejor puntuada según región",
    x = "Región",
    y = "Porcentaje de países",
    fill = "Dimensión"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )
