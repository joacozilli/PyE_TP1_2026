library(googlesheets4)
library(ggplot2)
library(dplyr)

# 
url <- "https://docs.google.com/spreadsheets/d/1Kwl4KByOv8q2kXMsgaO3d5QI3vUQ40RCZJgJHhg5bmE/edit#gid=580479207"

# Acceso público
gs4_deauth()

# LEER DATOS 
datos <- read_sheet(url,sheet = "tp", skip = 1)


ggplot(datos, aes(x = cap, y = ag)) +
  geom_point(alpha = 0.6, color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Relación entre la existencia de capacidades del Estado para impulsar IA responsable y la existencia
de acciones gubernamentales.",
       x = "Capacidades estatales (cap)",
       y = "Acciones gubernamentales (ag)") +
  theme_minimal()

correlacion_cap_gob <- cor(datos$cap, datos$gob, use = "complete.obs")
cat("Correlación entre cap y gob:", round(correlacion_cap_gob, 3))