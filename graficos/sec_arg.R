library(googlesheets4)
library(ggplot2)
library(dplyr)

# 
url <- "https://docs.google.com/spreadsheets/d/1Kwl4KByOv8q2kXMsgaO3d5QI3vUQ40RCZJgJHhg5bmE/edit#gid=580479207"

# Acceso público
gs4_deauth()

# LEER DATOS 
datos <- read_sheet(url,sheet = "tp", skip = 1)

# Gobernanza de la IA: mide el grado en que los gobiernos nacionales cuentan con normas,
# políticas, procesos y prácticas que guían y/o regulan el diseño, desarrollo y uso de sistemas
# de IA de manera segura, ética y responsable.


datos$sec_ag_ordenado <- factor(datos$sec_ag,
                    levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))

# Gráfico de barras ordenado  (segundo grafico)
ggplot(datos %>% filter(!is.na(sec_ag)), aes(x = sec_ag_ordenado)) +
  geom_bar(fill = "lightcoral") +
  labs(title = "Nivel de desarrollo en acciones gubernamentales respecto al uso responsable de IA",
       x = "Nivel de desarrollo",
       y = "Cantidad de países") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# 1. Filtramos los NA
datos_filtrados <- datos %>% filter(!is.na(sec_ag_ordenado))

moda <- datos_filtrados %>%
  count(sec_ag_ordenado) %>%
  arrange(desc(n)) %>%
  slice(1)


print(moda)

cat("La moda es:", as.character(moda$sec_ag_ordenado),
    "con", moda$n, "países")
