library(googlesheets4)
library(ggplot2)
library(dplyr)

url <- "https://docs.google.com/spreadsheets/d/1Kwl4KByOv8q2kXMsgaO3d5QI3vUQ40RCZJgJHhg5bmE/edit#gid=580479207"

# Acceso público
gs4_deauth()

# LEER DATOS 
datos <- read_sheet(url, sheet = "tp", skip = 1)

# CAMBIO: Creamos el factor ordenado sobre sec_mng
datos$sec_mng_ordenado <- factor(datos$sec_mng,
                                 levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))

# Gráfico de barras ordenado para sec_mng
ggplot(datos %>% filter(!is.na(sec_mng)), aes(x = sec_mng_ordenado)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Desarrollo en marcos normativos para IA",
    x = "Nivel de desarrollo",
    y = "Cantidad de países"
  ) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

# 1. Filtramos los NA de la nueva variable
datos_filtrados <- datos %>% filter(!is.na(sec_mng_ordenado))

# 2. Calculamos la moda
moda <- datos_filtrados %>%
  count(sec_mng_ordenado) %>%
  arrange(desc(n)) %>%
  slice(1)

# 3. Imprimimos resultados
print(moda)

cat("La moda es:", as.character(moda$sec_mng_ordenado),
    "con", moda$n, "países.\n")