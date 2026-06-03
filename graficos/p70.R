# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("ggplot2")

library(tidyverse)
library(googlesheets4)
library(ggplot2)

url <- "https://docs.google.com/spreadsheets/d/1Kwl4KByOv8q2kXMsgaO3d5QI3vUQ40RCZJgJHhg5bmE/edit#gid=580479207"

# LEER DATOS 
mis_datos <- read_sheet(url,sheet = "tp", skip = 1)


total_paises <- nrow(mis_datos)

analisis_univariado <- mis_datos %>%
  pivot_longer(
    cols = c(p70_sesgo, p70_infancia, p70_divers, p70_datpers, 
             p70_genero, p70_suphum, p70_laboral, p70_segu, p70_transp),
    names_to = "Area_Evaluada",
    values_to = "Supera_70"
  ) %>%
  filter(Supera_70 == 1) %>%
  # Contamos cuantos países tienen un 1 en cada área
  count(Area_Evaluada) %>%
  # Calculamos la proporcion
  mutate(porcentaje_paises = (n / total_paises) * 100) %>%
  # Ordenamos el grafico 
  arrange(desc(porcentaje_paises))

print(analisis_univariado)

ggplot(analisis_univariado, aes(x = reorder(Area_Evaluada, porcentaje_paises), y = porcentaje_paises)) +
  geom_col(fill = "orange", color = "white") +
  coord_flip() +
  labs(
    title = "Porcentaje de países con rendimiento destacado por área",
    subtitle = paste("Total de países analizados: N =", total_paises),
    x = "Áreas destacadas",
    y = "Porcentaje de países"
  ) +
  theme_minimal()
