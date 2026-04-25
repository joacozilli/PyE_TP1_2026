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
ggplot(datos %>% filter(!is.na(GIRAI_region)), aes(x = GIRAI_region)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Cantidad de países por región",
       x = "Región",
       y = "Cantidad de países") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))

#Moda
moda_region <- datos %>%
  filter(!is.na(GIRAI_region)) %>%
  count(GIRAI_region) %>%
  slice_max(n, n = 1) %>%
  pull(GIRAI_region)

print(moda_region)
#---------------------------------------------------------------

#
datos$sec_ag_ordenado <- factor(datos$sec_ag,
                                levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))

# Gráfico de barras ordenado  (segundo grafico)
ggplot(datos %>% filter(!is.na(sec_ag)), aes(x = sec_ag_ordenado)) +
  geom_bar(fill = "lightcoral") +
  labs(title = "Nivel de desarrollo en gobernanza de IA",
       x = "Nivel de desarrollo",
       y = "Cantidad de países") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# 1. Filtramos los NA
datos_filtrados <- datos %>% filter(!is.na(sec_ag_ordenado))

# 2. Calculamos la mediana (el valor central)
indice_mediana <- median(as.numeric(datos_filtrados$sec_ag_ordenado))

# 3. Obtenemos el nombre de la categoría
mediana_valor <- levels(datos_filtrados$sec_ag_ordenado)[round(indice_mediana)]

print(mediana_valor)

#---------------------------------------------------------------
# Boxplot (tercer grafico)
ggplot(datos, aes(x = GIRAI_region, y = areas_ag)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Distribución de áreas con buen desempeño en capacidades de IA (areas_ag) por región",
       x = "Región GIRAI",
       y = "Cantidad de áreas (areas_ag)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#---------------------------------------------------------------
#descripcion grafica de ddhh (cuarto grafico)
ggplot(datos, aes(x = ddhh)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15, 
                 fill = "lightgreen", color = "white") +
  labs(title = "Distribución del puntaje de Derechos Humanos en IA (ddhh)",
       x = "Puntaje ddhh (0-100)",
       y = "Densidad") +
  theme_minimal()

media_ddhh <- mean(datos$ddhh, na.rm = TRUE)
mediana_ddhh <- median(datos$ddhh, na.rm = TRUE)

print(media_ddhh)
print(mediana_ddhh)
#---------------------------------------------------------------


# relacion entre cap y gob  (quinto grafico)
ggplot(datos, aes(x = cap, y = gob)) +
  geom_point(alpha = 0.6, color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Relación entre Capacidades Estatales (cap) y Acciones de Gobierno (gob)",
       x = "Capacidades estatales (cap)",
       y = "Acciones de gobierno (gob)") +
  theme_minimal()

correlacion_cap_gob <- cor(datos$cap, datos$gob, use = "complete.obs")
cat("Correlación entre cap y gob:", round(correlacion_cap_gob, 3))

#---------------------------------------------------------------
# categorica vs cuantitativa (girai_region vs Brecha). (sexto grafico)
datos$brecha <- datos$mng - datos$ag
ggplot(datos %>% filter(!is.na(brecha), !is.na(GIRAI_region)), aes(x = GIRAI_region, y = brecha)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Brecha entre gobernanza y capacidades en IA por región",
       x = "Región GIRAI",
       y = "Brecha (mng - ag)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),axis.text.x = element_text(angle = 45, hjust = 1))



#
