# Cargar librerías necesarias
library(readr)
library(dplyr)
library(ggplot2)

# Leer el archivo
fact <- read_delim("C:/Users/Santiago.Obregon/Downloads/Facturacion ABRIL 2025.csv", delim = ";")

# Transformar columnas relevantes
fact <- fact %>%
  mutate(
    CICLO = as.numeric(CICLO),
    PROMEDIO_LECTURA = as.numeric(PROMEDIO_LECTURA),
    LECTURA_ANTERIOR = as.numeric(LECTURA_ANTERIOR),
    LECTURA_ACTUAL = as.numeric(LECTURA_ACTUAL),
    CONSUMO = as.numeric(CONSUMO)
  )

# Filtrar datos válidos
fact_clean <- fact %>%
  filter(
    !is.na(CICLO),
    !is.na(PROMEDIO_LECTURA),
    !is.na(LECTURA_ANTERIOR),
    !is.na(LECTURA_ACTUAL),
    !is.na(CONSUMO)
  )

# Modelo de regresión lineal
modelo_consumo <- lm(CONSUMO ~ factor(CICLO) + PROMEDIO_LECTURA + LECTURA_ANTERIOR + LECTURA_ACTUAL, data = fact_clean)
summary(modelo_consumo)

# Agregar predicciones
fact_clean <- fact_clean %>%
  mutate(
    CONSUMO_PREDICHO = predict(modelo_consumo),
    ERROR_ABSOLUTO = abs(CONSUMO - CONSUMO_PREDICHO)
  )

# Mostrar gráfico de dispersión: real vs predicho
ggplot(fact_clean, aes(x = CONSUMO_PREDICHO, y = CONSUMO)) +
  geom_point(alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "blue", linetype = "dashed") +
  labs(title = "Consumo Real vs Predicho", x = "Consumo Predicho", y = "Consumo Real")

# ========================
# 🍰 GRÁFICA DE PONQUÉ POR CICLO
# ========================
# Agrupar por ciclo y calcular consumo total
consumo_por_ciclo <- fact_clean %>%
  group_by(CICLO) %>%
  summarise(CONSUMO_TOTAL = sum(CONSUMO, na.rm = TRUE)) %>%
  mutate(PORCENTAJE = round(CONSUMO_TOTAL / sum(CONSUMO_TOTAL) * 100, 1))

# Crear gráfica de ponqué
ggplot(consumo_por_ciclo, aes(x = "", y = PORCENTAJE, fill = factor(CICLO))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Distribución del Consumo Total por Ciclo", fill = "Ciclo") +
  theme_void() +
  geom_text(aes(label = paste0(PORCENTAJE, "%")), 
            position = position_stack(vjust = 0.5), color = "white", size = 4)
