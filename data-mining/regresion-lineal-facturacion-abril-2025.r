install.packages("readr")     # lectura eficiente
install.packages("dplyr")     # manipulación de datos
install.packages("ggplot2")   # visualización
# 1. Cargar paquetes
library(readr)
library(dplyr)
library(ggplot2)

# 2. Leer el archivo CSV desde la carpeta Downloads
fact <- read_csv("C:/Users/Santiago.Obregon/Downloads/Facturacion ABRIL 2025.csv")

# 3. Limpiar y convertir columnas clave
fact <- fact %>%
  mutate(
    CONSUMO = as.numeric(CONSUMO),
    VR_TOTAL = as.numeric(VR_TOTAL),
    DIAS_CONSUMO = as.numeric(DIAS_CONSUMO)
  ) %>%
  filter(!is.na(CONSUMO), !is.na(VR_TOTAL), !is.na(DIAS_CONSUMO))

# 4. Ajustar el modelo de regresión
modelo <- lm(VR_TOTAL ~ CONSUMO + DIAS_CONSUMO, data = fact)

# 5. Mostrar resumen del modelo
summary(modelo)

# 6. Agregar columna con predicciones
fact <- fact %>%
  mutate(VR_PREDICHO = predict(modelo))

# 7. Gráfico: valores ajustados vs reales
ggplot(fact, aes(x = VR_PREDICHO, y = VR_TOTAL)) +
  geom_point(alpha = 0.6) +
  geom_abline(color = "blue", linetype = "dashed") +
  labs(
    title = "Valores Reales vs Predichos",
    x = "Valor Predicho (VR_TOTAL)",
    y = "Valor Real (VR_TOTAL)"
  )

# 8. Gráfico: residuos del modelo
ggplot(fact, aes(x = VR_PREDICHO, y = VR_TOTAL - VR_PREDICHO)) +
  geom_point(alpha = 0.6, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuos del Modelo",
    x = "Valor Predicho",
    y = "Residuo (Error)"
  )
