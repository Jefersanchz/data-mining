# Instalar paquetes si no están
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")

# 1. Cargar paquetes
library(readr)
library(dplyr)
library(ggplot2)

# 2. Leer el archivo CSV
fact <- read_delim("C:/Users/Santiago.Obregon/Downloads/Facturacion ABRIL 2025.csv", delim = ";")

# 3. Limpiar y convertir columnas clave
fact <- fact %>%
  mutate(
    CONSUMO = as.numeric(CONSUMO),
    VR_TOTAL = as.numeric(VR_TOTAL),
    ESTRATO = as.numeric(ESTRATO),
    CARGO_FIJO_ACUE = as.numeric(CARGO_FIJO_ACUE),
    CARGO_FIJO_ALCA = as.numeric(CARGO_FIJO_ALCA),
    RECONEXION = as.numeric(RECONEXION)
  )

# 4. Filtrar registros válidos
fact_clean <- fact %>%
  filter(!is.na(CONSUMO), !is.na(VR_TOTAL),
         !is.na(ESTRATO), !is.na(CARGO_FIJO_ACUE),
         !is.na(CARGO_FIJO_ALCA), !is.na(RECONEXION))

# 5. Ajustar modelo completo (modelo2)
modelo2 <- lm(VR_TOTAL ~ CONSUMO + ESTRATO + CARGO_FIJO_ACUE + CARGO_FIJO_ALCA + RECONEXION, data = fact_clean)
summary(modelo2)

# 6. Ajustar modelo optimizado (sin variable eliminada automáticamente)
modelo3 <- lm(VR_TOTAL ~ CONSUMO + ESTRATO + RECONEXION, data = fact_clean)
summary(modelo3)

# 7. Agregar predicciones del modelo final
fact_clean <- fact_clean %>%
  mutate(VR_PREDICHO = predict(modelo3))

# 8. Gráfico: valores reales vs predichos
ggplot(fact_clean, aes(x = VR_PREDICHO, y = VR_TOTAL)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") +
  labs(title = "Valor Real vs Predicho", x = "Predicho (VR_TOTAL)", y = "Real (VR_TOTAL)")

# 9. Gráfico: residuos
ggplot(fact_clean, aes(x = VR_PREDICHO, y = VR_TOTAL - VR_PREDICHO)) +
  geom_point(alpha = 0.5, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuos del Modelo", x = "Predicho", y = "Error (residuo)")

# 10. (Opcional) Exportar CSV con predicciones
write_csv(fact_clean, "C:/Users/Santiago.Obregon/Downloads/Facturacion_PREDICCIONES.csv")
fact_pred <- read_csv("C:/Users/Santiago.Obregon/Downloads/Facturacion_PREDICCIONES.csv")

# Agregar columna de error absoluto
fact_pred <- fact_pred %>%
  mutate(ERROR_ABSOLUTO = abs(VR_TOTAL - VR_PREDICHO))

# Mostrar los 10 casos con mayor error
fact_pred %>%
  arrange(desc(ERROR_ABSOLUTO)) %>%
  select(MATRICULA, VR_TOTAL, VR_PREDICHO, ERROR_ABSOLUTO) %>%
  head(10)

