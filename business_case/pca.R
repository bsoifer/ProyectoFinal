library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
library(caret)
library(FNN)
options(scipen = 999)

# Leo archivos
resultado_m1_m2 <- read_csv("bases_modelo/resultado_m1_m2.csv")
View(resultado_m1_m2)

tiendas_caba_m2 <- read_csv("bases_modelo/tiendas_caba_m2.csv")
View(tiendas_caba_m2)

tiendas_caba_m1<- read_csv("bases_modelo/tiendas_caba_m1.csv")
View(tiendas_caba_m1)


# 1. Eliminar las columnas id y cluster
tiendas_caba_m1_reducido <- tiendas_caba_m1[, !(names(tiendas_caba_m1) %in% c("id", "cluster"))]

# 2. Separar las variables booleanas y continuas
boolean_vars <- tiendas_caba_m1_reducido[, sapply(tiendas_caba_m1_reducido, is.numeric) & sapply(tiendas_caba_m1_reducido, function(x) all(x %in% c(0, 1)))]
continuous_vars <- tiendas_caba_m1_reducido[, sapply(tiendas_caba_m1_reducido, is.numeric) & !sapply(tiendas_caba_m1_reducido, function(x) all(x %in% c(0, 1)))]

# 3. Escalar las variables continuas
scaled_continuous_vars <- scale(continuous_vars)

# 4. Realizar el PCA
pca_model <- prcomp(scaled_continuous_vars, center = TRUE, scale. = TRUE)

# 5. Calcular la varianza explicada
varianza_explicada <- pca_model$sdev^2
proporcion_varianza_explicada <- varianza_explicada / sum(varianza_explicada)

# 6. Calcular el acumulado de varianza explicada
acumulado_varianza_explicada <- cumsum(proporcion_varianza_explicada)

# 7. Crear un gráfico de codo
ggplotly(ggplot(data.frame(Componente = 1:length(acumulado_varianza_explicada),
                  Acumulado_Varianza_Explicada = acumulado_varianza_explicada), 
       aes(x = Componente, y = Acumulado_Varianza_Explicada)) +
  geom_line() +
  geom_point() +
  labs(title = "Gráfico de Codo: Acumulado de Varianza Explicada por Componente Principal",
       x = "Número de Componente Principal",
       y = "Acumulado de Proporción de Varianza Explicada") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent))
