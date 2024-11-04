library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
library(caret)
options(scipen = 999)
setwd("C:/Users/bruno/Desktop/Proyecto final/ProyectoFinal")

# Bases
resultado_m1_m2 <- read_csv("bases_modelo/resultado_m1_m2_nuevo.csv")
View(resultado_m1_m2)

tiendas_caba_m1_v2 <- read_csv("bases_modelo/tiendas_caba_m1_v2.csv")
View(tiendas_caba_m1_v2)

tiendas_caba_m1 <- read_csv("bases_modelo/tiendas_caba_m1.csv")
View(tiendas_caba_m1)

montos_promedio <- read_csv("business_case/montos_promedio.csv")
View(montos_promedio)

montos_ajustados_por_pedido <- read_csv("business_case/montos_ajustados_por_pedido.csv")
View(montos_ajustados_por_pedido)

# Tratamiento - variables de días
monday <- tiendas_caba_m1$mon_open
tuesday <- tiendas_caba_m1$tue_open
wednesday <- tiendas_caba_m1$wed_open
thursday <- tiendas_caba_m1$thu_open
friday <- tiendas_caba_m1$fri_open
saturday <- tiendas_caba_m1$sat_open
sunday <- tiendas_caba_m1$sun_open

tiendas_caba_m1_v2$mon_open <- monday
tiendas_caba_m1_v2$tue_open <- tuesday
tiendas_caba_m1_v2$wed_open <- wednesday
tiendas_caba_m1_v2$thu_open <- thursday
tiendas_caba_m1_v2$fri_open <- friday
tiendas_caba_m1_v2$sat_open <- saturday
tiendas_caba_m1_v2$sun_open <- sunday

tiendas_caba_m1_v2$dias_abierto <- tiendas_caba_m1_v2$mon_open + 
  tiendas_caba_m1_v2$tue_open + tiendas_caba_m1_v2$wed_open + 
  tiendas_caba_m1_v2$thu_open + tiendas_caba_m1_v2$fri_open + 
  tiendas_caba_m1_v2$sat_open + tiendas_caba_m1_v2$sun_open 

# Union de datasets
tiendas_caba_m1_v2 <- tiendas_caba_m1_v2 %>% select(-cluster)
resultado_m1_m2 <- resultado_m1_m2 %>% select(-cluster, -predicted_cluster)
tiendas <- tiendas_caba_m1_v2 %>% merge(resultado_m1_m2, by.x = "id", by.y = "customer_id")
tiendas <- tiendas %>% merge(montos_promedio, by.x = "id", by.y = "customer_id")

View(tiendas)

# Prueba

tiendas %>% ggplot + geom_point(aes(x = escuelas_cercanas_1000m , y = monto_promedio))

#Confirmado

tiendas %>%
  group_by(subtes_1000mts) %>%
  summarize(monto_promedio = mean(monto_promedio, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(subtes_1000mts), y = monto_promedio)) +
  geom_col(fill = "salmon") +
  labs(
    x = "Estaciones de Subte en un radio de 1000 metros",
    y = "Monto Promedio Promedio"
  ) +
  theme_minimal()


tiendas %>%
  group_by(estaciones_servicio_1000mts) %>%
  summarize(monto_promedio = mean(monto_promedio, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(estaciones_servicio_1000mts), y = monto_promedio)) +
  geom_col(fill = "skyblue") +
  labs(
    x = "Estaciones de Servicio en un radio de 1000 metros",
    y = "Monto Promedio Promedio"
  ) +
  theme_minimal()

tiendas %>%
  group_by(subtes_1000mts) %>%
  summarize(monto_promedio = mean(monto_promedio, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(subtes_1000mts), y = 1, fill = monto_promedio)) + 
  geom_tile() +
  scale_fill_gradient(low = "lightyellow", high = "red") +
  labs(
    x = "Estaciones de Subte en un radio de 1000 metros",
    y = "",
    fill = "Monto Promedio"
  ) + 
  theme_minimal() + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())


tiendas %>%
  ggplot(aes(x = porc_varones, y = porc_mujeres, size = monto_promedio, color = monto_promedio)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(2, 12)) +
  scale_color_gradient(low = "lightgreen", high = "darkgreen") +
  labs(
    x = "Porcentaje de Varones",
    y = "Porcentaje de Mujeres",
    size = "Monto Promedio",
    color = "Monto Promedio",
    title = "Relación entre % de Varones, % de Mujeres y Monto Promedio",
    subtitle = "El tamaño y color de las burbujas representan el monto promedio"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12)
  )


tiendas %>%
  filter(!is.na(uni_privada_1000mts) & !is.na(uni_publica_1000mts) & !is.na(monto_promedio)) %>%
  mutate(
    universidades_segmento = cut(
      uni_privada_1000mts + uni_publica_1000mts,
      breaks = c(0, 1, 3, 5, 10, Inf),
      labels = c("0-1", "1-3", "3-5", "5-10", "10+"),
      right = FALSE  # Asegura que el límite superior sea excluido
    )
  ) %>%
  ggplot(aes(x = universidades_segmento, y = monto_promedio)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    x = "Número de Universidades Cercanas",
    y = "Monto Promedio",
    title = "Distribución del Monto Promedio por Rangos de Universidades Cercanas",
    subtitle = "Universidades en un radio de 1000 mts"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )



tiendas %>%
  mutate(
    grupos_escuelas = cut(
      escuelas_cercanas_1000m,
      breaks = c(-Inf, 0, 25, 50, 75, 100, Inf),
      labels = c("0", "0-25", "25-50", "50-75", "75-100", ">100"),
      right = FALSE
    )
  ) %>%
  ggplot(aes(x = grupos_escuelas, y = monto_promedio)) +
  geom_point(alpha = 0.6, position = position_jitter(width = 0.2), color = "dodgerblue") +
  labs(
    x = "Número de Escuelas Cercanas (1000 m)",
    y = "Monto Promedio",
    title = "Relación entre el Número de Escuelas Cercanas y el Monto Promedio",
    subtitle = "Dispersión de tiendas según el número de escuelas cercanas"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )



