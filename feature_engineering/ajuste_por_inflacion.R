library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
options(scipen = 999)

#Lectura de bases
items <- read_csv("bases/items_ar.csv")
View(items)

orders <- read_csv("bases/orders_ar.csv")
View(orders)

customers <- read_csv("bases/customers_ar.csv")
View(customers)

tiendas_caba_m1 <- read_csv("bases_modelo/tiendas_caba_m1_v2.csv")
View(tiendas_caba_m1)

tiendas_caba_m2 <- read_csv("bases_modelo/tiendas_caba_m2.csv")
View(tiendas_caba_m2)

# Total de monto de pedido por cada customer
pedidos = orders %>% select(customer_id, total, created_at)
pedidos = pedidos %>% filter(customer_id %in% tiendas_caba_m1$id)

# Ajuste por inflación con la variacion del IPC
ipc <- data.frame(
  fecha = seq(as.Date("2021-01-01"), as.Date("2024-09-01"), by="month"),
  variacion = c(
    4, 3.6, 4.8, 4.1, 3.3, 3.2, 3, 2.5, 3.5, 3.5, 2.5, 3.8,
    3.9, 4.7, 6.7, 6, 5.1, 5.3, 7.4, 7, 6.2, 6.3, 4.9, 5.1,
    6, 6.6, 7.7, 8.4, 7.8, 6, 6.3, 12.4, 12.7, 8.3, 12.8, 25.5,
    20.6, 13.2, 11, 8.8, 4.2, 4.6, 4, 4.2, 3.5
  )
)

ipc$variacion <- ipc$variacion / 100
ipc$indice <- cumprod(1 + ipc$variacion)

indice_final <- ipc$indice[ipc$fecha == as.Date("2024-09-01")]

# Le coloco a cada pedido la fecha de inicio del mes correspondiente
pedidos$created_at <- as.Date(as.POSIXct(pedidos$created_at))

pedidos$mes_inicio <- as.Date(format(pedidos$created_at, "%Y-%m-01"))

pedidos <- merge(pedidos, ipc, by.x = "mes_inicio", by.y = "fecha")

#Creo la columna "adjusted total", que es el monto ajustado por inflación a valores de septiembre 2024
pedidos$adjusted_total <- pedidos$total * (indice_final / pedidos$indice)
#write.csv(pedidos, "montos_ajustados_por_pedido.csv", row.names = F)

# Agrupo: promedio de monto por pedido
pedidos = pedidos %>% group_by(customer_id) %>% summarise(promedio_por_pedido = mean(adjusted_total)) 

# Agrego el monto promedio a las tiendas de CABA, para el modelo 2
tiendas_caba_m2 = merge(tiendas_caba_m2, pedidos, by = "customer_id")
#write.csv(tiendas_caba_m2, "tiendas_caba_m2.csv", row.names = F)
