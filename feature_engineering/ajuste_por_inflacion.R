library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
options(scipen = 999)

items <- read_csv("items_ar.csv")
View(items)

orders <- read_csv("orders_ar.csv")
View(orders)

customers <- read_csv("customers_ar.csv")
View(customers)

# Total de monto de pedido por cada customer
pedidos =  orders %>% select(customer_id, total, created_at)


# Ajuste por inflación con la variacion del IPC
ipc <- data.frame(
  fecha = seq(as.Date("2021-01-01"), as.Date("2024-08-01"), by="month"),
  variacion = c(
    4, 3.6, 4.8, 4.1, 3.3, 3.2, 3, 2.5, 3.5, 3.5, 2.5, 3.8,
    3.9, 4.7, 6.7, 6, 5.1, 5.3, 7.4, 7, 6.2, 6.3, 4.9, 5.1,
    6, 6.6, 7.7, 8.4, 7.8, 6, 6.3, 12.4, 12.7, 8.3, 12.8, 25.5,
    20.6, 13.2, 11, 8.8, 4.2, 4.6, 4, 4.2
  )
)

ipc$variacion <- ipc$variacion / 100
ipc$indice <- cumprod(1 + ipc$variacion)

indice_final <- ipc$indice[ipc$fecha == as.Date("2024-08-01")]

pedidos$created_at <- as.Date(as.POSIXct(pedidos$created_at))

pedidos$mes_inicio <- as.Date(format(pedidos$created_at, "%Y-%m-01"))

pedidos <- merge(pedidos, ipc, by.x = "mes_inicio", by.y = "fecha")

pedidos$adjusted_total <- pedidos$total * (indice_final / pedidos$indice)

# Graficos
pedidos = pedidos %>% group_by(customer_id) %>% summarise(promedio_por_pedido = mean(adjusted_total)) 
boxplot(pedidos$promedio_por_pedido)

# Eliminando el outlier
pedidos = pedidos %>% filter(promedio_por_pedido < 3000000)
boxplot(pedidos$promedio_por_pedido, ylab="Promedio por Pedido")
title(main="Monto promedio de pedido por tienda")

q1 <- quantile(pedidos$promedio_por_pedido, 0.25)
mediana <- median(pedidos$promedio_por_pedido)
q3 <- quantile(pedidos$promedio_por_pedido, 0.75)
iqr <- IQR(pedidos$promedio_por_pedido)

abline(h=q1, col="red", lty=2)
abline(h=mediana, col="blue", lty=2)
abline(h=q3, col="green", lty=2)

mtext(side=1, line=4, at=1, text=sprintf("Q1: %.2f", q1), cex=0.8)
mtext(side=1, line=3, at=1, text=sprintf("Mediana: %.2f", mediana), cex=0.8, col="blue")
mtext(side=1, line=2, at=1, text=sprintf("Q3: %.2f", q3), cex=0.8)
mtext(side=1, line=1, at=1, text=sprintf("IQR: %.2f", iqr), cex=0.8)

# Estimacion de ganancia por recomendación
decil_1 = quantile(pedidos$promedio_por_pedido, 0.1)

pedidos_2 = pedidos %>% filter(promedio_por_pedido < decil_1)
pedidos_2$distancia_a_media = 183340.61 - pedidos_2$promedio_por_pedido
mean(pedidos_2$distancia_a_media) * 463

