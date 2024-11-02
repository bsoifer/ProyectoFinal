library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
library(caret)
library(lubridate)
options(scipen = 999)

# 1. Se lee la base que contiene los montos por pedidos para cada tienda
orders <- read_csv("bases/orders_ar.csv")
View(orders)

tiendas_caba_m1 <- read_csv("bases_modelo/tiendas_caba_m1_v2.csv")
View(tiendas_caba_m1)

#2. Se seleccionan las tiendas que son objeto de estudio
orders <- orders %>% filter(customer_id %in% tiendas_caba_m1$id)

#3. Se suma el monto total, por mes y tienda
orders <- orders %>% filter(!is.na(created_at))
monto_por_mes <- orders %>% group_by(customer_id, "month" = month(created_at), "year" = year(created_at)) %>% summarise(monto = sum(total))

montos_promedio <- monto_por_mes %>% group_by(customer_id) %>% summarise(monto_promedio = mean(monto))

#4. Se eliminan como "posibles vecinos" a aquellas tiendas que sean outliers
boxplot(montos_promedio$monto_promedio)

iqr <- quantile(montos_promedio$monto_promedio, 0.75) - quantile(montos_promedio$monto_promedio, 0.25)
li <- quantile(montos_promedio$monto_promedio, 0.25) - iqr * 1.5
ls <- quantile(montos_promedio$monto_promedio, 0.75) + iqr * 1.5

montos_promedio <- montos_promedio %>% filter(monto_promedio >= li & monto_promedio <= ls)
boxplot(montos_promedio$monto_promedio)

#5. Dataset: tiendas que pueden ser consideradas como posibles vecinas
posibles_vecinos <- montos_promedio
#write.csv(posibles_vecinos, "posibles_vecinos.csv", row.names = F)
