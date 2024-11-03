library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
library(caret)
library(lubridate)
options(scipen = 999)

# 1. Se lee la base que contiene los montos por pedidos para cada tienda
orders <- read_csv("business_case/montos_ajustados_por_pedido.csv")
View(orders)

tiendas_caba_m1 <- read_csv("bases_modelo/tiendas_caba_m1_v2.csv")
View(tiendas_caba_m1)

boxplot(montos_promedio$monto_promedio)

#2. Se seleccionan las tiendas que son objeto de estudio
orders <- orders %>% filter(customer_id %in% tiendas_caba_m1$id)

#3. Se suma el monto total, por mes y tienda
orders <- orders %>% filter(!is.na(created_at))
monto_por_mes <- orders %>% group_by(customer_id, "month" = month(created_at), "year" = year(created_at)) %>% summarise(monto = sum(adjusted_total))

montos_promedio <- monto_por_mes %>% group_by(customer_id) %>% summarise(monto_promedio = mean(monto))

################################ Calculo decil inferior (estimacion BC)
iqr <- quantile(montos_promedio$monto_promedio, 0.75) - quantile(montos_promedio$monto_promedio, 0.25)
li <- quantile(montos_promedio$monto_promedio, 0.25) - iqr * 1.5
ls <- quantile(montos_promedio$monto_promedio, 0.75) + iqr * 1.5

montos_promedio <- montos_promedio %>% filter(monto_promedio >= li & monto_promedio <= ls)

quantile(montos_promedio$monto_promedio, 0.5)
decil_inferior <- montos_promedio %>% filter(monto_promedio <= quantile(monto_promedio, 0.1))
decil_inferior$distancia_mediana =  505228.8 - decil_inferior$monto_promedio 
mean(decil_inferior$distancia_mediana)

###############################

#4. Dataset: tiendas que pueden ser consideradas como posibles vecinas
posibles_vecinos <- montos_promedio
#write.csv(posibles_vecinos, "posibles_vecinos_v2.csv", row.names = F)
