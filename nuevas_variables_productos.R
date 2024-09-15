library(sf)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotly)
library(readxl)
library(readr)
library(stringr)
library(leaflet)
library(geosphere)
library(tidyr)
library(jsonlite)
options(scipen = 999)

# Bases de clientes, órdenes e items

customers_ar <- read_csv("bases/customers_ar.csv")
View(customers_ar)

items_ar <- read_csv("bases/items_ar.csv")
View(items_ar)

orders_ar <- read_csv("bases/orders_ar.csv")
View(orders_ar)

# Base con todas las variabes
tiendas_caba_v2 <- read_csv("bases_modelo/tiendas_caba_v2.csv")
View(tiendas_caba_v2)

# Extraigo de cada tienda, los 5 productos que más compró
pedidos_por_tienda = merge(items_ar, orders_ar, by = "order_id")
pedidos_por_tienda = merge(pedidos_por_tienda, customers_ar, by = "customer_id")
View(pedidos_por_tienda)

pedidos_por_tienda = pedidos_por_tienda %>% filter(ean != 11111122222222)
pedidos_por_tienda = pedidos_por_tienda %>% group_by(customer_id, ean, category_id) %>% summarise(total_por_producto = sum(units))
pedidos_por_tienda = pedidos_por_tienda %>% arrange(customer_id, desc(total_por_producto))


# Genero las nuevas variables
rellenar_con_cero <- function(x, n = 5) {
  length(x) <- n
  x[is.na(x)] <- 0
  return(x)
}

productos_top_5 <- pedidos_por_tienda %>%
  group_by(customer_id) %>%
  arrange(desc(total_por_producto)) %>%
  slice_head(n = 5) %>%
  summarise(top_productos = list(rellenar_con_cero(ean, 5)))

categorias_top_5 <- pedidos_por_tienda %>%
  group_by(customer_id, category_id) %>%
  summarise(total_categoria = sum(total_por_producto)) %>%
  arrange(customer_id, desc(total_categoria)) %>%
  slice_head(n = 5) %>%
  summarise(top_categorias = list(rellenar_con_cero(category_id, 5))) 


# Agrego nuevas variables

productos_tiendas = productos_top_5 %>% select(customer_id)
productos_tiendas$producto_1 = "0"
productos_tiendas$producto_2 = "0"
productos_tiendas$producto_3 = "0"
productos_tiendas$producto_4 = "0"
productos_tiendas$producto_5 = "0"
productos_tiendas$categoria_1 = 0
productos_tiendas$categoria_2 = 0
productos_tiendas$categoria_3 = 0
productos_tiendas$categoria_4 = 0
productos_tiendas$categoria_5 = 0

for (i in c(1:nrow(productos_top_5))) {
  productos_tiendas[i,2] = productos_top_5$top_productos[[i]][1]
  productos_tiendas[i,3] = productos_top_5$top_productos[[i]][2]
  productos_tiendas[i,4] = productos_top_5$top_productos[[i]][3]
  productos_tiendas[i,5] = productos_top_5$top_productos[[i]][4]
  productos_tiendas[i,6] = productos_top_5$top_productos[[i]][5]
  productos_tiendas[i,7] = categorias_top_5$top_categorias[[i]][1]
  productos_tiendas[i,8] = categorias_top_5$top_categorias[[i]][2]
  productos_tiendas[i,9] = categorias_top_5$top_categorias[[i]][3]
  productos_tiendas[i,10] = categorias_top_5$top_categorias[[i]][4]
  productos_tiendas[i,11] = categorias_top_5$top_categorias[[i]][5]
}

aux = customers_ar %>% select(id,customer_id)
productos_tiendas = merge(productos_tiendas, aux, by = "customer_id")

tiendas_caba_v3 = merge(tiendas_caba_v2, productos_tiendas, by = "id")
tiendas_caba_v3 = tiendas_caba_v3 %>% select(-customer_id)

#write.csv(tiendas_caba_v3, "tiendas_caba_v3.csv", row.names = F)
