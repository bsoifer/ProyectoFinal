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

# Bases de clientes, órdenes e items

customers_ar <- read_csv("bases/customers_ar.csv")
View(customers_ar)

items_ar <- read_csv("bases/items_ar.csv")
View(items_ar)

orders_ar <- read_csv("bases/orders_ar.csv")
View(orders_ar)

# Lectura de la base de sucursales con información de comercios

sucursales_comercios <- read_csv("bases/sucursales_comercios.csv", 
                                 col_types = cols(...1 = col_skip(), id_comercio = col_character(), 
                                                  id_bandera = col_character()))

# Eliminar filas con NA en latitud o longitud de sucursales_comercios
sucursales_comercios <- sucursales_comercios %>%
  filter(!is.na(sucursales_latitud) & !is.na(sucursales_longitud))


View(sucursales_comercios)

# Lectura de bases de tiendas de CABA
tiendas_caba <- read_csv("bases/tiendas_caba.csv")
View(tiendas_caba)

tiendas_caba_sf <- st_as_sf(tiendas_caba, wkt = "geometry", crs = 4326)

tiendas_caba$longitude <- st_coordinates(tiendas_caba_sf)[,1]
tiendas_caba$latitude <- st_coordinates(tiendas_caba_sf)[,2]

leaflet(tiendas_caba) %>%
  addTiles() %>%  # Añadir un mapa base
  addCircleMarkers(
    ~longitude, 
    ~latitude, 
    radius = 0.5,
    color = "blue",
    fillOpacity = 0.2,
    popup = ~paste("Lat:", latitude, "Lon:", longitude)
  )

# Variable 1: negocios cercanos

contar_negocios_cercanos <- function(lat, lon, sucursales_comercios, distancia_radio) {
  distancias <- distHaversine(
    cbind(sucursales_comercios$sucursales_longitud, sucursales_comercios$sucursales_latitud), 
    c(lon, lat)
  )
  count <- sum(distancias <= distancia_radio)
  return(count)
}


tiendas_caba <- tiendas_caba %>%
  rowwise() %>%
  mutate(negocios_cercanos_200m = contar_negocios_cercanos(
    latitude,
    longitude,
    sucursales_comercios,
    200
  ))

# 2. Negocios cercanos 1000m

tiendas_caba <- tiendas_caba %>%
  rowwise() %>%
  mutate(negocios_cercanos_1000m = contar_negocios_cercanos(
    latitude,
    longitude,
    sucursales_comercios,
    1000
  ))

#3. Tiendas cercanas 200m

contar_tiendas_cercanas <- function(lat, lon, tiendas_caba, radio = 200) {
  distancias <- distHaversine(
    cbind(tiendas_caba$longitude, tiendas_caba$latitude),  # Coordenadas de todas las tiendas
    c(lon, lat)
  )
  
  count <- sum(distancias <= radio & distancias > 0, na.rm = TRUE)  # distancias > 0 para excluir la tienda actual
  
  return(count)
}

tiendas_caba <- tiendas_caba %>%
  rowwise() %>%
  mutate(tiendas_cercanas_200m = contar_tiendas_cercanas(latitude, longitude, tiendas_caba, radio = 200))

# 4 a 9 - Supermercados, autoservicios e hipermercados en 200m

sucursales_comercios_2 = sucursales_comercios %>% filter(sucursales_tipo %in% c("Supermercado", "SUPERMERCADO"))
sucursales_comercios_3 = sucursales_comercios %>% filter(sucursales_tipo %in% c("Hipermercado", "HIPERMERCADO"))
sucursales_comercios_4 = sucursales_comercios %>% filter(sucursales_tipo %in% c("Autoservicio"))


tiendas_caba <- tiendas_caba %>%
  rowwise() %>%
  mutate(supermercados_200m = contar_negocios_cercanos(
    latitude,
    longitude,
    sucursales_comercios_2,
    200
  ))

tiendas_caba <- tiendas_caba %>%
  rowwise() %>%
  mutate(supermercados_1000m = contar_negocios_cercanos(
    latitude,
    longitude,
    sucursales_comercios_2,
    1000
  ))

tiendas_caba <- tiendas_caba %>%
  rowwise() %>%
  mutate(hipermercado_200m = contar_negocios_cercanos(
    latitude,
    longitude,
    sucursales_comercios_3,
    200
  ))

tiendas_caba <- tiendas_caba %>%
  rowwise() %>%
  mutate(hipermercado_1000m = contar_negocios_cercanos(
    latitude,
    longitude,
    sucursales_comercios_3,
    1000
  ))

tiendas_caba <- tiendas_caba %>%
  rowwise() %>%
  mutate(autoservicio_200m = contar_negocios_cercanos(
    latitude,
    longitude,
    sucursales_comercios_4,
    200
  ))

tiendas_caba <- tiendas_caba %>%
  rowwise() %>%
  mutate(autoservicio_1000m = contar_negocios_cercanos(
    latitude,
    longitude,
    sucursales_comercios_4,
    1000
  ))