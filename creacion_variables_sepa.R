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

# 10. Variable negocios_24hs_200m
comercios_24hs <- sucursales_comercios %>%
  filter(
    sucursales_lunes_horario_atencion == "00:00 a 24:00" |
      sucursales_martes_horario_atencion == "00:00 a 24:00" |
      sucursales_miercoles_horario_atencion == "00:00 a 24:00" |
      sucursales_jueves_horario_atencion == "00:00 a 24:00" |
      sucursales_viernes_horario_atencion == "00:00 a 24:00" |
      sucursales_sabado_horario_atencion == "00:00 a 24:00" |
      sucursales_domingo_horario_atencion == "00:00 a 24:00"
  )

contar_negocios_24hs_cercanos <- function(lat, lon, comercios_24hs, distancia_radio) {
  distancias <- distHaversine(
    cbind(comercios_24hs$sucursales_longitud, comercios_24hs$sucursales_latitud), 
    c(lon, lat)
  )
  count <- sum(distancias <= distancia_radio)
  return(count)
}

tiendas_caba <- tiendas_caba %>%
  rowwise() %>%
  mutate(negocios_24hs_cercanos_1000m = contar_negocios_24hs_cercanos(
    latitude,
    longitude,
    comercios_24hs,
    1000
  ))

# 11. Negocios abiertos fin de semana
comercios_fin_de_semana <- sucursales_comercios %>%
  filter(
      sucursales_sabado_horario_atencion != "Cerrado" &
      sucursales_domingo_horario_atencion!= "Cerrado"
  )

tiendas_caba <- tiendas_caba %>%
  rowwise() %>%
  mutate(negocios_findesemana_200m = contar_negocios_24hs_cercanos(
    latitude,
    longitude,
    comercios_fin_de_semana,
    200
  ))

# 12 a 18 - Variables booleanas para cada dia de la semana

## Función para extraer los días de apertura y el número de horarios
extraer_info_horarios <- function(working_days_json) {
  
  working_days_list <- fromJSON(working_days_json)
  weekdays <- working_days_list$weekdays
  n_horarios <- length(working_days_list$hourRange)
  
  df <- data.frame(
    mon_open = weekdays$mon,
    tue_open = weekdays$tue,
    wed_open = weekdays$wed,
    thu_open = weekdays$thu,
    fri_open = weekdays$fri,
    sat_open = weekdays$sat,
    sun_open = weekdays$sun,
    n_horarios = n_horarios
  )
  
  return(df)
}

## Aplico la función a la columna working_days, para cada tienda
aux_customers <- customers_ar %>%
  rowwise() %>%
  mutate(info_horarios = list(extraer_info_horarios(working_days))) %>%
  unnest_wider(info_horarios)

aux_customers <- aux_customers %>%
  select(id, mon_open, tue_open, wed_open, thu_open, fri_open, sat_open, sun_open, n_horarios)

aux_customers = aux_customers %>% select(-n_horarios)
aux_customers = aux_customers %>% filter(id %in% tiendas_caba$id)

tiendas_caba = merge(tiendas_caba, aux_customers, by = "id")

# Columna: supermercado_chino
aux_2 <- customers_ar %>%
  mutate(supermercado_chino = ifelse(
    !is.na(additional_info) & str_detect(tolower(additional_info), "chino") |
      !is.na(name) & str_detect(tolower(name), "chino"),
    TRUE,
    FALSE
  )) %>%
  select(id, supermercado_chino)

aux_2 = aux_2 %>% filter(id %in% tiendas_caba$id)

tiendas_caba = merge(tiendas_caba, aux_2, by = "id")

# Columna: supermercado
aux_3 <- customers_ar %>%
  mutate(supermercado = ifelse(
    !is.na(additional_info) & str_detect(tolower(additional_info), "super") |
    !is.na(name) & str_detect(tolower(name), "super") |
    !is.na(additional_info) & str_detect(tolower(additional_info), "súper") |
    !is.na(name) & str_detect(tolower(name), "súper"),
    TRUE,
    FALSE
  )) %>%
  select(id, supermercado)

aux_3 = aux_3 %>% filter(id %in% tiendas_caba$id)

tiendas_caba = merge(tiendas_caba, aux_3, by = "id")

############################################################
#write.csv(tiendas_caba, "variables_nuevas_tiendas.csv", row.names = FALSE)
