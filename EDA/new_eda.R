library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
library(caret)
options(scipen = 999)

# Bases
orders <- read_csv("bases/orders_ar.csv")
View(orders)

resultado_m1_m2 <- read_csv("bases_modelo/resultado_m1_m2_nuevo.csv")
View(resultado_m1_m2)

tiendas_caba_m1_v2 <- read_csv("bases_modelo/tiendas_caba_m1_v2.csv")
View(tiendas_caba_m1_v2)

tiendas_caba_m1 <- read_csv("bases_modelo/tiendas_caba_m1.csv")
View(tiendas_caba_m1)

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

View(tiendas)
