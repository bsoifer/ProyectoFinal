library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
library(caret)
library(scatterplot3d)
options(scipen = 999)

# Leo archivos
resultado_m1_m2 <- read_csv("bases_modelo/resultado_m1_m2_nuevo.csv")
View(resultado_m1_m2)

#tiendas_caba_m2 <- read_csv("bases_modelo/tiendas_caba_m2.csv")
#View(tiendas_caba_m2)

tiendas_caba_m1 <- read_csv("bases_modelo/tiendas_caba_m1_V2.csv")
View(tiendas_caba_m1)

# Tratamiento tiendas_caba_m1
#new_cluster <- tiendas_caba_m1$cluster
#new_cluster
#for (i in c(1:length(new_cluster))) {
#  if(new_cluster[i]==0){
#    new_cluster[i] = 1
#  }else if(new_cluster[i]==1){
#      new_cluster[i] = 2
#  }else{
#    new_cluster[i] = 0
#  }
#}

#tiendas_caba_m1 <- tiendas_caba_m1 %>% select(-cluster)
#tiendas_caba_m1$cluster = new_cluster
#tiendas_caba_m1 <- tiendas_caba_m1 %>% select(-mon_open, -tue_open, -wed_open, -thu_open, -fri_open, -sat_open, -sun_open)
#write.csv(tiendas_caba_m1, "tiendas_caba_m1_v2.csv", row.names = F)

# 1) Carga de dataset y guardo temporalmente las columna de cluster y id
tiendas <- tiendas_caba_m1
clusters <- tiendas$cluster
ids <- tiendas$id
tiendas <- tiendas %>% select(-cluster, -id)  

# 2) Defino columnas que no voy a escalar
columns_to_exclude <- c('supermercado_chino', 'supermercado',
                        'en_avenida', 'producto_1_freq', 'producto_2_freq', 'producto_3_freq',
                        'producto_4_freq', 'producto_5_freq', 'categoria_1_freq', 'categoria_2_freq',
                        'categoria_3_freq', 'categoria_4_freq', 'categoria_5_freq', 'comuna_freq',
                        'porc_nbi', 'porc_mujeres', 'porc_varones')

# 3) Se escalan las columnas restantes
columns_to_scale <- setdiff(names(tiendas), columns_to_exclude)
df_tiendas_scaled <- tiendas %>%
  mutate(across(all_of(columns_to_scale), scale))

# 4) Uno las columnas escaladas con las no escaladas
df_tiendas_scaled <- bind_cols(df_tiendas_scaled[, columns_to_scale],
                               tiendas[, columns_to_exclude])

# 5) Se realiza PCA
pca_result <- prcomp(df_tiendas_scaled, scale. = FALSE)

# 6) Graficar la varianza explicada acumulada
explained_variance <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))
ggplotly(ggplot(data.frame(PC = 1:length(explained_variance), Variance = explained_variance), aes(x = PC, y = Variance)) +
  geom_line() +
  geom_point() +
  labs(title = "Varianza explicada por los componentes principales", x = "Número de componentes", y = "Varianza explicada acumulada"))

# 7) Selecciono el número de componentes
n_components <- 21 # Explican el 80% de la varianza aproximadamente

# 8) Aplico PCA con el número de componentes seleccionado
pca_result_n <- prcomp(df_tiendas_scaled, scale. = FALSE, rank. = n_components)

# 9) Se extraen los valores transformados
pca_transformed <- pca_result_n$x

# 10) Calcular la varianza explicada para los componentes seleccionados
explained_var_ratio <- pca_result_n$sdev^2 / sum(pca_result_n$sdev^2)
explained_var_ratio_selected <- explained_var_ratio[1:n_components]

# 11) Multiplicar cada componente por su varianza explicada
pca_transformed_scaled <- sweep(pca_transformed, 2, explained_var_ratio_selected, `*`)

# 12) Crear un data frame para plotly con las primeras componentes y el cluster
pca_plot_data <- as.data.frame(pca_transformed_scaled)
pca_plot_data$cluster <- as.factor(clusters)
pca_plot_data$id <- ids

# 13) Gráfico 2D interactivo con las primeras dos componentes
plot_2d <- plot_ly(pca_plot_data, 
                   x = ~PC1, y = ~PC2, 
                   color = ~cluster, colors = c('orange', "blue", 'purple'),
                   text = ~paste('ID:', id, 
                                 '<br>Cluster:', cluster),
                   type = 'scatter', mode = 'markers',
                   marker = list(size = 8, opacity = 0.8)) %>%
  layout(title = list(text = 'Resultados PCA - Primeras dos componentes', 
                      font = list(size = 20)),
         xaxis = list(title = 'PC1', titlefont = list(size = 14), tickfont = list(size = 12)),
         yaxis = list(title = 'PC2', titlefont = list(size = 14), tickfont = list(size = 12)),
         hovermode = 'closest')


# 14) Gráfico 3D interactivo con las primeras tres componentes
plot_3d <- plot_ly(pca_plot_data, 
                   x = ~PC1, y = ~PC2, z = ~PC3, 
                   color = ~cluster, colors = c('orange', "blue", 'purple'),
                   text = ~paste('ID:', id, 
                                 '<br>Cluster:', cluster),
                   type = 'scatter3d', mode = 'markers',
                   marker = list(size = 5, opacity = 0.8)) %>%
  layout(title = list(text = 'Resultados PCA - Primeras tres componentes', 
                      font = list(size = 20)),
         scene = list(xaxis = list(title = 'PC1', titlefont = list(size = 14), tickfont = list(size = 12)),
                      yaxis = list(title = 'PC2', titlefont = list(size = 14), tickfont = list(size = 12)),
                      zaxis = list(title = 'PC3', titlefont = list(size = 14), tickfont = list(size = 12))),
         hovermode = 'closest')

# Mostrar ambos gráficos
plot_2d
plot_3d


#################################################################

# Gráficos donde se visualizan los puntos que no fueron clasificados correctamente

# 1) Crear un data frame para plotly con las primeras componentes y el cluster
pca_plot_data <- as.data.frame(pca_transformed_scaled)
pca_plot_data <- pca_plot_data %>% cbind('id' = ids, "cluster" = clusters)
pca_plot_data <- pca_plot_data %>% merge(resultado_m1_m2, by.x = "id", by.y = "customer_id")
pca_plot_data <- pca_plot_data %>% select(-cantidad_ordenes, -promedio_ordenes_mes, -promedio_por_pedido, -cluster.y, -cantidad_proveedores, -porcentaje_max_supplier, -porcentaje_descuento_total, -proporcion_ordenes_descuentos)
pca_plot_data <- pca_plot_data %>% rename("cluster" = cluster.x)
pca_plot_data$mismatch = case_when(
  pca_plot_data$cluster != pca_plot_data$predicted_cluster ~ T,
  T ~ F
)

# 2) Definir colores: 
# Cluster 0 -> orange, Cluster 1 -> blue, Cluster 2 -> purple, Mismatch -> red
colors <- ifelse(pca_plot_data$mismatch, '', 
                 ifelse(pca_plot_data$cluster == 0, 'orange',
                        ifelse(pca_plot_data$cluster == 1, 'blue', 'purple')))

# 3) Gráfico 2D interactivo con las primeras dos componentes
plot_2d.2 <- plot_ly(pca_plot_data, 
                   x = ~PC1, y = ~PC2, 
                   marker = list(color = colors),
                   text = ~paste('Customer ID:', id, '<br>Cluster:', cluster, '<br>Predicted Cluster:', predicted_cluster),
                   type = 'scatter', mode = 'markers') %>%
  layout(title = 'PCA - Primeras dos componentes',
         xaxis = list(title = 'PC1'),
         yaxis = list(title = 'PC2'))

# 4) Gráfico 3D interactivo con las primeras tres componentes
plot_3d.2 <- plot_ly(pca_plot_data, 
                   x = ~PC1, y = ~PC2, z = ~PC3, 
                   marker = list(color = colors, size = 5),
                   text = ~paste('Customer ID:', id, '<br>Cluster:', cluster, '<br>Predicted Cluster:', predicted_cluster),
                   type = 'scatter3d', mode = 'markers') %>%
  layout(title = 'Resultados PCA - Primeras tres componentes',
         scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')))

# Mostrar ambos gráficos
plot_2d.2
plot_3d.2
