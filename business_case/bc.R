library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
library(caret)
options(scipen = 999)


# Leo archivos
resultado_m1_m2 <- read_csv("bases_modelo/resultado_m1_m2.csv")
View(resultado_m1_m2)

tiendas_caba_m2 <- read_csv("bases_modelo/tiendas_caba_m2.csv")
View(tiendas_caba_m2)

tiendas_caba_m1<- read_csv("bases_modelo/tiendas_caba_m1.csv")
View(tiendas_caba_m1)

# PCA - me quedo con las primeras 18 componentes principales, que explican más del 75%
variables_numericas <- tiendas_caba_m1 %>% select(-id, -cluster) %>% 
  select_if(is.numeric)
variables_booleanas <- tiendas_caba_m1 %>% select(-id, -cluster) %>% 
  select_if(is.logical)
variables_numericas_scaled <- scale(variables_numericas)
tiendas_caba_m1_scaled <- cbind(variables_numericas_scaled, variables_booleanas)
pca_resultado <- prcomp(tiendas_caba_m1_scaled, center = TRUE, scale. = FALSE)
pca_scores <- as.data.frame(pca_resultado$x[, 1:18])
pca_scores <- cbind(id = tiendas_caba_m1$id, pca_scores)

# Matriz de confusion
conf_matrix <- confusionMatrix(factor(resultado_m1_m2$predicted_cluster), factor(resultado_m1_m2$cluster))
print(conf_matrix)

tiendas_fuera_diagonal <- resultado_m1_m2 %>% filter(cluster != predicted_cluster)
tiendas_diagonal <- resultado_m1_m2 %>% filter(cluster == predicted_cluster)

# Paso 1: se definen los n vecinos mas cercanos para cada tienda fuera de la diagonal
  ## Se tienen en cuenta únicamente las variables del primer modelo.
  ## A cada tienda fuera de la diagonal principal se le calculan sus vecinos, es decir:
    ### Si una tienda fue categorizada como cluster 1 en el primer modelo, pero cluster 2
    ### en el segundo, sus vecinos más cercanos saldrán de aquellas tiendas que hayan sido
    ### clusterizadas como cluster 1 en ambos modelos

calcular_distancia_tiendas <- function(id_tienda1, id_tienda2, pca_scores) {
  tienda1_scores <- pca_scores[pca_scores$id == id_tienda1, -1]
  tienda2_scores <- pca_scores[pca_scores$id == id_tienda2, -1]
  
  if (nrow(tienda1_scores) == 0 || nrow(tienda2_scores) == 0) {
    stop("Una o ambas IDs de tiendas no son válidas.")
  }
  
  distancia <- sqrt(sum((tienda1_scores - tienda2_scores) ^ 2))
  
  return(distancia)
}

vecinos_mas_cercanos <- data.frame(
  id = 0,               
  vecino_1 = 0,          
  vecino_2 = 0,          
  vecino_3 = 0,        
  vecino_4 = 0,          
  vecino_5 = 0,          
  dist_vecino_1 = 0,     
  dist_vecino_2 = 0,     
  dist_vecino_3 = 0,    
  dist_vecino_4 = 0,     
  dist_vecino_5 = 0      
)

for(i in c(1:nrow(tiendas_fuera_diagonal))){
  print(i)
  id <- tiendas_fuera_diagonal[[4]][i]
  num_cluster <- tiendas_fuera_diagonal[[5]][i]
  distancias <- data.frame(id = 0, distancia = 0)

  tiendas_mismo_cluter <- tiendas_diagonal %>% filter(cluster == num_cluster)
  
  for(j in c(1:nrow(tiendas_mismo_cluter))){
    dist <- calcular_distancia_tiendas(id, tiendas_mismo_cluter[[4]][j], pca_scores = pca_scores)
    aux <- c(tiendas_mismo_cluter[[4]][j], dist)
    distancias <- distancias %>% rbind(aux)
  }
  
  distancias <- distancias %>% filter(id != 0) %>% arrange(distancia)
  
  aux_1 <- c(id, distancias[1,1], distancias[2,1], distancias[3,1], distancias[4,1], distancias[5,1], distancias[1,2], distancias[2,2], distancias[3,2], distancias[4,2], distancias[5,2])
  vecinos_mas_cercanos <- vecinos_mas_cercanos %>% rbind(aux_1)
}

#vecinos_mas_cercanos <- vecinos_mas_cercanos %>% filter(id != 0)
#write.csv(vecinos_mas_cercanos, "vecinos_mas_cercanos.csv", row.names = F)
