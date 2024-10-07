library(tidyverse)
library(dplyr)

calcular_distancia_matriz <- function(pca_scores) {
  ids <- pca_scores$id
  num_tiendas <- nrow(pca_scores)
  
  matriz_distancias <- matrix(0, nrow = num_tiendas, ncol = num_tiendas)
  rownames(matriz_distancias) <- ids
  colnames(matriz_distancias) <- ids
  
  for (i in 1:num_tiendas) {
    for (j in 1:num_tiendas) {
      if (i != j) {
        distancia <- sqrt(sum((pca_scores[i, -1] - pca_scores[j, -1])^2))
        matriz_distancias[i, j] <- distancia
      }
    }
  }
  return(matriz_distancias)
}

# Aplicar la función de distancias
matriz_distancias_tiendas <- calcular_distancia_matriz(pca_transformed_scaled)

#write.csv(matriz_distancias_tiendas, "matriz_distancias_tiendas.csv", row.names = TRUE)


# Método el codo para determinar distancias
matriz_distancias_tiendas <- read_csv("business_case/matriz_distancias_tiendas.csv")
View(matriz_distancias_tiendas)


tienda_ids <- matriz_distancias_tiendas[-1, 1]
matriz_distancias_tiendas_sin_ids <- as.matrix(matriz_distancias_tiendas[-1, -1])


promedios_generales <- numeric()

for (k in 1:20) {
  promedios_por_tienda <- numeric()
  
  for (i in 1:nrow(matriz_distancias_tiendas_sin_ids)) {
    distancias <- as.numeric(matriz_distancias_tiendas_sin_ids[i, ])
    distancias <- distancias[distancias > 0]
    
    distancias_k_menores <- sort(distancias)[1:k]
 
    promedio_tienda <- mean(distancias_k_menores)
    
    promedios_por_tienda <- c(promedios_por_tienda, promedio_tienda)
  }
  
  promedio_general_k <- mean(promedios_por_tienda)
  
  promedios_generales <- c(promedios_generales, promedio_general_k)
}

df_codo <- data.frame(k = 1:20, promedio_general = promedios_generales)

ggplot(df_codo, aes(x = k, y = promedio_general)) +
  geom_line(color = "black", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Elbow Method: Promedio de Distancias",
       x = "Cantidad de tiendas",
       y = "Promedio General de Distancias") +
  theme_minimal()
