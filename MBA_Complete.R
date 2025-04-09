library(igraph)
library(tidyverse)
library(qgraph)
library(GGally)
library(dplyr)
library(purrr)
library(RColorBrewer)
library(viridis)
library(ggcorrplot)
library(tidyr)
library(corrplot)

set.seed(123)

### ----- Limpeza do espaço de trabalho do R 
rm(list=ls())

# Leitura da tabela contendo os dados (Usar caminho relativo)
df <- read.table(  file = "MBA.txt",  dec = ".",  sep = "\t",  header = T)

##################### Calcular a matriz de correlação de Spearman e p-valores
# Calcular a matriz de correlação de Spearman
cor_matrix <- cor(df, method = "spearman", use = "pairwise.complete.obs")

# Função para calcular p-valores da correlação Spearman (apenas a parte inferior da matriz)
cor_pvalues <- function(x, method = "spearman") {
  n <- ncol(x)
  p_matrix <- matrix(NA, n, n)
  colnames(p_matrix) <- rownames(p_matrix) <- colnames(x)
  
  for (i in 1:n) {
    for (j in 1:i) { # Apenas parte inferior
      if (i != j) {
        test <- cor.test(x[, i], x[, j], method = method, exact = FALSE) # Aproximação p/ empates
        p_matrix[i, j] <- test$p.value
      } else {
        p_matrix[i, j] <- NA # Remover autocorrelação
      }
    }
  }
  return(p_matrix)
}

# Calcular matriz de p-valores
p_matrix <- cor_pvalues(df)

# Criar matriz de asteriscos para significância (para o gráfico de correlação)
sig_matrix <- ifelse(p_matrix < 0.05, "*", "") # * para p < 0.05

# Plotar a matriz de correlação de Spearman
corrplot(cor_matrix,
         method = "color",       # Exibir como mapa de cores
         type = "lower",         # Apenas a metade inferior da matriz
         tl.col = "black",       # Cor dos rótulos das variáveis
         tl.srt = 45,            # Rotação dos rótulos
         addCoef.col = "black",  # Adicionar valores de correlação
         p.mat = p_matrix,       # Matriz de p-valores
         sig.level = c(0.05),     # Níveis de significância
         insig = "label_sig",     # Adicionar "*" nas correlações significativas
         number.cex = 0.7,       # Tamanho dos números das correlações (aumentei para melhor visualização)
         pch.cex = 1.2)          # Tamanho dos `*` (aumentei para melhor visualização)

##################### Criar a rede no pacote "igraph"
# Aplicar limiar de significância (p < 0.05) e limiar de correlação de 0.3 para criar a rede
binary_adj_matrix <- ifelse(p_matrix < 0.05 & abs(cor_matrix) > 0.3, 1, 0)

# Criar a rede igraph a partir da matriz binária
net <- graph_from_adjacency_matrix(binary_adj_matrix, mode = "directed", diag = FALSE)

# Calcular os graus dos nós
degree_values <- degree(net)

# Obter os graus únicos e ordenados para criar a legenda
unique_degrees <- sort(unique(degree_values))
n_unique_degrees <- length(unique_degrees)

# Escolher a paleta de cores viridis com base no número de graus únicos
degree_palette <- viridis(n_unique_degrees)

# Mapear os graus dos nós para as cores da paleta
vertex_colors <- degree_palette[match(degree_values, unique_degrees)]

# Plotar a rede com cores baseadas no grau e adicionar legenda
plot(net,
     layout = layout.fruchterman.reingold,
     vertex.label = V(net)$name,
     vertex.size = 10,
     vertex.color = vertex_colors,
     edge.arrow.size = 0.2,
     edge.width = E(net)$weight * 2,
     edge.color = "gray50",
     main = "Rede de Correlações Colorida por Grau (Número de Conexões)")

# Adicionar legenda
legend("bottomright",
       legend = unique_degrees,
       pch = 16,
       col = degree_palette,
       cex = 0.8,
       title = "Grau")

##################### CRIAÇÃO DE ARQUIVOS DE NÓS E ARESTAS PARA USAR NO GEPHI ###################
# 1. Create the Nodes File:
nodes <- data.frame(Id = colnames(cor_matrix))
write.csv(nodes, "nodes_3.csv", row.names = FALSE)

# 2. Create the Edges File:
edges <- as.data.frame(as.table(cor_matrix))
colnames(edges) <- c("Source", "Target", "Weight")

# Remove os auto-loops 
edges <- edges[edges$Source != edges$Target, ]

# Remove as arestas duplicadas ( a correlação é simétrica)
edges <- edges[!duplicated(apply(edges[, 1:2], 1, function(x) paste(sort(x), collapse = "_"))), ]

# Filtra as arestas baseadas na suignificância (p-valor < 0.05)
significant_edges <- edges[p_matrix[as.matrix(edges[, 1:2])] < 0.05, ]

# Exporta as arestas como um arquivo CSV
write.csv(significant_edges, "edges_3.csv", row.names = FALSE)

##################### FUNÇÃO PARA AVALIAR AS MÉTRICAS DA REDE ############
#Função para calcular p-valores da correlação Spearman
cor_pvalues <- function(x, method = "spearman") {
  n <- ncol(x)
  p_matrix <- matrix(NA, n, n)
  colnames(p_matrix) <- rownames(p_matrix) <- colnames(x)
  
  for (i in 1:n) {
    for (j in 1:i) {
      if (i != j) {
        test <- cor.test(x[, i], x[, j], method = method, exact = FALSE)
        p_matrix[i, j] <- test$p.value
      } else {
        p_matrix[i, j] <- NA
      }
    }
  }
  return(p_matrix)
}

# Calcular matriz de correlação Spearman
cor_matrix <- cor(df, method = "spearman", use = "pairwise.complete.obs")

# Calcular matriz de p-valores
p_matrix <- cor_pvalues(df)

# --- Criar a rede igraph ---
binary_adj_matrix <- ifelse(p_matrix < 0.05 & abs(cor_matrix) > 0, 1, 0)
net <- graph_from_adjacency_matrix(binary_adj_matrix, mode = "directed", diag = FALSE)

# --- Extrair os pesos das arestas existentes ---
edge_list <- as_edgelist(net)
edge_weights <- abs(cor_matrix[edge_list])

# --- Calcular métricas ---
degree_values <- degree(net)
strength_values <- strength(net, weights = edge_weights)
betweenness_values <- betweenness(net)
closeness_values <- closeness(net)
eigen_centrality_values <- eigen_centrality(net)$vector
pagerank_values <- page_rank(net)$vector
transitivity_values <- transitivity(net, type = "local")
clustering_coef_values <- transitivity(net, type = "local")
triangles_values <- count_triangles(net) / 2
eccentricity_values <- eccentricity(net)

# --- Criar um data frame com todas as métricas por nó ---
network_metrics <- data.frame(
  Node = V(net)$name,
  Degree = format(round(degree_values, 2), nsmall = 2),
  Strength = format(round(strength_values, 2), nsmall = 2),
  Betweenness = format(round(betweenness_values, 2), nsmall = 2),
  Closeness = format(round(closeness_values, 2), nsmall = 2),
  Eigenvector = format(round(eigen_centrality_values, 2), nsmall = 2),
  PageRank = format(round(pagerank_values, 2), nsmall = 2),
  Transitivity = format(round(transitivity_values, 2), nsmall = 2),
  Clustering_Coefficient = format(round(clustering_coef_values, 2), nsmall = 2),
  Triangles = format(round(triangles_values, 2), nsmall = 2),
  Eccentricity = format(round(eccentricity_values, 2), nsmall = 2)
)

# Converter colunas numéricas para numérico
numeric_cols <- names(network_metrics)[-1]
network_metrics[numeric_cols] <- lapply(network_metrics[numeric_cols], as.numeric)

# Substituir NaN por 0
network_metrics[numeric_cols] <- lapply(network_metrics[numeric_cols], function(x) ifelse(is.nan(x), 0, x))

# Calcular estatísticas descritivas
stats <- data.frame(
  Metric = numeric_cols,
  Mean = sapply(network_metrics[, numeric_cols], mean),
  Median = sapply(network_metrics[, numeric_cols], median),
  Max = sapply(network_metrics[, numeric_cols], max),
  Min = sapply(network_metrics[, numeric_cols], min),
  SD = sapply(network_metrics[, numeric_cols], sd),
  CV = sapply(network_metrics[, numeric_cols], function(x) (sd(x) / mean(x)) * 100)
)

# Exibir a tabela de estatísticas
print(stats)


##################### FUNÇÃO DE ANÁLISE DE ESTABILIDADE DA REDE ##########################
# Função para computar as métricas da rede
compute_metrics <- function(graph) {
  avg_degree <- mean(degree(graph, mode = "all"))  # Average Degree
  avg_path_length <- mean_distance(graph, directed = TRUE,  unconnected = TRUE)  # Avg Path Length
  avg_clustering <- transitivity(graph, type = "global")  # Clustering Coefficient
  
  return(c(avg_degree, avg_path_length, avg_clustering))
}

# Computa e armazena as métricas para a rede inteira 
full_metrics <- compute_metrics(net)
full_results <- data.frame(
  Metric = c("AvgDegree", "AvgPathLength", "AvgClustering"),
  Value = full_metrics
)
print("Full Network Metrics:")
print(full_results)

# Cria um dataframe para armazenar os resultados da remoção dos nós
stability_results <- data.frame(Node = V(net)$name,
                                AvgDegree = NA,
                                AvgPathLength = NA,
                                AvgClustering = NA)

# Interação de cada nó e sua remoção para avaliar o impacto
for (i in seq_along(V(net))) {
  modified_net <- delete_vertices(net, V(net)[i])  # Remove node
  if (vcount(modified_net) > 0) {  # Ensure network is not empty
    stability_results[i, 2:4] <- compute_metrics(modified_net)
  } else {
    stability_results[i, 2:4] <- NA  # If network collapses, assign NA
  }
}

# Adiciona a porcentagem de mudança da rede 
stability_results$DegreeChange <- ((stability_results$AvgDegree - full_metrics[1]) / full_metrics[1]) * 100
stability_results$PathLengthChange <- ((stability_results$AvgPathLength - full_metrics[2]) / full_metrics[2]) * 100
stability_results$ClusteringChange <- ((stability_results$AvgClustering - full_metrics[3]) / full_metrics[3]) * 100

#Formata a tabela para uma melhor leitura 
stability_results <- stability_results %>%
  mutate(across(c(AvgDegree, AvgPathLength, AvgClustering, DegreeChange, PathLengthChange, ClusteringChange),
                ~ round(., 2)))  # Valores com 2 casas decímais

# Mostra os resultados de estabilidade da rede após a remoção dos nós
print("Stability Analysis (Impact of Removing Each Node):")
print(stability_results)


