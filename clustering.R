# Installare i pacchetti se necessario
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(cluster)) install.packages("cluster")
if(!require(factoextra)) install.packages("factoextra")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(gridExtra)) install.packages("gridExtra")

# Caricare le librerie
library(ggplot2)
library(dplyr)
library(cluster)       # Per silhouette score
library(factoextra)    # Per visualizzazioni del clustering
library(tidyverse)     # Per manipolazione dati
library(gridExtra)     # Per visualizzare più grafici insieme

# Caricare il dataset
df <- read.csv("ACTIVITIESdata_normalized.csv")
# df <- read.csv("ACTIVITIESdata_no_outliers.csv") # PROVA SENZA "OUTLIERS"
# df <- read.csv("ACTIVITIESdata_cleaned.csv")

# Visualizzare le prime righe del dataset
head(df)

# Rimuovere le colonne 'ActivityType', 'StartTime' e 'EndTime'
df_cleaned <- df %>%
  select(-ActivityType, -StartTime, -EndTime)

# Visualizzare le prime righe del dataset pulito
head(df_cleaned)

# Standardizzare le variabili numeriche
df_scaled <- scale(df_cleaned)

# Controllare il risultato della standardizzazione
head(df_scaled)

# Metodo del Gomito per determinare il numero ottimale di cluster
fviz_nbclust(df_scaled, kmeans, method = "wss") + 
  labs(title = "Metodo del Gomito per K-Means")

# Calcolo del Silhouette Score per diversi valori di k
fviz_nbclust(df_scaled, kmeans, method = "silhouette") + 
  labs(title = "Silhouette Score per K-Means")

# Calcolare il clustering gerarchico con metodo Ward
hc <- hclust(dist(df_scaled), method = "ward.D2")

# Visualizzare il dendrogramma
plot(hc, main = "Dendrogramma del Clustering Gerarchico", 
     xlab = "", sub = "", cex = 0.7)
abline(h = 10, col = "red", lwd = 2)  # Aggiungere una linea per suggerire il taglio

# Tagliare il dendrogramma per ottenere 3 cluster
hc_clusters <- cutree(hc, k = 3)

# Aggiungere i cluster ai dati originali
df$HC_Cluster <- as.factor(hc_clusters)

# Applicare K-Means con k = 3 e nstart = 10 (numero di inizializzazioni)
set.seed(42)
kmeans_result <- kmeans(df_scaled, centers = 3, nstart = 10)

# Aggiungere i cluster ai dati originali
df$KMeans_Cluster <- as.factor(kmeans_result$cluster)


# Valutazione risultati

# Clustering Gerarchico
fviz_cluster(list(data = df_scaled, cluster = hc_clusters)) +
  labs(title = "Clustering Gerarchico")

# K-Means
fviz_cluster(kmeans_result, data = df_scaled) +
  labs(title = "Clustering K-Means")


# CONFRONTO

# Visualizzare una tabella di contingenza per confrontare i cluster con 
# le etichette originali
table(df$ActivityType, df$HC_Cluster)
table(df$ActivityType, df$KMeans_Cluster)


# Silhouette Score per il clustering gerarchico
silhouette_hc <- silhouette(hc_clusters, dist(df_scaled))
mean_silhouette_hc <- mean(silhouette_hc[, 3])
cat("Silhouette Score Medio per Clustering Gerarchico:", mean_silhouette_hc, "\n")

# Silhouette Score per K-Means
silhouette_kmeans <- silhouette(kmeans_result$cluster, dist(df_scaled))
mean_silhouette_kmeans <- mean(silhouette_kmeans[, 3])
cat("Silhouette Score Medio per K-Means:", mean_silhouette_kmeans, "\n")

# Visualizzare i plot delle silhouette
fviz_silhouette(silhouette_hc) + labs(title = "Silhouette per Clustering Gerarchico")
fviz_silhouette(silhouette_kmeans) + labs(title = "Silhouette per K-Means")


# ---------------------------------------------------------------------
# Ora utilizziamo Min-Max Scaling e nuovi algoritmi di clustering
# per valutare se possiamo migliorare i risultati precedenti.
# ---------------------------------------------------------------------

# Installare i pacchetti se necessario
if(!require(dbscan)) install.packages("dbscan")
if(!require(mclust)) install.packages("mclust")

# Caricare le librerie aggiuntive per i nuovi metodi di clustering
library(dbscan)    # Per DBSCAN
library(mclust)    # Per Gaussian Mixture Models (GMM)

# -----------------------------------------------------------
# 1. Min-Max Scaling per normalizzare i dati tra 0 e 1
# -----------------------------------------------------------

# Applicare Min-Max Scaling
df_minmax <- as.data.frame(apply(df_cleaned, 2, function(x) (x - min(x)) / (max(x) - min(x))))

# Controllare il risultato della normalizzazione
head(df_minmax)

# -----------------------------------------------------------
# 2. Applicare nuovi algoritmi di clustering
# -----------------------------------------------------------

# -------------------------------
# DBSCAN (Density-Based Clustering)
# -------------------------------

# Impostare i parametri di DBSCAN
# eps: distanza massima tra i punti per formarli in cluster
# minPts: numero minimo di punti per formare un cluster
dbscan_result <- dbscan(df_minmax, eps = 0.3, minPts = 4)

# Aggiungere i cluster DBSCAN al dataframe originale
df$DBSCAN_Cluster <- as.factor(dbscan_result$cluster)

# Verificare i risultati di DBSCAN
print("Risultati DBSCAN:")
print(table(df$DBSCAN_Cluster))

# -------------------------------
# Gaussian Mixture Model (GMM)
# -------------------------------

# Applicare GMM con 3 componenti (cluster)
gmm_result <- Mclust(df_minmax, G = 3)

# Aggiungere i cluster GMM al dataframe originale
df$GMM_Cluster <- as.factor(gmm_result$classification)


# Verificare i risultati di GMM
print("Risultati GMM:")
print(table(df$GMM_Cluster))

# -----------------------------------------------------------
# 3. Valutazione dei Risultati con Silhouette Score
# -----------------------------------------------------------

# Funzione per calcolare il Silhouette Score in modo sicuro
calculate_silhouette <- function(data, clusters) {
  if (length(unique(clusters)) > 1) {
    mean(silhouette(clusters, dist(data))[, 3])
  } else {
    NA
  }
}

# Silhouette Score per DBSCAN
silhouette_dbscan <- calculate_silhouette(df_minmax, dbscan_result$cluster)
cat("Silhouette Score per DBSCAN:", silhouette_dbscan, "\n")

# Silhouette Score per GMM
silhouette_gmm <- calculate_silhouette(df_minmax, gmm_result$classification)
cat("Silhouette Score per GMM:", silhouette_gmm, "\n")

# -----------------------------------------------------------
# 4. Visualizzazione dei Risultati dei Nuovi Clustering
# -----------------------------------------------------------

# Visualizzare i risultati dei cluster con fviz_cluster

# DBSCAN
fviz_cluster(list(data = df_minmax, cluster = dbscan_result$cluster)) +
  labs(title = "Clustering DBSCAN")

# GMM
fviz_cluster(list(data = df_minmax, cluster = gmm_result$classification)) +
  labs(title = "Clustering GMM")

# -----------------------------------------------------------
# 5. Confronto delle Metriche di Valutazione
# -----------------------------------------------------------

# Creare un dataframe con i risultati dei Silhouette Score

results <- data.frame(
  Method = c("DBSCAN", "GMM"),
  Silhouette_Score = c(silhouette_dbscan, silhouette_gmm)
)

# Stampare i risultati
print(results)

# Visualizzare i risultati in un grafico
ggplot(results, aes(x = Method, y = Silhouette_Score, fill = Method)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Confronto dei Silhouette Score tra i Metodi di Clustering",
       x = "Metodo di Clustering",
       y = "Silhouette Score")

# -----------------------------------------------------------
# 6. Selezione delle Feature in Base alla Matrice di Correlazione
# -----------------------------------------------------------

# Selezionare feature meno correlate e più rappresentative
# Dalla matrice di correlazione scegliamo: SpeedAvg, Calories, HrAvg, Duration
selected_features <- df_cleaned %>% select(SpeedAvg, Calories, HrAvg, Duration)

# Standardizzare le feature selezionate
selected_scaled <- scale(selected_features)

# Visualizzare le prime righe delle feature selezionate standardizzate
head(selected_scaled)

# -----------------------------------------------------------
# 7. Clustering con le Feature Selezionate
# -----------------------------------------------------------

# K-Means con 3 cluster
set.seed(42)
kmeans_selected <- kmeans(selected_scaled, centers = 3, nstart = 10)
df$KMeans_Selected <- as.factor(kmeans_selected$cluster)

# Clustering Gerarchico con metodo Ward
hc_selected <- hclust(dist(selected_scaled), method = "ward.D2")
hc_selected_clusters <- cutree(hc_selected, k = 3)
df$HC_Selected <- as.factor(hc_selected_clusters)

# DBSCAN con le feature selezionate
dbscan_selected <- dbscan(selected_scaled, eps = 0.3, minPts = 3)
df$DBSCAN_Selected <- as.factor(dbscan_selected$cluster)

# -----------------------------------------------------------
# 8. Valutazione dei Risultati con Silhouette Score e Altre Metriche
# -----------------------------------------------------------

# Funzione per calcolare il Silhouette Score in modo sicuro
calculate_silhouette <- function(data, clusters) {
  if (length(unique(clusters)) > 1) {
    mean(silhouette(clusters, dist(data))[, 3])
  } else {
    NA
  }
}

# Silhouette Score per K-Means con feature selezionate
silhouette_kmeans_selected <- silhouette(kmeans_selected$cluster, dist(selected_scaled))
mean_silhouette_kmeans_selected <- mean(silhouette_kmeans_selected[, 3])
cat("Silhouette Score per K-Means con Feature Selezionate:", mean_silhouette_kmeans_selected, "\n")

# Silhouette Score per Clustering Gerarchico con feature selezionate
silhouette_hc_selected <- silhouette(hc_selected_clusters, dist(selected_scaled))
mean_silhouette_hc_selected <- mean(silhouette_hc_selected[, 3])
cat("Silhouette Score per Clustering Gerarchico con Feature Selezionate:", mean_silhouette_hc_selected, "\n")

# Silhouette Score per DBSCAN con feature selezionate (escludere outlier)
valid_clusters <- dbscan_selected$cluster != 0
if (any(valid_clusters)) {
  silhouette_dbscan_selected <- silhouette(dbscan_selected$cluster[valid_clusters], dist(selected_scaled[valid_clusters, ]))
  mean_silhouette_dbscan_selected <- mean(silhouette_dbscan_selected[, 3])
} else {
  mean_silhouette_dbscan_selected <- NA
}
cat("Silhouette Score per DBSCAN con Feature Selezionate:", mean_silhouette_dbscan_selected, "\n")

# -----------------------------------------------------------
# 9. Visualizzazione e Confronto dei Risultati
# -----------------------------------------------------------

# K-Means
p1 <- fviz_cluster(kmeans_selected, data = selected_scaled) + 
  labs(title = "K-Means con Feature Selezionate")

# Clustering Gerarchico
p2 <- fviz_cluster(list(data = selected_scaled, cluster = hc_selected_clusters)) + 
  labs(title = "Clustering Gerarchico con Feature Selezionate")

# DBSCAN
p3 <- fviz_cluster(list(data = selected_scaled, cluster = dbscan_selected$cluster), geom = "point") + 
  labs(title = "DBSCAN con Feature Selezionate")

# Visualizzare i grafici insieme
grid.arrange(p1, p2, p3, nrow = 2)

# -----------------------------------------------------------
# 10. Confronto dei Silhouette Score
# -----------------------------------------------------------

# Creare un dataframe con i risultati dei Silhouette Score
results_selected <- data.frame(
  Method = c("K-Means", "Hierarchical", "DBSCAN"),
  Silhouette_Score = c(mean_silhouette_kmeans_selected, 
                       mean_silhouette_hc_selected, 
                       mean_silhouette_dbscan_selected)
)

# Visualizzare i risultati in un grafico
ggplot(results_selected, aes(x = Method, y = Silhouette_Score, fill = Method)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Confronto dei Silhouette Score con Feature Selezionate",
       x = "Metodo di Clustering",
       y = "Silhouette Score")

# -----------------------------------------------------------
# 11. Tabella di Contingenza per Verificare la Distribuzione dei Cluster
# -----------------------------------------------------------

# Tabella di contingenza per K-Means
cat("Tabella di Contingenza per K-Means:\n")
print(table(df$ActivityType, df$KMeans_Selected))

# Tabella di contingenza per Clustering Gerarchico
cat("Tabella di Contingenza per Clustering Gerarchico:\n")
print(table(df$ActivityType, df$HC_Selected))

# Tabella di contingenza per DBSCAN (escludendo outlier)
if (any(valid_clusters)) {
  cat("Tabella di Contingenza per DBSCAN:\n")
  print(table(df$ActivityType[valid_clusters], df$DBSCAN_Selected[valid_clusters]))
} else {
  cat("Nessun cluster valido per DBSCAN.\n")
}


# -----------------------------------------------------------
#               NUOVO STUDIO SOLO 2 CLUSTER
# -----------------------------------------------------------
# 1. Filtrare i dati per includere solo Biking e Running
# -----------------------------------------------------------

df_filtered <- df %>% filter(ActivityType %in% c("Biking", "Running"))

# Rimuovere colonne non necessarie
df_cleaned_filtered <- df_filtered %>% select(-ActivityType, -StartTime, -EndTime)

# -----------------------------------------------------------
# 2. Selezionare le Feature più Rappresentative
# -----------------------------------------------------------

selected_features <- df_cleaned_filtered %>% select(SpeedAvg, Calories, HrAvg, Duration)

# Standardizzare le feature selezionate
selected_scaled <- scale(selected_features)

# Visualizzare le prime righe delle feature selezionate e standardizzate
head(selected_scaled)

# -----------------------------------------------------------
# 3. Clustering con 2 Cluster
# -----------------------------------------------------------

# K-Means con 2 cluster
set.seed(42)
kmeans_2 <- kmeans(selected_scaled, centers = 2, nstart = 10)
df_filtered$KMeans_2 <- as.factor(kmeans_2$cluster)

# Clustering Gerarchico con metodo Ward e 2 cluster
hc_2 <- hclust(dist(selected_scaled), method = "ward.D2")
hc_clusters_2 <- cutree(hc_2, k = 2)
df_filtered$HC_2 <- as.factor(hc_clusters_2)

# DBSCAN con 2 cluster (modificare i parametri per ottenere 2 cluster)
dbscan_2 <- dbscan(selected_scaled, eps = 0.3, minPts = 3)
df_filtered$DBSCAN_2 <- as.factor(dbscan_2$cluster)

# -----------------------------------------------------------
# 4. Valutazione dei Risultati con Silhouette Score
# -----------------------------------------------------------

# Silhouette Score per K-Means con 2 cluster
silhouette_kmeans_2 <- silhouette(kmeans_2$cluster, dist(selected_scaled))
mean_silhouette_kmeans_2 <- mean(silhouette_kmeans_2[, 3])
cat("Silhouette Score per K-Means con 2 Cluster:", mean_silhouette_kmeans_2, "\n")

# Silhouette Score per Clustering Gerarchico con 2 cluster
silhouette_hc_2 <- silhouette(hc_clusters_2, dist(selected_scaled))
mean_silhouette_hc_2 <- mean(silhouette_hc_2[, 3])
cat("Silhouette Score per Clustering Gerarchico con 2 Cluster:", mean_silhouette_hc_2, "\n")

# Silhouette Score per DBSCAN con 2 cluster (escludere outlier)
valid_clusters <- dbscan_2$cluster != 0
if (any(valid_clusters)) {
  silhouette_dbscan_2 <- silhouette(dbscan_2$cluster[valid_clusters], dist(selected_scaled[valid_clusters, ]))
  mean_silhouette_dbscan_2 <- mean(silhouette_dbscan_2[, 3])
} else {
  mean_silhouette_dbscan_2 <- NA
}
cat("Silhouette Score per DBSCAN con 2 Cluster:", mean_silhouette_dbscan_2, "\n")

# -----------------------------------------------------------
# 5. Visualizzazione dei Risultati dei Clustering
# -----------------------------------------------------------

# K-Means
p1 <- fviz_cluster(kmeans_2, data = selected_scaled) + 
  labs(title = "K-Means con 2 Cluster")

# Clustering Gerarchico
p2 <- fviz_cluster(list(data = selected_scaled, cluster = hc_clusters_2)) + 
  labs(title = "Clustering Gerarchico con 2 Cluster")

# DBSCAN
p3 <- fviz_cluster(list(data = selected_scaled, cluster = dbscan_2$cluster), geom = "point") + 
  labs(title = "DBSCAN con 2 Cluster")

# Visualizzare i grafici insieme
grid.arrange(p1, p2, p3, nrow = 2)

# -----------------------------------------------------------
# 6. Tabella di Contingenza per Verificare i Cluster
# -----------------------------------------------------------

# Tabella di contingenza per K-Means
cat("Tabella di Contingenza per K-Means con 2 Cluster:\n")
print(table(df_filtered$ActivityType, df_filtered$KMeans_2))

# Tabella di contingenza per Clustering Gerarchico
cat("Tabella di Contingenza per Clustering Gerarchico con 2 Cluster:\n")
print(table(df_filtered$ActivityType, df_filtered$HC_2))

# Tabella di contingenza per DBSCAN (escludendo outlier)
if (any(valid_clusters)) {
  cat("Tabella di Contingenza per DBSCAN con 2 Cluster:\n")
  print(table(df_filtered$ActivityType[valid_clusters], df_filtered$DBSCAN_2[valid_clusters]))
} else {
  cat("Nessun cluster valido per DBSCAN.\n")
}

