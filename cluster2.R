# -----------------------------------------------------------
# 1. Installare e Caricare i Pacchetti Necessari
# -----------------------------------------------------------

if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(cluster)) install.packages("cluster")
if (!require(factoextra)) install.packages("factoextra")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(gridExtra)) install.packages("gridExtra")

# Caricare le librerie
library(ggplot2)
library(dplyr)
library(cluster)       # Per silhouette score
library(factoextra)    # Per visualizzazioni del clustering
library(tidyverse)     # Per manipolazione dei dati
library(gridExtra)     # Per visualizzare più grafici insieme

# -----------------------------------------------------------
# 2. Caricare i Dataset
# -----------------------------------------------------------

# Dataset normalizzato
df_norm <- read.csv("ACTIVITIESdata_normalized.csv")

# Dataset pulito ma non normalizzato
df_clean <- read.csv("ACTIVITIESdata_cleaned.csv")

# Visualizzare le prime righe dei dataset
head(df_norm)
head(df_clean)

# -----------------------------------------------------------
# 3. Selezionare e Preparare le Feature per Entrambi i Dataset
# -----------------------------------------------------------

# Selezionare le colonne rilevanti nei due dataset
df_norm_selected <- df_norm %>%
  select(SpeedAvg, Calories, HrAvg, Duration)

df_clean_selected <- df_clean %>%
  select(SpeedAvg, Calories, HrAvg, Duration)

# Applicare la trasformazione logaritmica al dataset pulito
df_clean_log <- df_clean_selected %>% mutate_all(~ log(. + 1))

# Standardizzare le feature per il dataset pulito con trasformazione logaritmica
df_clean_log_scaled <- scale(df_clean_log)

# Le feature nel dataset normalizzato sono già normalizzate
df_norm_scaled <- df_norm_selected

# -----------------------------------------------------------
# 4. Clustering K-Means con 2 Cluster su Entrambi i Dataset
# -----------------------------------------------------------

# K-Means sul dataset normalizzato
set.seed(42)
kmeans_norm <- kmeans(df_norm_scaled, centers = 2, nstart = 10)

# K-Means sul dataset pulito con trasformazione logaritmica
set.seed(42)
kmeans_clean_log <- kmeans(df_clean_log_scaled, centers = 2, nstart = 10)

# -----------------------------------------------------------
# 5. Aggiungere i Risultati dei Cluster ai Dataframe Originali
# -----------------------------------------------------------

df_norm$KMeans_Cluster <- as.factor(kmeans_norm$cluster)
df_clean$KMeans_Cluster <- as.factor(kmeans_clean_log$cluster)

# -----------------------------------------------------------
# 6. Valutazione con Silhouette Score
# -----------------------------------------------------------

# Silhouette Score per il dataset normalizzato
silhouette_norm <- silhouette(kmeans_norm$cluster, dist(df_norm_scaled))
mean_silhouette_norm <- mean(silhouette_norm[, 3])
cat("Silhouette Score per Dataset Normalizzato:", mean_silhouette_norm, "\n")

# Silhouette Score per il dataset pulito con trasformazione logaritmica
silhouette_clean_log <- silhouette(kmeans_clean_log$cluster, dist(df_clean_log_scaled))
mean_silhouette_clean_log <- mean(silhouette_clean_log[, 3])
cat("Silhouette Score per Dataset Pulito con Log Transformation:", mean_silhouette_clean_log, "\n")

# -----------------------------------------------------------
# 7. Visualizzazione dei Risultati del Clustering
# -----------------------------------------------------------

# K-Means per dataset normalizzato
p1 <- fviz_cluster(kmeans_norm, data = df_norm_scaled) +
  labs(title = "K-Means su Dataset Normalizzato")

# K-Means per dataset pulito con trasformazione logaritmica
p2 <- fviz_cluster(kmeans_clean_log, data = df_clean_log_scaled) +
  labs(title = "K-Means su Dataset Pulito con Log Transformation")

# Visualizzare i grafici insieme
grid.arrange(p1, p2, nrow = 2)

# -----------------------------------------------------------
# 8. Confronto dei Risultati con Tabella di Contingenza
# -----------------------------------------------------------

# Tabella di contingenza per dataset normalizzato
cat("Tabella di Contingenza per Dataset Normalizzato:\n")
print(table(df_norm$ActivityType, df_norm$KMeans_Cluster))

# Tabella di contingenza per dataset pulito con trasformazione logaritmica
cat("Tabella di Contingenza per Dataset Pulito con Log Transformation:\n")
print(table(df_clean$ActivityType, df_clean$KMeans_Cluster))

# -----------------------------------------------------------
# 9. Confronto dei Silhouette Score
# -----------------------------------------------------------

# Creare un dataframe per i Silhouette Score
results <- data.frame(
  Dataset = c("Normalizzato", "Pulito con Log Transformation"),
  Silhouette_Score = c(mean_silhouette_norm, mean_silhouette_clean_log)
)

# Visualizzare i risultati in un grafico
ggplot(results, aes(x = Dataset, y = Silhouette_Score, fill = Dataset)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Confronto dei Silhouette Score tra i Due Dataset",
       x = "Dataset",
       y = "Silhouette Score")
