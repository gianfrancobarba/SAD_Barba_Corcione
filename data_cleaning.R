# Caricare il dataset
df <- read.csv("ACTIVITIESdata.csv", sep = ";")

# Visualizzare le prime righe del dataset per comprendere la struttura dei dati
head(df)

# Visualizzare i tipi di dati delle colonne
str(df)

# Contare i valori mancanti per ciascuna colonna
missing_values <- colSums(is.na(df))
print(missing_values) # Sono carattere, quindi 'None' è come se non ci fossero valori mancanti

# --- Osservazioni ---
# Molte colonne numeriche sono state lette come 'character' a causa della formattazione dei dati.
# È necessario convertire queste colonne nel formato numerico appropriato.

# Elenco delle colonne da convertire in numerico
cols_to_convert <- c("AltitudeAvg", "AltitudeMin", "AltitudeMax", 
                     "Duration", "HrAvg", "HrMin", "HrMax", 
                     "SpeedAvg", "SpeedMax", "CadenceAvg", "CadenceMax")

# Verificare il contenuto delle colonne prima della conversione per identificare valori non numerici
for (col in cols_to_convert) {
  print(paste("Colonna:", col))
  print(unique(df[[col]])[1:10])  # Mostrare i primi 10 valori unici per ciascuna colonna
}

# --- Osservazioni ---
# Dopo aver controllato i valori unici, notiamo che:
# - Alcune colonne contengono valori mancanti o formattati in modo non corretto.
# - Alcune colonne sono completamente vuote (es. HrMax, SpeedMax, CadenceAvg, CadenceMax).

# Rimuovere le colonne completamente vuote
cols_to_drop <- c("HrMax", "SpeedMax", "CadenceAvg", "CadenceMax")
df_cleaned <- df[, !(names(df) %in% cols_to_drop)]

# Convertire le colonne rimanenti al tipo numerico, sostituendo i valori non convertibili con NA
df_cleaned[cols_to_convert[!cols_to_convert %in% cols_to_drop]] <- lapply(
  df_cleaned[cols_to_convert[!cols_to_convert %in% cols_to_drop]], 
  function(x) as.numeric(as.character(x))
)

# Convertire StartTime e EndTime in POSIXct
df_cleaned$StartTime <- as.POSIXct(df_cleaned$StartTime, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
df_cleaned$EndTime <- as.POSIXct(df_cleaned$EndTime, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")

# Verificare i tipi di dati dopo la conversione
str(df_cleaned)

# Calcolare il numero totale di colonne nel dataset
num_cols <- ncol(df_cleaned)

# Definire una soglia per la rimozione: 30% di valori nulli
# Osservazione: Se una riga ha più del 30% di valori nulli, la eliminiamo
threshold <- 0.3 * num_cols

# Identificare le righe con più del 30% di valori nulli
rows_to_drop <- apply(df_cleaned, 1, function(row) sum(is.na(row)) > threshold)

# Contare quante righe saranno eliminate
num_rows_to_drop <- sum(rows_to_drop)
print(paste("Numero di righe da eliminare:", num_rows_to_drop))

# Rimuovere le righe identificate
df_cleaned <- df_cleaned[!rows_to_drop, ]

# Verificare il numero di righe rimanenti
remaining_rows <- nrow(df_cleaned)
print(paste("Numero di righe rimanenti:", remaining_rows))

# Visualizzare le prime righe del dataset pulito
head(df_cleaned)

# Contare i valori mancanti dopo la conversione ed eliminazione righe
missing_values_after_conversion <- colSums(is.na(df_cleaned))
print(missing_values_after_conversion)


# --- Osservazione ---
# Dopo le conversioni ed eliminazioni, alcune colonne presentano valori nulli (NA) dovuti a conversioni fallite.
# Procederemo con l'imputazione dei valori nulli utilizzando la mediana specifica per ciascun tipo di attività.
# Quindi segmenteremo il dataset per tipo di attività e imputeremo i valori nulli con la mediana di quella 
# specifica attività.

# Caricare la libreria dplyr per la manipolazione dei dati
# togliere il commento alla prima esecuzione
#install.packages("dplyr")
library(dplyr)

# Imputare i valori nulli con la mediana per ciascun tipo di attività
df_cleaned <- df_cleaned %>%
  group_by(ActivityType) %>%  # Raggruppa per tipo di attività
  mutate(
    AltitudeAvg = ifelse(is.na(AltitudeAvg), median(AltitudeAvg, na.rm = TRUE), AltitudeAvg),
    AltitudeMin = ifelse(is.na(AltitudeMin), median(AltitudeMin, na.rm = TRUE), AltitudeMin),
    AltitudeMax = ifelse(is.na(AltitudeMax), median(AltitudeMax, na.rm = TRUE), AltitudeMax),
    Duration    = ifelse(is.na(Duration), median(Duration, na.rm = TRUE), Duration),
    HrAvg       = ifelse(is.na(HrAvg), median(HrAvg, na.rm = TRUE), HrAvg),
    HrMin       = ifelse(is.na(HrMin), median(HrMin, na.rm = TRUE), HrMin),
    SpeedAvg    = ifelse(is.na(SpeedAvg), median(SpeedAvg, na.rm = TRUE), SpeedAvg)
  ) %>%
  ungroup()  # Termina il raggruppamento

# Verificare se ci sono ancora valori nulli dopo l'imputazione
colSums(is.na(df_cleaned))

# --- Osservazione ---
# Dopo l'imputazione, non ci sono più valori nulli nelle colonne selezionate.
# Ora il dataset è pulito e pronto per ulteriori analisi.

# Salvare il dataset pulito su un nuovo file CSV
write.csv(df_cleaned, "ACTIVITIESdata_cleaned.csv", row.names = FALSE)

# Visualizzare le prime righe del dataset pulito per confermare le modifiche
head(df_cleaned)


# ANALISI DEGLI OUTLIER
# -----------------------------------------------------------
# 1. Caricare i Pacchetti Necessari
# -----------------------------------------------------------

if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")

# Caricare le librerie
library(ggplot2)
library(dplyr)
library(tidyr)

# -----------------------------------------------------------
# 2. Caricare il Dataset Pulito
# -----------------------------------------------------------

df <- read.csv("ACTIVITIESdata_cleaned.csv")

# Visualizzare le prime righe del dataset
head(df)

# -----------------------------------------------------------
# 3. Selezionare le Feature Rilevanti
# -----------------------------------------------------------

selected_features <- df %>% select(SpeedAvg, Calories, HrAvg, Duration)

# -----------------------------------------------------------
# 4. Identificare Outlier Utilizzando lo Z-Score per Ogni Tipo di Attività
# -----------------------------------------------------------

# Funzione per calcolare lo Z-Score e identificare outlier
identify_outliers_zscore <- function(data, threshold = 3) {
  outlier_indices <- c()
  for (activity in unique(data$ActivityType)) {
    # Filtrare i dati per tipo di attività
    activity_data <- data %>% filter(ActivityType == activity) %>% select(SpeedAvg, Calories, HrAvg, Duration)
    # Calcolare lo Z-Score per le feature selezionate
    z_scores <- scale(activity_data)
    # Identificare gli outlier con Z-Score superiore alla soglia
    outliers <- apply(abs(z_scores), 1, function(x) any(x > threshold))
    # Aggiungere gli indici degli outlier
    outlier_indices <- c(outlier_indices, which(outliers) + min(which(data$ActivityType == activity)) - 1)
  }
  return(outlier_indices)
}

# Identificare gli outlier con una soglia di 3
outlier_indices <- identify_outliers_zscore(df, threshold = 3)

# Estrarre i punti identificati come outlier
outlier_points <- df[outlier_indices, ]

# Visualizzare i punti identificati come outlier
print("Righe del dataset identificate come outlier:")
print(outlier_points)

# -----------------------------------------------------------
# 5. Visualizzare i Boxplot delle Feature Selezionate per Tipo di Attività
# -----------------------------------------------------------

# Funzione per creare un boxplot per ciascuna feature
plot_boxplots <- function(data, features) {
  for (feature in features) {
    p <- ggplot(data, aes(x = ActivityType, y = .data[[feature]], fill = ActivityType)) +
      geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 2) +
      labs(title = paste("Boxplot di", feature, "per Tipo di Attività"),
           x = "Tipo di Attività",
           y = feature) +
      theme_minimal()
    print(p)
  }
}

# Creare boxplot per ciascuna feature selezionata
plot_boxplots(df, c("SpeedAvg", "Calories", "HrAvg", "Duration"))

# -----------------------------------------------------------
# 6. Contare gli Outlier per Ogni Tipo di Attività
# -----------------------------------------------------------

outlier_counts <- outlier_points %>% count(ActivityType)
print("Numero di outlier per ciascun tipo di attività:")
print(outlier_counts)

# -----------------------------------------------------------
# 7. Visualizzare gli Outlier in un Grafico a Barre
# -----------------------------------------------------------

ggplot(outlier_counts, aes(x = ActivityType, y = n, fill = ActivityType)) +
  geom_bar(stat = "identity") +
  labs(title = "Numero di Outlier per Tipo di Attività",
       x = "Tipo di Attività",
       y = "Numero di Outlier") +
  theme_minimal()

# -----------------------------------------------------------
# 8. Identificare le Feature che Hanno Causato l'Outlier con Valori Medi e Outlier
# -----------------------------------------------------------

# Funzione per identificare gli outlier e aggiungere i valori medi e outlier
identify_outlier_features <- function(data, threshold = 3) {
  outlier_details <- data.frame(Row = integer(), 
                                ActivityType = character(), 
                                Feature = character(), 
                                Mean_Value = numeric(), 
                                Outlier_Value = numeric(), 
                                stringsAsFactors = FALSE)
  
  for (activity in unique(data$ActivityType)) {
    # Filtrare i dati per tipo di attività
    activity_data <- data %>% filter(ActivityType == activity) %>% select(SpeedAvg, Calories, HrAvg, Duration)
    # Calcolare lo Z-Score per le feature selezionate
    z_scores <- scale(activity_data)
    # Identificare gli outlier per ciascuna riga
    for (i in 1:nrow(activity_data)) {
      outlier_features <- which(abs(z_scores[i, ]) > threshold)
      if (length(outlier_features) > 0) {
        for (feature_index in outlier_features) {
          feature_name <- names(activity_data)[feature_index]
          mean_value <- mean(activity_data[[feature_name]], na.rm = TRUE)
          outlier_value <- activity_data[[feature_name]][i]
          
          outlier_details <- rbind(outlier_details, data.frame(
            Row = i + min(which(data$ActivityType == activity)) - 1,
            ActivityType = activity,
            Feature = feature_name,
            Mean_Value = round(mean_value, 2),
            Outlier_Value = round(outlier_value, 2)
          ))
        }
      }
    }
  }
  return(outlier_details)
}

# Identificare le feature che hanno causato l'outlier con i valori medi e outlier
outlier_details <- identify_outlier_features(df, threshold = 3)

# Stampare i dettagli degli outlier come dataframe
print("Dettagli delle righe outlier con valori medi e outlier corrispondenti:")
print(outlier_details)

# Visualizzare i dettagli degli outlier in una tabella ordinata
library(knitr)
kable(outlier_details, caption = "Dettagli degli Outlier con Valori Medi e Outlier")

# -----------------------------------------------------------
# Ora provo a creare un nuovo csv con rimuovendo queste righe "critiche" e ri-eseguo clustering

# Creare un nuovo dataframe rimuovendo le righe identificate come outlier
df_no_outliers <- df[-outlier_details$Row, ]

# Visualizzare le prime righe del nuovo dataframe senza outlier
head(df_no_outliers)

# Salvare il nuovo dataframe in un file CSV
write.csv(df_no_outliers, "ACTIVITIESdata_no_outliers.csv", row.names = FALSE)

# Confermare la creazione del file
print("Nuovo file CSV 'ACTIVITIESdata_no_outliers.csv' creato con successo senza outlier.")

