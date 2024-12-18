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

