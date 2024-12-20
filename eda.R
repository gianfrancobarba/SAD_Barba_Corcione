# Caricare il dataset pulito dal file CSV
df <- read.csv("ACTIVITIESdata_cleaned.csv")

# Visualizzare le prime righe del dataset per avere una panoramica dei dati
head(df)

# Verificare le dimensioni del dataset (numero di righe e colonne)
dim(df)

# Visualizzare i nomi delle colonne del dataset
names(df)

# Verificare i tipi di dati delle colonne
str(df)
# --- Osservazione ---
# È importante verificare che le variabili siano nel formato corretto (es. numeriche, fattori, date).

# Contare i valori mancanti per ciascuna colonna
# Controlliamo se ci sono ancora valori mancanti nel dataset pulito.
colSums(is.na(df))

# Verificare se ci sono righe duplicate
sum(duplicated(df))
# --- Osservazione ---
# Se ci sono righe duplicate, possiamo valutarne la rimozione.

# Calcolare le statistiche descrittive per le variabili numeriche
summary(df)
# --- Osservazione ---
# Le statistiche descrittive includono minimi, massimi, mediana, media e quartili per ciascuna variabile numerica.
# Questo aiuta a comprendere la distribuzione dei dati e a individuare possibili outlier.

# Visualizzare il conteggio delle attività per tipo (Biking, Running, Other)
table(df$ActivityType)
# --- Osservazione ---
# Questo ci permette di vedere la distribuzione delle diverse attività nel dataset.


# Controllo coerenza della colonna Durata

# Calcolare la durata effettiva in secondi
df$CalculatedDuration <- as.numeric(difftime(df$EndTime, df$StartTime, units = "secs"))

# Confrontare la durata calcolata con la colonna Duration
df$DurationDifference <- df$CalculatedDuration - df$Duration

# Verificare se ci sono discrepanze
duration_issues <- df[df$DurationDifference != 0, ]

# Visualizzare le righe con discrepanze
print(duration_issues)

# Conteggio delle discrepanze
cat("Numero di discrepanze nella durata:", nrow(duration_issues), "\n")
# --- Osservazione ---
# Non ci sono discrepanze nella durata tra la colonna Duration e la durata calcolata.


# Verifica coerenza della distanza percorsa in relazione alla velocità e tempo

# Calcolare la velocità media teorica (in km/h) usando distanza e durata effettiva
df$CalculatedSpeed <- (df$Distance / df$CalculatedDuration) * 3.6  # Conversione da m/s a km/h

# Confrontare la velocità calcolata con la velocità media registrata
df$SpeedDifference <- df$CalculatedSpeed - df$SpeedAvg

# Verificare se ci sono discrepanze significative
speed_issues <- df[abs(df$SpeedDifference) > 1, ]  # Consideriamo differenze maggiori di 1 km/h

# Visualizzare le righe con discrepanze significative
print(speed_issues)

# Conteggio delle discrepanze
cat("Numero di discrepanze nella velocità:", nrow(speed_issues), "\n")
# --- Osservazione ---
# Non ci sono discrepanze significative tra la velocità calcolata e la velocità media registrata.


# Analisi delle Variabili Categoriali
if(!require(ggplot2))
  install.packages("ggplot2")
library(ggplot2)
# Grafico a barre per la distribuzione dei tipi di attività
ggplot(df, aes(x = ActivityType)) +
  geom_bar(fill = "orange") +
  labs(title = "Distribuzione dei Tipi di Attività", x = "Tipo di Attività", y = "Conteggio") +
  theme_minimal()


# Distribuzioni delle Variabili Numeriche

# Caricare le librerie necessarie
if (!require("ggplot2")) {
  install.packages("ggplot2")
}
library(ggplot2)

# Lista delle colonne numeriche da analizzare
numeric_cols <- c("AltitudeAvg", "AltitudeMin", "AltitudeMax", 
                  "Ascent", "Descent", "Distance", 
                  "Duration", "HrAvg", "HrMin", 
                  "Calories", "SpeedAvg")

# Creare istogrammi per ciascuna variabile numerica
for (col in numeric_cols) {
  print(
    ggplot(df, aes_string(x = col)) +
      geom_histogram(bins = 30, fill = "lightblue", color = "black") +
      labs(title = paste("Distribuzione di", col), x = col, y = "Frequenza") +
      theme_minimal()
  )
}

# Creare boxplot per ciascuna variabile numerica per identificare outlier
for (col in numeric_cols) {
  print(
    ggplot(df, aes_string(y = col)) +
      geom_boxplot(fill = "lightgreen") +
      labs(title = paste("Boxplot di", col), y = col) +
      theme_minimal()
  )
}


# Relazioni tra Variabili

# Calcolare la matrice di correlazione per le variabili numeriche
cor_matrix <- cor(df[, numeric_cols], use = "complete.obs")

# Visualizzare la matrice di correlazione come heatmap
if (!require("ggcorrplot")) {
  library(ggcorrplot)
}

ggcorrplot(cor_matrix, lab = TRUE, colors = c("red", "white", "blue"), 
           title = "Matrice di Correlazione", 
           tl.cex = 10)


# Confronto tra tipi di attività

# Boxplot della velocità media per tipo di attività
ggplot(df, aes(x = ActivityType, y = SpeedAvg, fill = ActivityType)) +
  geom_boxplot() +
  labs(title = "Confronto della Velocità Media per Tipo di Attività", x = "Tipo di Attività", y = "Velocità Media (km/h)") +
  theme_minimal()

# Boxplot delle calorie bruciate per tipo di attività
ggplot(df, aes(x = ActivityType, y = Calories, fill = ActivityType)) +
  geom_boxplot() +
  labs(title = "Confronto delle Calorie Bruciate per Tipo di Attività", x = "Tipo di Attività", y = "Calorie") +
  theme_minimal()


# Analisi Temporale

if (!require("TSstudio")) {
  install.packages("TSstudio")
}

# Caricare la libreria
library(TSstudio)

# Assicurarsi che StartTime sia in formato POSIXct
df$StartTime <- as.POSIXct(df$StartTime, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")

# Trovare la data della prima e dell'ultima attività
start_date <- min(df$StartTime, na.rm = TRUE)
end_date <- max(df$StartTime, na.rm = TRUE)

# Verificare il tipo dei dati
str(start_date)
str(end_date)

# Calcolare il numero totale di giorni coperti dai dati
days_covered <- as.numeric(difftime(end_date, start_date, units = "days"))

# Stampare i risultati corretti
cat("Periodo coperto dai dati:", format(start_date, "%Y-%m-%d"), "a", format(end_date, "%Y-%m-%d"), "\n")
cat("Numero totale di giorni coperti:", round(days_covered), "giorni\n")


# Caricare la libreria dplyr
if (!require("dplyr")) {
  install.packages("dplyr")
}
library(dplyr)

# Creare una nuova colonna con la data senza l'ora
df$Date <- as.Date(df$StartTime)

# Calcolare il numero di allenamenti per ciascun giorno e tipo di attività
df_freq <- df %>%
  group_by(Date, ActivityType) %>%
  summarise(Count = n(), .groups = 'drop')

# Visualizzare le prime righe del risultato
head(df_freq)

# Grafico a linee per il conteggio degli allenamenti nel tempo
ggplot(df_freq, aes(x = Date, y = Count, color = ActivityType)) +
  geom_line() +
  labs(title = "Conteggio degli Allenamenti nel Tempo", x = "Data", y = "Conteggio") +
  theme_minimal()
