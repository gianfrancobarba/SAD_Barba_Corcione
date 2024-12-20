# -----------------------------------------------------------
# 1. Caricare i Pacchetti Necessari
# -----------------------------------------------------------

if (!require(dplyr)) install.packages("dplyr")

# Caricare la libreria
library(dplyr)

# -----------------------------------------------------------
# 2. Caricare il Dataset Pulito
# -----------------------------------------------------------

df <- read.csv("ACTIVITIESdata_cleaned.csv")

# Visualizzare le prime righe del dataset originale
head(df)

# -----------------------------------------------------------
# 3. Filtrare le Attività con Durate Inferiori a 5 Minuti
# -----------------------------------------------------------

# Filtrare le attività con durata >= 300 secondi (5 minuti)
df_filtered <- df %>% filter(Duration >= 300)

# Visualizzare il numero di righe dopo il filtraggio
print(paste("Numero di righe dopo aver rimosso attività brevi:", nrow(df_filtered)))

# -----------------------------------------------------------
# 4. Sostituire le Feature con le Versioni Normalizzate per Durata
# -----------------------------------------------------------

# Sostituire le colonne originali con le versioni normalizzate
df_filtered <- df_filtered %>%
  mutate(
    Calories = Calories / (Duration / 60),
    SpeedAvg = SpeedAvg / (Duration / 60),
    HrAvg = HrAvg / (Duration / 60)
  )

# Visualizzare le prime righe del dataset modificato
head(df_filtered)

# -----------------------------------------------------------
# 5. Salvare il Nuovo Dataset Normalizzato
# -----------------------------------------------------------

write.csv(df_filtered, "ACTIVITIESdata_normalized.csv", row.names = FALSE)

# Conferma della creazione del file CSV
print("Nuovo file CSV 'ACTIVITIESdata_normalized.csv' creato con successo con le feature normalizzate per durata.")
