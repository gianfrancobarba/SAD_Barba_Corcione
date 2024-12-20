# Caricare il dataset pulito dal file CSV
df <- read.csv("ACTIVITIESdata_cleaned.csv")

# funzione di distribuzione sul dataset 
if(!require(ggplot2)) 
  install.packages("ggplot2")
library(ggplot2)
# Calcolare la funzione di distribuzione cumulativa per la distanza percorsa
ggplot(df, aes(x = Distance)) +
  stat_ecdf(geom = "step", color = "blue") +
  labs(title = "Funzione di Distribuzione Cumulativa della Distanza Percorsa", x = "Distanza (km)", y = "Probabilità") +
  theme_minimal()

# Calcolare la funzione di distribuzione cumulativa per la durata
ggplot(df, aes(x = Duration)) +
  stat_ecdf(geom = "step", color = "green") +
  labs(title = "Funzione di Distribuzione Cumulativa della Durata", x = "Durata (s)", y = "Probabilità") +
  theme_minimal()

# Calcolare la funzione di distribuzione cumulativa per la velocità media
ggplot(df, aes(x = SpeedAvg)) +
  stat_ecdf(geom = "step", color = "red") +
  labs(title = "Funzione di Distribuzione Cumulativa della Velocità Media", x = "Velocità Media (km/h)", y = "Probabilità") +
  theme_minimal()

# calcolo della funzione di distribuzione cumulativa per le calorie bruciate
ggplot(df, aes(x = Calories)) +
  stat_ecdf(geom = "step", color = "purple") +
  labs(title = "Funzione di Distribuzione Cumulativa delle Calorie Bruciate", x = "Calorie", y = "Probabilità") +
  theme_minimal()

# calcolo della funzione di distribuzione cumulativa per l'altitudine
ggplot(df, aes(x = AltitudeAvg)) +
  stat_ecdf(geom = "step", color = "orange") +
  labs(title = "Funzione di Distribuzione Cumulativa dell'Altitudine Media", x = "Altitudine Media (m)", y = "Probabilità") +
  theme_minimal()

# calcolo della funzione di distribuzione cumulativa per la frequenza cardiaca media
ggplot(df, aes(x = HrAvg)) +
  stat_ecdf(geom = "step", color = "brown") +
  labs(title = "Funzione di Distribuzione Cumulativa della Frequenza Cardiaca Media", x = "Frequenza Cardiaca Media (bpm)", y = "Probabilità") +
  theme_minimal()


# slides 
#FDE_velocita <- ecdf(df$SpeedAvg)
#plot(FDE_velocita, main = "Funzione di Distribuzione Cumulativa della Velocità Media", xlab = "Velocità Media (km/h)", ylab = "Probabilità", col = "red", verticals = FALSE)

