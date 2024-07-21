#Pakete laden
library(ggplot2)

# Test-Datensatz erstellen
# z. B. aus vorheriger vergleichbarer Untersuchung oder aus pre-Test bereits bekannt
set.seed(123)
konsumausgaben <- rnorm(100, mean = 30, sd = 15)  # 100 Besucher, Mittelwert = 50, Standardabweichung = 25
# Korrektur, falls Werte negativ (dann Ausgabe = 5)

# Werte runden
konsumausgaben <- round(konsumausgaben, 2)
konsumausgaben[konsumausgaben < 0] <- 5

min(konsumausgaben)
max(konsumausgaben)

# Visualisierung des Histogramms mit ggplot2

plot_1 <- ggplot(data.frame(konsumausgaben), aes(x = konsumausgaben)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = rgb(46/255, 69/255, 82/255), color = "white") +
  scale_x_continuous(breaks = seq(0, 70, by = 5), limits = c(0, 70), expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Histogramm der Konsumausgaben (Pilot- oder Vorstudie)", x = "Konsumausgaben (€)", y = "Häufigkeit") +
  coord_cartesian(xlim = c(0, 70)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10),
    axis.title=element_text(size=10)
  )


# Plot speichern für Präsentation
ggsave("konsumausgaben_histogram.png", plot = plot_1, width = 11, height = 7, units = "cm", bg = "transparent")

# Berechnung der Stichprobenvarianz und des Konfidenzintervalls
stichprobenmittelwert <- mean(konsumausgaben)
round(stichprobenmittelwert,2)
stichprobenvarianz <- var(konsumausgaben)
round(stichprobenvarianz,2)
stichproben_sd <- sd(konsumausgaben)
round(stichproben_sd)
n <- length(konsumausgaben)
alpha <- 0.05
z <- qnorm(1 - alpha/2)


# Bestimmung des notwendigen Stichprobenumfangs
gewünschte_genauigkeit <- 1  # Genauigkeit von ±2 Euro
n_notwendig <- (z^2 * stichproben_sd^2) / (gewünschte_genauigkeit^2)

n_notwendig



# Visualisierung der notwendigen Stichprobengröße in Abhängigkeit von der Genauigkeit e
genauigkeiten <- seq(1, 4, by = 0.1)
n_notwendig_e <- sapply(genauigkeiten, function(e) {
  (z^2 * stichproben_sd^2) / (e^2)
})

genauigkeit_df <- data.frame(
  Genauigkeit = genauigkeiten,
  Stichprobengroesse = n_notwendig_e
)

# Plot der notwendigen Stichprobengröße in Abhängigkeit von der Genauigkeit
plot_2 <- ggplot(genauigkeit_df, aes(x = Genauigkeit, y = Stichprobengroesse)) +
  geom_line(color = rgb(46/255, 69/255, 82/255), size = 1.5) +
  labs(title = "Stichprobenumfang zu Genauigkeit",
       x = "Genauigkeit (e) (€)",
       y = "Notwendiger Stichprobenumfang") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".",decimal.mark =",", scientific = FALSE))+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )


ggsave("Stichprobenumfang.png", plot = plot_2, width = 11, height = 7, units = "cm", bg = "transparent")





# Visualisierung der Konfidenzintervalle und des Zusammenhangs zwischen Stichprobengröße und Genauigkeit

# Daten für verschiedene Stichprobengrößen generieren
stichproben_groessen <- seq(50, 1000, by = 10)
konfidenzintervalle <- sapply(stichproben_groessen, function(n) {
  stichproben_sd <- sd(konsumausgaben)
  c(stichprobenmittelwert - z * (stichproben_sd / sqrt(n)),
    stichprobenmittelwert + z * (stichproben_sd / sqrt(n)))
})

konfidenzintervall_df <- data.frame(
  Stichprobengroesse = rep(stichproben_groessen, each = 2),
  Grenze = as.vector(konfidenzintervalle),
  Typ = rep(c("untere Grenze", "obere Grenze"), times = length(stichproben_groessen))
)

# Plot der Konfidenzintervalle
plot_3 <- ggplot(konfidenzintervall_df, aes(x = Stichprobengroesse, y = Grenze, color = Typ)) +
  geom_line(size =1.5) +
  scale_color_manual(values = c(rgb(46/255, 69/255, 82/255), rgb(209/255, 186/255, 173/255))) +
  labs(title = "Konfidenzintervalle zu Stichprobenumfang",
       x = "Stichprobenumfang",
       y = "Grenze des Konfidenzintervalls (€)") +
  scale_x_continuous(breaks = seq(0, 1000, by = 100)) +
  guides(color = guide_legend(title = "Konfidenzintervall"))+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 8, face = "plain"), 
    legend.position = c(0.55, 0.1), 
    legend.text = element_text(margin = margin(l = -5, r = -5, t = -5, b = -5)),
    legend.spacing.y = unit(-5, 'cm'),
    legend.direction = "horizontal" ,
    legend.background = element_rect(fill = alpha('white', 0.5))
  )

ggsave("Konfidenzintervall.png", plot = plot_3, width = 11, height = 7, units = "cm", bg = "transparent")

