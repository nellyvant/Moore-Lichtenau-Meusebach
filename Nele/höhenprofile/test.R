#höhendaten test
library(dplyr)

hoehen_tabelle <- read.csv("C:/Users/Nele/OneDrive - Technische Universität Ilmenau/Dokumente/Moore-Lichtenau-Meusebach/Nele/höhenprofile/Meu-trock/mt_MO.csv", header=F)
str(hoehen_tabelle)
hoehen_tabelle <- hoehen_tabelle %>%
  mutate(V2 = as.numeric(gsub(",", ".", V2)))
hoehen_tabelle
#funktion
werte <- hoehen_tabelle$V2   # deine 47 Höhenwerte

n_plots <- length(27)
n_hoehe <- length(werte)

faktor <- n_hoehe / n_plots  # 47 / 32 = 1.46875

hoehe_plot <- sapply(1:n_plots, function(i) {
  
  start <- (i - 1) * faktor
  ende  <- i * faktor
  
  anteile <- sapply(1:n_hoehe, function(j) {
    zell_start <- j - 1
    zell_ende  <- j
    
    max(0, min(ende, zell_ende) - max(start, zell_start))
  })
  
  sum(werte * anteile) / sum(anteile)
})

hoehe_plot_df <- data.frame(
  Plot_im_Transekt = 1:27,
  Hoehe = hoehe_plot
)
hoehe_plot_df
plot(hoehe_plot, type = "b")
range(hoehe_plot)

range(werte)
unique(werte)

werte <- hoehen_tabelle$Hoehe

n_plots <- 27
n_hoehe <- length(werte)

hoehe_plot <- sapply(1:n_plots, function(i) {
  
  start <- (i - 1) * n_hoehe / n_plots
  ende  <- i * n_hoehe / n_plots
  
  anteile <- pmax(
    0,
    pmin(ende, 1:n_hoehe) - pmax(start, 0:(n_hoehe - 1))
  )
  
  weighted.mean(werte, anteile)
})

hoehe_plot_df <- data.frame(
  Plot_im_Transekt = 1:27,
  Hoehe = hoehe_plot
)



hoehe_plot <- approx(
  x = seq(1, n_plots, length.out = length(werte)),
  y = werte,
  xout = 1:n_plots
)$y
hoehe_plot
hoehe_plot_df$Hoehe
plot(hoehe_plot, type = "b")


hoehe_plot <- approx(
  x = seq(0, 1, length.out = length(werte)),
  y = werte,
  xout = seq(0, 1, length.out = n_plots)
)$y



22.5*2
#beste lösung: lineare interpolation

# Höhenprofil
x_hoehe <- seq(0, 1, length.out = length(werte))
x_plot  <- seq(0, 1, length.out = n_plots)

hoehe_plot <- approx(
  x = seq(0, 1, length.out = length(werte)),
  y = werte,
  xout = seq(0, 1, length.out = n_plots)
)$y
plot(hoehe_plot, type ="b")
hoehe_plot
str(hoehe_plot)







##### test für plots aus logger mit signifikamzniveaus####

summary_daily_logger %>%
  filter(Standort == "L") %>%
  ggplot(aes(x=Logger,fill = Logger, y=Licht_mean)) +
  geom_boxplot(width=0.8)+
  labs(title ="Licht mean Kleiner Sumpf ",
       x="Logger",
       y="tägliche Lichtsumme")

daten_L <- summary_daily_logger %>%
  filter(Standort == "L") %>%
  arrange(Logger_ID, Datum)

friedman.test(Licht_mean ~ Logger_ID | Datum, data = daten_L)

pairwise.wilcox.test(
  daten_L$Licht_mean,
  daten_L$Logger_ID,
  paired = TRUE,
  p.adjust.method = "BH",
  exact = FALSE
)



daten_L <- summary_daily_logger %>%
  filter(Standort == "L")

buchstaben <- data.frame(
  Logger = c("M", "N", "O", "S", "W"),
  gruppe = c("a", "b", "b", "c", "d")
)

buchstaben_pos <- daten_L %>%
  group_by(Logger) %>%
  summarise(y = max(Licht_mean, na.rm = TRUE) * 1.05) %>%
  left_join(buchstaben, by = "Logger")

ggplot(daten_L, aes(x = Logger, fill = Logger, y = Licht_mean)) +
  geom_boxplot(width = 0.8) +
  geom_text(
    data = buchstaben_pos,
    aes(x = Logger, y = y, label = gruppe),
    inherit.aes = FALSE,
    size = 6
  ) +
  labs(
    title = "Licht mean Kleiner Sumpf",
    x = "Logger",
    y = "Licht mean"
  ) +
  theme_classic()



summary_daily_logger %>%
  filter(Standort == "L") %>%
  {
    y_pos <- max(.$Licht_mean, na.rm = TRUE) * 1.08
    
    ggplot(., aes(x = Logger, fill = Logger, y = Licht_mean)) +
      geom_boxplot(width = 0.8) +
      annotate(
        "text",
        x = c("M", "N", "O", "S", "W"),
        y = y_pos,
        label = c("a", "b", "b", "c", "d"),
        size = 6
      ) +
      coord_cartesian(ylim = c(NA, y_pos * 1.08)) +
      labs(
        title = "Licht mean Kleiner Sumpf",
        x = "Logger",
        y = "tägliche Lichtsumme"
      )
  }
