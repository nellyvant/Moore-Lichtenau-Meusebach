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
