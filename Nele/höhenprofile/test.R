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


##### Niederschläge#####

#Frage: unterscheiden sich die austrocknungsraten der drei flächen signifikant?
#ich betrachte nur die tage ohne regen, da an regentagen LF sprunghaft ansteigt



#plusprecip im daily logger summary
plusprecip <- left_join(summary_daily_logger, precip, by = "Datum")
str(plusprecip)
plusprecip <- plusprecip %>%
  select(-Temp_sonne_mean:-Licht_summe)
str(plusprecip)
plusprecip <- plusprecip %>%
  mutate(Monat = format(Datum, "%Y-%m"))

plusprecip%>%
  filter(Standort =="L")%>%
  ggplot(aes(x = Datum)) +
  geom_line(aes(y = rLF_mean,
                color = Logger),
            linewidth = 1) +
  geom_col(aes(y = `rain (mm)`),
           fill = "blue",
           alpha = 0.5) +
  
  scale_y_continuous(
    name = "Luftfeuchte (%)",
    sec.axis = sec_axis(~ . / 5,
                        name = "Niederschlag (mm)")
  ) +
  
  facet_wrap(~ Monat, scales = "free_x") +
  
  theme_minimal()

plusprecip <- plusprecip %>%
  arrange(Datum) %>%
  mutate(
    Regen_gestern = lag(`rain (mm)`, 1),
    Regen_3Tage = zoo::rollsum(`rain (mm)`,
                               k = 3,
                               fill = NA,
                               align = "right")) %>%
  group_by(Datum) %>%
  mutate(
    Regen = first(`rain (mm)`) > 0) %>%
  ungroup() %>%
  mutate(
    letzter_Regen = if_else(Regen, Datum, as.POSIXct(NA))) %>%
  mutate(
    letzter_Regen = zoo::na.locf(letzter_Regen, na.rm = FALSE)) %>%
  mutate(
    Tage_seit_Regen =as.numeric(difftime(Datum, letzter_Regen,units = "days"))) %>%
  select(-letzter_Regen) %>%
  arrange(Logger_ID, Datum) %>%
  group_by(Logger_ID) %>%
  mutate(
    delta_LF = rLF_mean - lag(rLF_mean)
  ) %>%
  ungroup()

trocken <- plusprecip %>%
  filter(
    Regen == FALSE,
    !is.na(Tage_seit_Regen))

mod <-trocken%>%
  filter(Standort =="MT")%>%
  {lm(
  rLF_mean ~ Tage_seit_Regen * Logger_ID,
  data=.)}

summary(mod)
anova(mod)

trocken %>%
  filter(Standort=="MT")%>%
  ggplot(aes(x = Tage_seit_Regen,
           y = rLF_mean,
           color = Logger_ID)) +
  geom_point(alpha = 0.4) +
  geom_smooth(aes(fill=Logger_ID),method = "lm", se = T, alpha=0.1) +
  theme_minimal() +
  labs(
    x = "Tage seit letztem Niederschlag",
    y = "mittlere relative Luftfeuchte (%)",
    color = "Logger"
  )




##### Niederschläge ordentlich#####
#| label: Niederschlagsereignisse von externer Klimastation
precip <- read_xlsx("Niederschlag_AWEKAS_Station_Meusebach.xlsx")
str(precip)
precip <- rename(precip, Datum = Date)
precip%>%
  ggplot(aes(x = Date, y = `rain (mm)`)) + geom_line()

# Join an AREA summary
plusprecipa <- left_join(summary_daily_area, precip, by = "Datum")
str(plusprecipa)
plusprecipa <- plusprecipa %>%
  select(-Temp_area_sonne_mean:-Licht_summe)
str(plusprecipa)
plusprecipa <- plusprecipa %>%
  mutate(Monat = format(Datum, "%Y-%m"))

#Plot NS und Luftfeuchte über Zeit
ggplot(plusprecipa, aes(x = Datum)) +
  geom_line(aes(y = rLF_area_mean,
                color = Standort),
            linewidth = 1) +
  geom_col(aes(y = `rain (mm)`),
           fill = "blue",
           alpha = 0.5) +
  scale_y_continuous(
    name = "Luftfeuchte (%)",
    sec.axis = sec_axis(~ . / 5,
                        name = "Niederschlag (mm)")) +
  facet_wrap(~ Monat, scales = "free_x") +
  theme_minimal()

# Korrelationstests
cor.test(plusprecipa$`rain (mm)`, plusprecipa$rLF_area_mean, method = "spearman")
#p-value < 2.2e-16,  rho 0.6032023 -> logisch, wenns regnet ist zeitgleich die LF hoch

plusprecipa <- plusprecipa %>%
  arrange(Datum) %>%
  mutate(
    Regen_gestern = lag(`rain (mm)`, 1),
    Regen_3Tage = zoo::rollsum(`rain (mm)`,
                               k = 3,
                               fill = NA,
                               align = "right"))
cor.test(plusprecipa$Regen_gestern, plusprecipa$rLF_area_mean, method = "spearman")
#p-value < 2.2e-16,  rho 0.5398155 
cor.test(plusprecipa$Regen_3Tage, plusprecipa$rLF_area_mean, method = "spearman")
#p-value < 2.2e-16,  rho 0.5821407

#Regen erhöht am selben Tag die LF (logisch), beeinflusst die LF am nächsten Tag und auch in den nächsten 3 Tagen


#Austrockungsrate vergleichen
plusprecipa <- plusprecipa %>%
  group_by(Datum) %>%
  mutate(
    Regen = first(`rain (mm)`) > 0) %>%
  ungroup() %>%
  mutate(
    letzter_Regen = if_else(Regen, Datum, as.POSIXct(NA))) %>%
  mutate(
    letzter_Regen = zoo::na.locf(letzter_Regen, na.rm = FALSE)) %>%
  mutate(
    Tage_seit_Regen =as.numeric(difftime(Datum, letzter_Regen,units = "days"))) %>%
  select(-letzter_Regen) %>%
  arrange(Standort, Datum) %>%
  group_by(Standort) %>%
  mutate(
    delta_LF = rLF_area_mean - lag(rLF_area_mean)
  ) %>%
  ungroup()
#verwende nur Tage ohne Regen, da an Regentagen keine Austrocknung stattfindet
trockena <- plusprecip %>%
  filter(
    Regen == FALSE,
    !is.na(Tage_seit_Regen))

mod <- lm(
  rLF_area_mean ~ Tage_seit_Regen * Standort,
  data = trockena
)

summary(mod)
#Tage seit Regen erklärt signifikant die Abnahme in LF (logisch)
#L: LF nimmt 1,8% pro Tag ab
#MN: LF nimmt 1,8%+0,5%= 2,3% pro Tag ab
#MT LF nimmt 1,8%+0,1%= 1,9% pro Tag ab
#Unterschiede zwishcen Flächen nicht signifikant
anova(mod)

ggplot(trockena,
       aes(x = Tage_seit_Regen,
           y = rLF_area_mean,
           color = Standort)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    x = "Tage seit letztem Niederschlag",
    y = "mittlere relative Luftfeuchte (%)",
    color = "Standort"
  )

# Join mit LOGGER summary

plusprecipl <- left_join(summary_daily_logger, precip, by = "Datum")
str(plusprecipl)
plusprecipl <- plusprecipl %>%
  select(-Temp_area_sonne_mean:-Licht_summe)
str(plusprecipl)
plusprecipl <- plusprecipl %>%
  mutate(Monat = format(Datum, "%Y-%m"))

#Plot NS und Luftfeuchte über Zeit
ggplot(plusprecipl, aes(x = Datum)) +
  geom_line(aes(y = rLF_area_mean,
                color = Logger_ID),
            linewidth = 1) +
  geom_col(aes(y = `rain (mm)`),
           fill = "blue",
           alpha = 0.5) +
  scale_y_continuous(
    name = "Luftfeuchte (%)",
    sec.axis = sec_axis(~ . / 5,
                        name = "Niederschlag (mm)")) +
  facet_wrap(~ Monat, scales = "free_x") +
  theme_minimal()

# Korrelationstests
cor.test(plusprecipl$`rain (mm)`, plusprecipl$rLF__mean, method = "spearman")

plusprecipl <- plusprecipl %>%
  arrange(Datum) %>%
  mutate(
    Regen_gestern = lag(`rain (mm)`, 1),
    Regen_3Tage = zoo::rollsum(`rain (mm)`,
                               k = 3,
                               fill = NA,
                               align = "right"))
cor.test(plusprecipl$Regen_gestern, plusprecipl$rLF_area_mean, method = "spearman")

cor.test(plusprecipl$Regen_3Tage, plusprecipl$rLF_area_mean, method = "spearman")


#Regen erhöht am selben Tag die LF (logisch), beeinflusst die LF am nächsten Tag und auch in den nächsten 3 Tagen


#Austrockungsrate vergleichen
plusprecipl <- plusprecipl %>%
  group_by(Datum) %>%
  mutate(
    Regen = first(`rain (mm)`) > 0) %>%
  ungroup() %>%
  mutate(
    letzter_Regen = if_else(Regen, Datum, as.POSIXct(NA))) %>%
  mutate(
    letzter_Regen = zoo::na.locf(letzter_Regen, na.rm = FALSE)) %>%
  mutate(
    Tage_seit_Regen =as.numeric(difftime(Datum, letzter_Regen,units = "days"))) %>%
  select(-letzter_Regen) %>%
  arrange(Standort, Datum) %>%
  group_by(Standort) %>%
  mutate(
    delta_LF = rLF_mean - lag(rLF_mean)
  ) %>%
  ungroup()
#verwende nur Tage ohne Regen, da an Regentagen keine Austrocknung stattfindet
trockenl <- plusprecipl %>%
  filter(
    Regen == FALSE,
    !is.na(Tage_seit_Regen))

mod <-trocken%>%
  filter(Standort =="MT")%>%
  {lm(
    rLF_mean ~ Tage_seit_Regen * Logger_ID,
    data=.)}

summary(mod)
anova(mod)

trocken %>%
  filter(Standort=="MT")%>%
  ggplot(aes(x = Tage_seit_Regen,
             y = rLF_mean,
             color = Logger_ID)) +
  geom_point(alpha = 0.4) +
  geom_smooth(aes(fill=Logger_ID),method = "lm", se = T, alpha=0.1) +
  theme_minimal() +
  labs(
    x = "Tage seit letztem Niederschlag",
    y = "mittlere relative Luftfeuchte (%)",
    color = "Logger"
  )