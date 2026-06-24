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


##### ornden Signifikanztests#####

#| label: verschiedene Methoden für Signifikanztestung (Beispiel Lichtenau) bzw alle flächen gegeneinander
#testen auf normalverteilung

#innerhalb des Standort
#summary_daily_logger
library(ggpubr)
summary_daily_logger %>%
  filter(Standort == "L") %>%
  ggqqplot(
    x = "Licht_mean",
    facet.by = "Logger",
    add = "qqline"
  )
summary_daily_logger %>%
  filter(Standort == "L") %>%
  ggplot(aes(x = Licht_mean)) +
  geom_histogram(bins = 10) +
  facet_wrap(~ Logger, scales = "free") 
#alle in etwa normalverteilt

#Innerhalb der Fläche
#summary_daily_area

summary_daily_area %>%
  ggqqplot(
    x = "Licht_area_mean",
    facet.by = "Standort",
    add = "qqline")
summary_daily_area %>%
  ggplot(aes(x = Licht_area_mean)) +
  geom_histogram(bins = 10) +
  facet_wrap(~ Standort, scales = "free") 
#nicht normalverteilt



##Signifikanztets Möglichkeiten


#1. T-Tests
#Annahme der Normalverteilung ->häufig nicht gegeben
dat_L <- summary_daily_logger %>%
  filter(Standort == "L") %>%
  select(Datum, Logger, Licht_summe) %>%
  pivot_wider(
    names_from = Logger,
    values_from = Licht_summe
  )
#2.a) händisch
t.test(dat_L$M, dat_L$N, paired = TRUE) #p-value < 2.2e-16
t.test(dat_L$M, dat_L$O, paired = TRUE) #p-value < 2.2e-16
t.test(dat_L$M, dat_L$W, paired = TRUE) #p-value < 2.2e-16
t.test(dat_L$M, dat_L$S, paired = TRUE) #p-value < 2.2e-16
t.test(dat_L$N, dat_L$O, paired = TRUE) #p-value = 0.1268
t.test(dat_L$N, dat_L$W, paired = TRUE) #p-value < 2.2e-16
t.test(dat_L$N, dat_L$S, paired = TRUE) #p-value < 2.2e-16
t.test(dat_L$O, dat_L$W, paired = TRUE) #p-value = 8.215e-16
t.test(dat_L$O, dat_L$S, paired = TRUE) #p-value < 2.2e-16
t.test(dat_L$W, dat_L$S, paired = TRUE) #p-value < 2.2e-16

#alle Logger signif. unterchidlich außer N&O

#2.b) automatisierte T-tests mit ChatGPT 5.5
logger <- c("M", "N", "O", "S", "W")
ergebnisse <- combn(logger, 2, simplify = FALSE)
tests <- lapply(ergebnisse, function(x) {
  test <- t.test(
    dat_L[[x[1]]],
    dat_L[[x[2]]],
    paired = TRUE)
  data.frame(
    Gruppe1 = x[1],
    Gruppe2 = x[2],
    p_Wert = test$p.value,
    Mittelwert_Diff = mean(dat_L[[x[1]]] - dat_L[[x[2]]],na.rm = TRUE))
})
tests_df <- bind_rows(tests)
tests_df

#plot
summary_daily_logger %>%
  filter(Standort == "L") %>%
  ggplot(aes(x = Logger,
             fill = Logger,
             y = Licht_summe)) +
  geom_boxplot(width = 0.8) +
  geom_text(
    data = summary_daily_logger %>%
      filter(Standort == "L") %>%
      group_by(Logger) %>%
      summarise(
        y = max(Licht_summe, na.rm = TRUE) * 1.08,
        gruppe = case_when(
          Logger == "M" ~ "a",
          Logger == "N" ~ "b",
          Logger == "O" ~ "b",
          Logger == "S" ~ "c",
          Logger == "W" ~ "d"
        ),
        .groups = "drop"
      ),
    aes(x = Logger, y = y, label = gruppe),
    inherit.aes = FALSE,
    size = 6,
    fontface = "bold"
  ) +
  labs(title = "Lichtverhältnisse Kleiner Sumpf",
       x = "Logger",
       y = "kumulative Lichtintensität (Lux × Zeit)") +
  theme_minimal() +
  theme(legend.position = "none")





#3. Testung auf signif. Unterschiede mit mehreren gepaarten T-test  und sqrt transformation um Normalverteilung zu erreichen
dat_L_sqrt <- summary_daily_logger %>%
  filter(Standort == "L")%>%
  mutate(Licht_sqrt = sqrt(Licht_summe)) %>%
  select(Datum, Logger, Licht_sqrt) %>%
  pivot_wider(
    names_from = Logger,
    values_from = Licht_sqrt
  )


tests <- lapply(ergebnisse, function(x) {
  test <- t.test(
    dat_L_sqrt[[x[1]]],
    dat_L_sqrt[[x[2]]],
    paired = TRUE)
  data.frame(
    Gruppe1 = x[1],
    Gruppe2 = x[2],
    p_Wert = test$p.value,
    Mittelwert_Diff = mean(dat_L_sqrt[[x[1]]] - dat_L_sqrt[[x[2]]],na.rm = TRUE))
})
tests_df <- bind_rows(tests)
tests_df

#alle signif unterschiedl außer N&O


#4. Testung auf signif. Unterschiede mit friedman test
dat_L_fried <- summary_daily_logger %>%
  filter(Standort == "L") %>%
  arrange(Logger_ID, Datum)

friedman.test(Licht_mean ~ Logger_ID | Datum, data = dat_L_fried)

pairwise.wilcox.test(
  dat_L_fried$Licht_mean,
  dat_L_fried$Logger_ID,
  paired = TRUE,
  p.adjust.method = "BH",
  exact = FALSE
)

#alle Logger in L unterscheiden sich signif außer N&O

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


#5. Test mit repeated measures anova
library(rstatix)

res <- anova_test(
  data = dat_L_fried,
  dv = Licht_mean,
  wid = Datum,
  within = Logger
)

get_anova_table(res)

pwc <- dat_L_fried %>%
  pairwise_t_test(
    Licht_mean ~ Logger, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

#alle logger in L unterscheiden sich signif. außer N&O



# 1. ein model -> chatgpt

mod_L_light <- lmer(log10(Licht_summe + 1) ~ Logger + (1|Datum),
                    data = summary_daily_logger %>%
                      filter(Standort == "L"))
summary(mod_L_light)
coefs <- fixef(mod_L_light)
intercept_lux <- 10^(coefs[1]) - 1
prozent <- (10^(coefs[-1]) - 1) * 100
#ODER
mod_light_L <- lmer(
  Licht_summe ~ Logger + (1|Datum),
  data = summary_daily_logger %>%
    filter(Standort == "L")
)
library(emmeans)
pairs <- emmeans(mod_light_L, pairwise ~ Logger)
pairs
# alle signif unterschiedlich außer N&O


#####Signifikanztests Version vom 16.6. VOR dem ordnen für meeting mit markus#####
#| label: verschiedene Methoden für Signifikanztestung (Beispiel Lichtenau)
#testen auf normalverteilung

#summary_daily_logger
summary_daily_logger %>%
  filter(Standort == "L") %>%
  {
    qqnorm(.$Licht_mean)
    qqline(.$Licht_mean)
    hist(.$Licht_mean)
    shapiro.test(.$Licht_mean)
  }


#für alle standorte testen siehe nächster code oder wie oben drüber für jeden stanodrt filtern?
#summary_daily_area
qqnorm(summary_daily_area$Licht_area_mean); qqline(summary_daily_area$Licht_area_mean)
hist(summary_daily_area$Licht_area_mean)
#normalverteilt
qqnorm(summary_daily_area$Licht_area_mean_day); qqline(summary_daily_area$Licht_area_mean_day)
hist(summary_daily_area$Licht_area_mean_day)
#normalverteilt
qqnorm(sqrt(summary_daily_area$Licht_summe)); qqline(sqrt(summary_daily_area$Licht_summe))
hist(sqrt(summary_daily_area$Licht_summe))
#normalverteilt, aber leicht schief


#qqnorm(summary_daily_logger$Licht_mean); qqline(summary_daily_logger$Licht_mean)
#hist(summary_daily_logger$Licht_mean)
#qqnorm(summary_daily_logger$Licht_mean_day); qqline(summary_daily_logger$Licht_mean_day)
#hist(summary_daily_logger$Licht_mean_day)
#qqnorm(summary_daily_logger$Licht_summe); qqline(summary_daily_logger$Licht_summe)
#hist(summary_daily_logger$Licht_summe)
#alle nicht normalverteilt
#qqnorm(sqrt(summary_daily_logger$Licht_mean)); qqline(sqrt(summary_daily_logger$Licht_mean))
#hist(sqrt(summary_daily_logger$Licht_mean))
#qqnorm(sqrt(summary_daily_logger$Licht_mean_day)); qqline(sqrt(summary_daily_logger$Licht_mean_day))
#hist(sqrt(summary_daily_logger$Licht_mean_day))
#qqnorm(sqrt(summary_daily_logger$Licht_summe)); qqline(sqrt(summary_daily_logger$Licht_summe))
#hist(sqrt(summary_daily_logger$Licht_summe))
#jetzt ist alles normalverteilt



##Signifikanztets Möglichkeiten
# 1. ein model -> chatgpt

mod_L_light <- lmer(log10(Licht_summe + 1) ~ Logger + (1|Datum),
                    data = summary_daily_logger %>%
                      filter(Standort == "L"))
summary(mod_L_light)
coefs <- fixef(mod_L_light)
intercept_lux <- 10^(coefs[1]) - 1
prozent <- (10^(coefs[-1]) - 1) * 100
#ODER
mod_light_L <- lmer(
  Licht_summe ~ Logger + (1|Datum),
  data = summary_daily_logger %>%
    filter(Standort == "L")
)
library(emmeans)
pairs <- emmeans(mod_light_L, pairwise ~ Logger)
pairs
# contrast estimate    SE  df t.ratio p.value
# M - N      812334 42600 580  19.055 <0.0001
# M - O      838218 42600 580  19.662 <0.0001
# M - S     1951543 42600 580  45.778 <0.0001
# M - W     1115488 42600 580  26.167 <0.0001
# N - O       25884 42600 580   0.607  0.9740
# N - S     1139209 42600 580  26.723 <0.0001
# N - W      303154 42600 580   7.111 <0.0001
# O - S     1113325 42600 580  26.116 <0.0001
# O - W      277270 42600 580   6.504 <0.0001
# S - W     -836055 42600 580 -19.612 <0.0001

#2. T-Tests
#Annahme der Normalverteilung ->häufig nicht gegeben
dat_L <- summary_daily_logger %>%
  filter(Standort == "L") %>%
  select(Datum, Logger, Licht_summe) %>%
  pivot_wider(
    names_from = Logger,
    values_from = Licht_summe
  )
#2.a) händisch
t.test(dat_L$M, dat_L$N, paired = TRUE) #p-value < 2.2e-16
t.test(dat_L$M, dat_L$O, paired = TRUE) #p-value < 2.2e-16
t.test(dat_L$M, dat_L$W, paired = TRUE) #p-value < 2.2e-16
t.test(dat_L$M, dat_L$S, paired = TRUE) #p-value < 2.2e-16
t.test(dat_L$N, dat_L$O, paired = TRUE) #p-value = 0.1268
t.test(dat_L$N, dat_L$W, paired = TRUE) #p-value < 2.2e-16
t.test(dat_L$N, dat_L$S, paired = TRUE) #p-value < 2.2e-16
t.test(dat_L$O, dat_L$W, paired = TRUE) #p-value = 8.215e-16
t.test(dat_L$O, dat_L$S, paired = TRUE) #p-value < 2.2e-16
t.test(dat_L$W, dat_L$S, paired = TRUE) #p-value < 2.2e-16

#2.b) automatisierte T-tests mit ChatGPT 5.5
logger <- c("M", "N", "O", "S", "W")
ergebnisse <- combn(logger, 2, simplify = FALSE)
tests <- lapply(ergebnisse, function(x) {
  test <- t.test(
    dat_L[[x[1]]],
    dat_L[[x[2]]],
    paired = TRUE)
  data.frame(
    Gruppe1 = x[1],
    Gruppe2 = x[2],
    p_Wert = test$p.value,
    Mittelwert_Diff = mean(dat_L[[x[1]]] - dat_L[[x[2]]],na.rm = TRUE))
})
tests_df <- bind_rows(tests)
tests_df

#plot
summary_daily_logger %>%
  filter(Standort == "L") %>%
  ggplot(aes(x = Logger,
             fill = Logger,
             y = Licht_summe)) +
  geom_boxplot(width = 0.8) +
  geom_text(
    data = summary_daily_logger %>%
      filter(Standort == "L") %>%
      group_by(Logger) %>%
      summarise(
        y = max(Licht_summe, na.rm = TRUE) * 1.08,
        gruppe = case_when(
          Logger == "M" ~ "a",
          Logger == "N" ~ "b",
          Logger == "O" ~ "b",
          Logger == "S" ~ "c",
          Logger == "W" ~ "d"
        ),
        .groups = "drop"
      ),
    aes(x = Logger, y = y, label = gruppe),
    inherit.aes = FALSE,
    size = 6,
    fontface = "bold"
  ) +
  labs(title = "Lichtverhältnisse Kleiner Sumpf",
       x = "Logger",
       y = "kumulative Lichtintensität (Lux × Zeit)") +
  theme_minimal() +
  theme(legend.position = "none")





#Testung auf signif. Unterschiede mit mehreren gepaarten T-test  und sqrt transformation um Normalverteilung zu erreichen
dat <- summary_daily_logger %>%
  filter(Standort == "L",
         Logger %in% c("M", "N")) %>%
  mutate(Licht_sqrt = sqrt(Licht_summe)) %>%
  select(Datum, Logger, Licht_sqrt) %>%
  pivot_wider(
    names_from = Logger,
    values_from = Licht_sqrt
  )

t.test(dat$M, dat$N, paired = TRUE)




#Testung auf signif. Unterschiede mit friedman test
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


#Test mit repeated measures anova
library(rstatix)

res <- anova_test(
  data = data,
  dv = luftfeuchte,
  wid = zeitpunkt,
  within = standort
)

get_anova_table(res)

#library(e1071)
#kurtosis(summary_daily_logger$Licht_mean);skewness(summary_daily_logger$Licht_mean)



#####Signifikanztest deltaLF#####
str(trockenl)
trockenl %>%
  filter(Standort == "L") %>%
  ggqqplot(
    x = "delta_LF",
    facet.by = "Logger",
    add = "qqline"
  )
trockenl %>%
  filter(Standort == "L") %>%
  ggplot(aes(x = delta_LF)) +
  geom_histogram(bins = 10) +
  facet_wrap(~ Logger, scales = "free") 
shapiro.test(trockenl$delta_LF[trockenl$Standort == "L"])
#keine Normalverteilung
#friedman test
dat_L_fried <- summary_daily_logger %>%
  filter(Standort == "L") %>%
  arrange(Logger_ID, Datum)

dat_rLF_L<- trockenl %>%
  filter(Standort =="L")%>%
  arrange(Logger_ID, Datum)

friedman.test( delta_LF ~ Logger_ID | Datum, data = dat_rLF_L)

pairwise.wilcox.test(
  dat_rLF_L$delta_LF,
  dat_rLF_L$Logger_ID,
  paired = TRUE,
  p.adjust.method = "BH",
  exact = FALSE
)
dat_rLF_L %>%
  group_by(Logger_ID) %>%
  summarise(
    n = sum(!is.na(delta_LF)),
    Median = median(delta_LF, na.rm = TRUE),
    IQR = IQR(delta_LF, na.rm = TRUE)
  ) %>%
  arrange(Median)
ggplot(dat_rLF_L, aes(x = Logger_ID, y = delta_LF)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.3) +
  labs(x = "Logger", y = "delta_LF") +
  theme_classic()


#model
#join trocken mit daten aus summary-daily_logger
mod_data<- trockenl %>%
  left_join(summary_daily_logger%>% select(Logger_ID,Datum,Licht_summe, Temp_schatten_mean), by = c("Logger_ID","Datum"))

mod<- lmer(delta_LF~Temp_schatten_mean + Licht_summe +(1| Logger_ID), data=mod_data)
plot(mod)
summary(mod)
anova(mod)

mod <- lm(
  plusprecipl$deltaLF ~  summary_daily_logger$Temp_schatten_mean + summary_daily_logger$Licht_summe* summary_daily_logger$Standort)
plot(mod)
summary(mod)
#Tage seit Regen erklärt signifikant die Abnahme in LF (logisch)
#L: LF nimmt 1,8% pro Tag ab
#MN: LF nimmt 1,8%+0,5%= 2,3% pro Tag ab
#MT LF nimmt 1,8%+0,1%= 1,9% pro Tag ab
#Unterschiede zwischen Flächen nicht signifikant
#anova(mod)



#deckung tree layer an den loggern
deckung_TL <- veg_long %>%
  filter(
    Schicht == "TL",
    Transekt %in% c("NS", "WO")
  ) %>%
  group_by(site, Transekt) %>%
  filter(
    Transektnummer == min(Transektnummer, na.rm = TRUE) |
      Transektnummer == max(Transektnummer, na.rm = TRUE) |
      Transektnummer == 0
  ) %>%
  ungroup() %>%
  group_by(
    site,
    Transekt,
    Transektnummer,
    Name,
    plot.ID,
    arbeitsname
  ) %>%
  summarise(
    Deckung_TL = sum(Deckung, na.rm = TRUE),
    .groups = "drop"
  )
deckung_TL
deckung_TL <- deckung_TL %>%
  group_by(site, Transekt) %>%
  mutate(
    logger = case_when(
      Transektnummer == 0 ~ "M",
      
      Transekt == "NS" &
        Transektnummer == min(Transektnummer, na.rm = TRUE) ~ "S",
      
      Transekt == "NS" &
        Transektnummer == max(Transektnummer, na.rm = TRUE) ~ "N",
      
      Transekt == "WO" &
        Transektnummer == min(Transektnummer, na.rm = TRUE) ~ "W",
      
      Transekt == "WO" &
        Transektnummer == max(Transektnummer, na.rm = TRUE) ~ "O",
      
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()
deckung_TL

#####Top10 redo#####
p3_data <- MT_subset %>%
  select(Art, Deckung) %>%
  group_by(Art) %>%
  summarise(
    mean_deckung = mean(Deckung, na.rm = TRUE),
    n = sum(Deckung > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  slice_max(
    order_by = mean_deckung,
    n = 10,
    with_ties = FALSE
  ) %>%
  mutate(
    Art = reorder(Art, mean_deckung),
    label = paste0("n=", n)
  ) %>%
  as.data.frame()

p3 <- ggplot(
  p3_data,
  aes(x = Art, y = mean_deckung)
) +
  geom_col(fill = "darkgreen") +
  geom_text(
    aes(label = label),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 45)) +
  labs(
    x = "Art",
    y = "Mittlere Deckung %",
    title = "Fuchshügel"
  ) +
  theme_minimal()
p3
object.size(p3)



#####DCA worklflow#####
#pfeile der umwelt nur für ganze flächen anzeigen lassen, farbe nach entfernung zur mitte
#arten nur auf transekten anzeigen lassen, um gemeinschaften zu erkennen, farbe nach richtung (WO,NS)

#DCA für die ganze Fläche mit Pfeilen
data_MN <- merged_data$site == "Meu_trock" 
comm_MN <- comm_matrix[data_MN, , drop = FALSE]


comm_MN <- comm_MN[
  rowSums(comm_MN) > 0,
  colSums(comm_MN) > 0,
  drop = FALSE
]

dca <- decorana(comm_MN)
summary(dca)

env_MN <- merged_data %>%
  filter(site == "Meu_trock")
fit <- envfit(dca, env_MN[, c("Weighted_light",
                              "Weighted_temperature",
                              "Weighted_wetness",
                              "Weighted_nitrogen",
                              "Weighted_alkalinity",
                              "Hoehe",
                              "TL",
                              "ML",
                              "HL",
                              "SL")])
fit

plot(dca)
arrows <-as.data.frame(scores(fit, display = "vectors"))
arrows$Variable <- rownames(arrows)
arrows$p_value <- fit$vectors$pvals
arrows$r2 <- fit$vectors$r
arrows$Variable <- c(
  "Licht",
  "Temperatur",
  "Feuchte",
  "Stickstoff",
  "Basengehalt",
  "Höhe",
  "Baumschicht",
  "Moosschicht",
  "Krautschicht",
  "Strauchschicht"
)
# nur Umweltvariablen mit signifikanten p-Werten anzeigen
arrows_sig <- arrows %>%
  filter(p_value <= 0.05)

dca_scores <- scores(dca, display = "sites")
dca_df <- as.data.frame(dca_scores)
dca_df$Name <- rownames(dca_df)
dca_df$site <- vegdata$site[match(dca_df$Name, vegdata$plot.ID)]
dca_df$Transektnummer <- vegdata$Transektnummer[match(dca_df$Name, vegdata$plot.ID)]
dca_df$Distanz <- abs(dca_df$Transektnummer)

ggplot(dca_df, aes(DCA1, DCA2, colour = Distanz)) +
  geom_point(size = 3) +
  scale_colour_gradient2(
    low = "#005A8D",
    mid = "#FFD700",
    high = "#7A0177",,
    midpoint = 0,
    name = "Entfernung zum Mittelpunkt")+
  geom_segment(
    data = arrows_sig,
    aes(
      x = 0, y = 0,
      xend = DCA1 * 2,
      yend = DCA2 * 2
    ),
    inherit.aes = FALSE,
    arrow = arrow(length = unit(0.25, "cm")),
    colour = "black") +
  geom_text(
    data = arrows_sig,
    aes(
      x = DCA1 * 2.3,
      y = DCA2 * 2.3,
      label = Variable
    ),
    inherit.aes = FALSE
  ) +
  
  labs(
    title = "DCA Vegetation Fuchshügel",
    x = "DCA1",
    y = "DCA2"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


#DCA für einzelne Transekte mit Darstellung der Arten
data_MN_WO <- merged_data$site == "Meu_trock" & merged_data$Transekt == "WO"
comm_MN_WO <- comm_matrix[idx_MN, , drop = FALSE]


comm_MN_WO <- comm_MN_WO[
  rowSums(comm_MN_WO) > 0,
  colSums(comm_MN_WO) > 0,
  drop = FALSE
]

dca <- decorana(comm_MN_WO)
summary(dca)

dca_scores <- scores(dca, display = "sites")
dca_df <- as.data.frame(dca_scores)
dca_df$Name <- rownames(dca_df)
dca_df$site <- vegdata$site[match(dca_df$Name, vegdata$plot.ID)]
dca_df$Transektnummer <- vegdata$Transektnummer[match(dca_df$Name, vegdata$plot.ID)]

species_df <- as.data.frame(scores(dca, display = "species", choices = c(1, 2)))
species_df$Art <- rownames(species_df)
species_totals <- colSums(comm_MN, na.rm = TRUE)
species_df <- species_df %>%
  filter(Art %in% names(species_totals[species_totals > 0]))

ggplot(dca_df, aes(DCA1, DCA2, colour = Transektnummer)) +
  geom_point(size = 3) +
  
  geom_text(
    data = species_df,
    aes(x = DCA1, y = DCA2, label = Art),
    inherit.aes = FALSE,
    size = 3,
    colour = "grey30"
  ) +
  scale_colour_gradient2(
    low = "#005A8D",
    mid = "#FFD700",
    high = "#7A0177",
    midpoint = 0,
    name = "Position im Transekt"
  ) +
  labs(
    title = "DCA Vegetation Auf der Pfalz",
    x = "DCA1",
    y = "DCA2"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )






ggplot(dca_df, aes(DCA1, DCA2, colour = Transektnummer)) +
  geom_point(size = 3) +
  
  # Symbole für die Arten
  geom_point(
    data = species_df,
    aes(x = DCA1, y = DCA2),
    inherit.aes = FALSE,
    shape = 17,          # Dreieck
    size = 3,
    colour = "grey20"
  ) +
  
  # Beschriftungen ohne Überlappung
  geom_text_repel(
    data = species_df,
    aes(x = DCA1, y = DCA2, label = Art),
    inherit.aes = FALSE,
    size = 3,
    colour = "grey20",
    box.padding = 0.5,
    point.padding = 0.3,
    segment.colour = "grey60",
    max.overlaps = Inf
  ) +
  
  scale_colour_gradient2(
    low = "#005A8D",
    mid = "#FFD700",
    high = "#7A0177",
    midpoint = 0,
    breaks = range(dca_df$Transektnummer, na.rm = TRUE),
    labels = c("Westen", "Osten"),
    name = "Position im Transekt"
  ) +
  
  labs(
    title = "DCA Vegetation Auf der Pfalz",
    x = "DCA1",
    y = "DCA2"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
#####dca test run#####
ggplot(dca_df, aes(DCA1, DCA2, colour = Distanz)) +
  geom_point(size = 5) +
  geom_point(
    data = species_df,
    aes(x = DCA1, y = DCA2),
    inherit.aes = FALSE,
    shape = 17,       
    size = 3,
    colour = "grey20"
  ) +
  geom_text_repel(
    data = species_df,
    aes(x = DCA1, y = DCA2, label = Art),
    inherit.aes = FALSE,
    size = 3,
    colour = "grey20",
    box.padding = 0.5,
    point.padding = 0.3,
    segment.colour = "grey60",
    max.overlaps = Inf
  ) +
  scale_colour_gradient2(
    low = "#005A8D",
    mid = "#FFD700",
    high = "#7A0177",
    name = "Entfernung vom Mittelpunkt"
  ) +
  labs(
    title = "DCA Vegetation Kleiner Sumpf, ohne Moose",
    x = "DCA1",
    y = "DCA2"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

