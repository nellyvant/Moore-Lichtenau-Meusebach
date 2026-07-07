########
#zuerst musst du in Analysis-clean alles bis zu den summaries laufen lassen, damit die Daten da sind, mit dene der Code rechnet, oder importiere eine workspace .RData mit den summaries
##### Einzelne Fläche #####
#Beispiel Lichtenau, andere Standorte durch Ersetzung zu analysieren
df_pca_L <- summary_daily_logger %>%
  filter(Standort =="MT")%>%
  select(Standort, Logger, Temp_schatten_mean,Licht_summe, rLF_mean)

pca <- rda(df_pca_L[, c("Temp_schatten_mean","Licht_summe", "rLF_mean")],
           scale = TRUE)
summary(pca)
screeplot(pca)
biplot(pca)


site_scores <- scores(pca, display = "sites")
var_scores  <- scores(pca, display = "species")

sites <- data.frame(site_scores,
                    Standort = df_pca_L$Standort,
                    Logger   = df_pca_L$Logger)

vars <- data.frame(var_scores,
                   Variable = rownames(var_scores))
eig <- summary(pca)$cont$importance[2, ]
vars <- data.frame(var_scores,
                   Variable = rownames(var_scores))
vars$Label <- c("Temperatur", "Licht", "Luftfeuchte") 

pfeilfaktor<- 0.1
ggplot() +
  stat_ellipse(data = sites,
               aes(PC1, PC2, fill =Logger),
               geom="polygon",
               alpha = 0.2,
               show.legend = F) +
  geom_point(data = sites,
             aes(PC1, PC2, color =Logger),
             size = 3) +
  #geom_text(data = sites,
  #          aes(PC1, PC2),
  #          vjust = -1, show.legend = F) +
  geom_segment(data = vars,
               aes(x = 0, y = 0,
                   xend = PC1*pfeilfaktor, yend = PC2*pfeilfaktor),
               arrow = arrow(length = unit(0.25, "cm")),
               color = "black") +
  # geom_text(data = vars,
  #            aes(PC1, PC2, label = Label),
  #            color = "black",
  #            vjust = -0.5, hjust= 0.3) +
  xlab(paste0("PC1 (", round(eig[1]*100,1), "%)")) +
  ylab(paste0("PC2 (", round(eig[2]*100,1), "%)")) +
  theme_bw() +
  coord_fixed(ratio = 0.7)

#keine Gruppierungen zu erkennen


##### alle Flächen #####
#nur Temp_schatten für bessere Vergleichbarkeit
df_pca <- summary_logger_year %>%
  select(Standort, Logger, Tempschatten_Jahresmittel,Licht_Jahresmittel, rLF_Jahresmittel)

pca <- rda(df_pca[, c("Tempschatten_Jahresmittel",
                      "Licht_Jahresmittel","rLF_Jahresmittel")],
           scale = TRUE)
summary(pca)
screeplot(pca)
biplot(pca)


site_scores <- scores(pca, display = "sites")
var_scores  <- scores(pca, display = "species")

sites <- data.frame(site_scores,
                    Standort = df_pca$Standort,
                    Logger   = df_pca$Logger)

vars <- data.frame(var_scores,
                   Variable = rownames(var_scores))
eig <- summary(pca)$cont$importance[2, ]
vars <- data.frame(var_scores,
                   Variable = rownames(var_scores))
vars$Label <- c("Temperatur", "Licht", "Luftfeuchte") 

ggplot() +
  stat_ellipse(data = sites,
               aes(PC1, PC2, fill=Standort),
               geom="polygon",
               alpha = 0.2,
               show.legend = F) +
  geom_point(data = sites,
             aes(PC1, PC2, color = Standort),
             size = 3) +
  geom_text(data = sites,
            aes(PC1, PC2, label = Logger, color = Standort),
            vjust = -1, show.legend = F) +
  geom_segment(data = vars,
               aes(x = 0, y = 0,
                   xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.25, "cm")),
               color = "black") +
  geom_text(data = vars,
            aes(PC1, PC2, label = Label),
            color = "black",
            vjust = -0.5, hjust= 0.3) +
  xlab(paste0("PC1 (", round(eig[1]*100,1), "%)")) +
  ylab(paste0("PC2 (", round(eig[2]*100,1), "%)")) +
  theme_bw() +
  coord_fixed(ratio = 0.7)


# Hull-Funktion für jeden Standort
hulls <- sites %>%
  dplyr::group_by(Standort) %>%
  dplyr::slice(chull(PC1, PC2))  
vars$LabelX <- vars$PC1 * c(2.1, 1.7, 1.4)
vars$LabelY <- vars$PC2 * c(1.5, 2, 1.8)

ggplot() +
  geom_polygon(data = hulls,
               aes(x = PC1, y = PC2, fill = Standort),
               alpha = 0.2, show.legend = F) +
  geom_point(data = sites,
             aes(PC1, PC2, color = Standort),
             size = 3) +
  geom_text(data = sites,
            aes(PC1, PC2, label = Logger, color = Standort),
            vjust = -1, show.legend = F) +
  geom_segment(data = vars,
               aes(x = 0, y = 0,
                   xend = PC1 * 1.5,
                   yend = PC2 * 1.5),
               arrow = arrow(length = unit(0.25, "cm")),
               color = "black") +
  geom_text(data = vars,
            aes(x=LabelX, y=LabelY, label = Label),
            color = "black") +
  xlab(paste0("PC1 (", round(eig[1]*100,1), "%)")) +
  ylab(paste0("PC2 (", round(eig[2]*100,1), "%)")) +
  theme_bw() +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2))
