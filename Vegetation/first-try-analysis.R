#Vegetation Analysis

#einlesen
#data <- read.csv("Vegetationsaufnahmen_R.csv")
#ersetzen der NA durch 0, bissche  umständlich weil viele zeilen character sind und wegen Dezimaltrennzeichen nicht in numeric umgewandelt werden
#art_cols <- grep("_", names(data), value = TRUE)

#unique(unlist(data[art_cols]))
#art_cols <- grep("_", names(data), value = TRUE)

#data[art_cols] <- lapply(data[art_cols], function(x) {
#  x <- as.character(x)
  
#  x <- trimws(x)        # Leerzeichen entfernen
#  x[x == ""] <- NA      # leere Zellen → NA
#  x <- gsub(",", ".", x) # Komma → Punkt
  
#  x <- as.numeric(x)    # jetzt funktioniert es
  
#  return(x)
#})
#überprüfen ob jetzt alles numeric ist
#summary(data[art_cols])
#str(data)

library(vegan)
library(ggplot2)
library(tidyverse)

#new try einlesen 16.4.26
#einlesen
data <- read.csv("Vegetationsaufnahmen_R - Kopie.csv")
str(data)

#name als rowname festelgen -> tabelle wird zur matrix
rownames(data) <- data$Name
data$Name <- NULL
ncol(data)

#longformat herstellen, etwas komliziert, da am ende immer die layer stehen soll, es gibt den seperator "_" aber der funktioniert zb bei Vaccinium_vitis_idaea_HL nicht weil es eben mehr Unterstriche gibt
# sep = "_(?=[^_]+$)") bedeutet Suche nach Unterstrich, (?...) ist ein lookahead, der bedingungen prüft und zwar ^_ heißt es kommt danach kein weiterer Unterstrich mehr und +$ heißt er schaut bis zum ende des string


art_cols <- grep("_", names(data), value = TRUE)
data_long <- data %>%
  rownames_to_column("Name") %>%
  pivot_longer(cols = all_of(art_cols),
               names_to = "Art_full",
               values_to = "Deckung") %>%
  
  # Schicht abtrennen (letzter Teil nach "_")
  separate(Art_full, into = c("Art", "Schicht"),
           sep = "_(?=[^_]+$)")



##### erste Analyse #####
richness <- rowSums(data > 0)
richness
mean(richness)

#deckung der layer in jedem Plot -> das sollte ich ja auch ungefähr auf papier haben, die berechnugn kann also später sicherlich raus
layer_cover <- data_long %>%
  group_by(plot.ID, Schicht) %>%
  summarise(Deckung_sum = sum(Deckung))




#transformation nach hellinger
library(vegan)

data_species <- data[art_cols]
row_sums <- rowSums(data_species); which(row_sums == 0)
data_species <- data_species[row_sums > 0, ]
data_hell <- decostand(data_species, method = "hellinger")

#pca

pca <- rda(data_hell)
plot(pca)



scores <- scores(pca, display = "sites")

scores_df <- as.data.frame(scores)
scores_df$Name <- rownames(scores_df)
scores_df$site <- data$site[match(scores_df$Name, data$Name)]

ggplot(scores_df, aes(PC1, PC2, color = site)) +
  geom_point(size = 3) +
  geom_text(aes(label = Name), size = 3, vjust = -1)

#dca
data_species <- data[art_cols]
row_sums <- rowSums(data_species); which(row_sums == 0)
data_species <- data_species[row_sums > 0, ]
dca <- decorana(data_species)
plot(dca)
dca
# Achsenlänge DCA1
diff(range(dca$rproj[,1]))

# Achsenlänge DCA2
diff(range(dca$rproj[,2]))


dca_scores <- scores(dca, display = "sites")
dca_df <- as.data.frame(dca_scores)
dca_df$Name <- rownames(dca_df)
dca_df$site <- data$site[match(dca_df$Name, data$Name)]
ggplot(dca_df, aes(x = DCA1, y = DCA2, color = site)) +
  geom_point(size = 3) +
  geom_text(aes(label = Name), size = 3, vjust = -1) +
  theme_minimal()



species_scores <- scores(dca, display = "species")
species_df <- as.data.frame(species_scores)
species_df$Art <- rownames(species_df)
species_df[order(abs(species_df$DCA1), decreasing = TRUE), ]



#next goal: zeigerwerte raussuchen und in die pca/dca als pfeile anzeigen lassen