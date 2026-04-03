#Vegetation Analysis

#einlesen
data <- read.csv("Vegetationsaufnahmen_R.csv")
#ersetzen der NA durch 0, bissche  umständlich weil viele zeilen character sind und wegen Dezimaltrennzeichen nicht in numeric umgewandelt werden
art_cols <- grep("_", names(data), value = TRUE)

unique(unlist(data[art_cols]))
art_cols <- grep("_", names(data), value = TRUE)

data[art_cols] <- lapply(data[art_cols], function(x) {
  x <- as.character(x)
  
  x <- trimws(x)        # Leerzeichen entfernen
  x[x == ""] <- NA      # leere Zellen → NA
  x <- gsub(",", ".", x) # Komma → Punkt
  
  x <- as.numeric(x)    # jetzt funktioniert es
  
  return(x)
})
#überprüfen ob jetzt alles numeric ist
summary(data[art_cols])
str(data)

#name als rowname festelgen -> tabelle wird zur matrix
rownames(data) <- data$Name
data$Name <- NULL


#longformat herstellen, etwas komliziert, da am ende immer die layer stehen soll, es gibt den seperator "_" aber der funktioniert zb bei Vaccinium_vitis_idaea_HL nicht weil es eben mehr Unterstriche gibt
# sep = "_(?=[^_]+$)") bedeutet Suche nach Unterstrich, (?...) ist ein lookahead, der bedingungen prüft und zwar ^_ heißt es kommt danach kein weiterer Unterstrich mehr und +$ heißt er schaut bis zum ende des string

library(tidyverse)

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
data_hell <- decostand(data_species, method = "hellinger")

#pca

pca <- rda(data_hell)
plot(pca)

#dca
dca <- decorana(data_species)
plot(dca)

#cluster analyse
dist_matrix <- vegdist(data_hell, method = "bray")
cluster <- hclust(dist_matrix)

plot(cluster)
