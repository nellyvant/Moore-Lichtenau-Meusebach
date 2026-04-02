#Vegetation Analysis

data <- read.csv("Vegetationsaufnahmen_R.csv")

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
summary(data[art_cols])
colSums(is.na(data[art_cols]))
data[art_cols][is.na(data[art_cols])] <- 0


str(data)

