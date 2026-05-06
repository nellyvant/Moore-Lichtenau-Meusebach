# neu ordnen oder was auch immer meiner vegetaionsadaten, damit ich für jedes transekt eine matrix machen kann
data <- read.csv("Vegetationsaufnahmen_R - Kopie.csv")
str(data)

# 1 aerbwitsname zerlegen
library(dplyr)
library(stringr)

data2 <- data %>%
  mutate(
    start = str_sub(arbeitsname, 1, 1),
    end   = str_sub(arbeitsname, 2, 2)
  )
# 2 achse definieren
data2 <- data2 %>%
  mutate(
    axis = case_when(
      start %in% c("N", "S") | end %in% c("N", "S") ~ "NS",
      start %in% c("W", "O") | end %in% c("W", "O") ~ "WO",
      TRUE ~ NA_character_
    )
  )

# 3 position entlang der achse vereinheitlichen
data2 <- data2 %>%
  mutate(
    position = case_when(
      # West–Ost
      arbeitsname == "WM" ~ "W",
      arbeitsname == "MW" ~ "W",
      arbeitsname == "MM" ~ "M",
      arbeitsname == "MO" ~ "O",
      arbeitsname == "OM" ~ "O",
      
      # Nord–Süd
      arbeitsname == "NM" ~ "N",
      arbeitsname == "MN" ~ "N",
      arbeitsname == "MM" ~ "M",
      arbeitsname == "MS" ~ "S",
      arbeitsname == "SM" ~ "S",
      
      TRUE ~ NA_character_
    )
  )

# 4 reihenfolge festlegen
data2 <- data2 %>%
  mutate(
    pos_order = case_when(
      axis == "WO" ~ factor(position, levels = c("W", "M", "O")),
      axis == "NS" ~ factor(position, levels = c("N", "M", "S"))
    )
  )

# 5 sortieren
data_sorted <- data2 %>%
  arrange(site, axis, pos_order)
str(data_sorted)

write.csv(data_sorted, "data_sorted_test.csv", row.names = FALSE)




#neu

data2 <- data %>%
  mutate(
    code = str_sub(arbeitsname, 1, 2)  # <- entscheidend!
  )

data2 <- data2 %>%
  mutate(
    axis = case_when(
      str_detect(code, "[NS]") ~ "NS",
      str_detect(code, "[WO]") ~ "WO",
      TRUE ~ NA_character_
    ),
    
    position = case_when(
      axis == "WO" & str_detect(code, "W") ~ "W",
      axis == "WO" & str_detect(code, "O") ~ "O",
      axis == "WO" & str_detect(code, "M") ~ "M",
      
      axis == "NS" & str_detect(code, "N") ~ "N",
      axis == "NS" & str_detect(code, "S") ~ "S",
      axis == "NS" & str_detect(code, "M") ~ "M",
      
      TRUE ~ NA_character_
    )
  )

data2 <- data2 %>%
  mutate(
    pos_order = case_when(
      axis == "WO" ~ factor(position, levels = c("W", "M", "O")),
      axis == "NS" ~ factor(position, levels = c("N", "M", "S"))
    )
  )

write.csv(data_sorted, "data_sorted_test.csv", row.names = FALSE)


#####
#nochmal neu
data2 <- data %>%
  mutate(code = str_sub(arbeitsname, 1, 2))

data2 <- data2 %>%
  mutate(
    axis = case_when(
      code %in% c("NM", "MN", "SM", "MS", "SN", "NS") ~ "NS",
      code %in% c("WM", "MW", "MO", "OM", "OW", "WO") ~ "WO",
      TRUE ~ NA_character_
    )
  )


data2 <- data2 %>%
  mutate(
    position = case_when(
      
      # West-Ost Achse
      code %in% c("WM", "MW") ~ "W",
      code %in% c("MO", "OM") ~ "M",
      code %in% c("OW", "WO") ~ "O",
      
      # Nord-Süd Achse
      code %in% c("NM", "MN") ~ "N",
      code %in% c("MS", "SM") ~ "M",
      code %in% c("SN", "NS") ~ "S",
      
      TRUE ~ NA_character_
    )
  )


data_sorted <- data2 %>%
  mutate(
    pos_order = case_when(
      axis == "WO" ~ factor(position, levels = c("W", "M", "O")),
      axis == "NS" ~ factor(position, levels = c("N", "M", "S"))
    )
  ) %>%
  arrange(site, axis, pos_order)


table(data_sorted$axis, data_sorted$position)
write.csv(data_sorted, "data_sorted_test.csv", row.names = FALSE)


##### fortlaufende nummer ####
data2 <- data %>%
  mutate(
    code = str_sub(arbeitsname, 1, 2),
    transect_pos = as.numeric(str_extract(arbeitsname, "\\d+"))
  )

data2 <- data2 %>%
  mutate(
    axis = case_when(
      code %in% c("NM", "SM", "SN") ~ "NS",
      code %in% c("WM", "MO", "OW") ~ "WO"
    )
  )

data2 <- data2 %>%
  group_by(site, axis, code) %>%
  mutate(
    max_pos = max(transect_pos, na.rm = TRUE),
    
    transect_index = case_when(
      code %in% c("OW", "SN") ~ max_pos - transect_pos + 1,  # umdrehen
      TRUE ~ transect_pos
    )
  ) %>%
  ungroup()

write.csv(data2, "data_sorted_test.csv", row.names = FALSE)


##### nochmal fürs gesamte transekt #####


data2 <- data %>%
  mutate(
    code = str_sub(arbeitsname, 1, 2),
    pos = as.numeric(str_extract(arbeitsname, "\\d+"))
  )

data2 <- data2 %>%
  mutate(
    axis = case_when(
      code %in% c("WM", "MO", "OW") ~ "WO",
      code %in% c("NM", "MS", "SN") ~ "NS"
    ),
    
    segment = case_when(
      code %in% c("WM", "MW") ~ "W_M",
      code %in% c("MO", "OM") ~ "M_O",
      code %in% c("OW", "WO") ~ "O_W",
      
      code %in% c("NM", "MN") ~ "N_M",
      code %in% c("MS", "SM") ~ "M_S",
      code %in% c("SN", "NS") ~ "S_N"
    )
  )

segment_order <- c("W_M", "M_O", "O_W",
                   "N_M", "M_S", "S_N")

data2 <- data2 %>%
  mutate(
    segment = factor(segment, levels = segment_order)
  ) %>%
  arrange(site, axis, segment, pos)

data2 <- data2 %>%
  group_by(site, axis) %>%
  mutate(transect_id = row_number()) %>%
  ungroup()

write.csv(data2, "data_sorted_test.csv", row.names = FALSE)


##### 6.5.progress #####
#okay, ich habe die datei data_sorted_test jetzt einfach mal in excel bearbeitet und per hand eingetragen ein transekt NS udn WO und habe die transektnummer vergeben. die mitte bekommt dabei immer 0 und nach westen und norden werden die zahlen kleiner und nach osten und süden werden sie größer. 
#damit sollte ich mit szbset nach site, transekt filtern können wenn nötig. das macht die spalte name zwar etwas überflüssig, aber mein gott, ist halt jetzt so, muss ich am ende eben im readme zum skript erläutern

newdata <- read.csv("data_sorted_test.csv")
str(newdata)
