library(readxl)
library(ggplot2)
library(dplyr)
library(sf)


hum <- read_xlsx("Torfzersetzung.xlsx")
str(hum)
hum_colors <- c(
  "1"  = "#ffffcc",
  "2"  = "#ffeda0",
  "3"  = "#fed976",
  "4"  = "#feb24c",
  "5"  = "#fd8d3c",
  "6"  = "#fc4e2a",
  "7"  = "#e31a1c",
  "8"  = "#bd0026",
  "9"  = "#800026",
  "10" = "#4d0018"
)




plot_data <- hum %>%
  filter(site == "l") %>%
  mutate(
    Punkt = factor(Punkt),
    top = -von,
    bottom = -bis,
    hum_factor = factor(hum_num, levels = 1:10)
  )
ggplot(plot_data) +
  geom_rect(aes(
    xmin = as.numeric(Punkt) - 0.4,
    xmax = as.numeric(Punkt) + 0.4,
    ymin = bottom,
    ymax = top,
    fill = hum_factor
  ),
  color = "black"
  ) +
  
  scale_fill_manual(
    values = hum_colors,
    name = "Humifizierung",
    labels = paste0("H", 1:10),
    na.translate = FALSE
  )+
  scale_x_continuous(
    breaks = seq_along(levels(plot_data$Punkt)),
    labels = levels(plot_data$Punkt)
  ) +
  labs(
    x = "Bohrpunkt",
    y = "Tiefe [cm]"
  ) +
  
  theme_bw()

#hier werden Mitelwerte berechnet, obwohl die Skala eigentlich ordinal ist! also mit Vorsicht genießen! oder vlt lieber auf max oder min verlassen
hum_summary <- hum %>%
  filter(!is.na(hum_num)) %>%
  group_by(Punkt, site, transekt) %>%
  summarise(
    mean_H = weighted.mean(hum_num, mächtigkeit),
    mean_H_rounded = round(mean_H),
    max_H = max(hum_num),
    min_H = min(hum_num),
    torfmaechtigkeit = max(bis),
    .groups = "drop"
  )

#hier wird BOR4 entfernt weil NA in humifizierungsgrad ist. das war die Bohrung an der Spundwand in Meu trock und dort gab es ja keinen Torf, sondern nur Sand aka das Verfüllungsmaterial für die Plombe -> im methodenteil erwähnen


points <- st_read("bohrer-plus-logger.gpkg")
str(points)

#join points$name mit hum$Punkt
hum_sf <- hum_summary %>%
  left_join(
    points %>%
      select(name, ele, time, geom),
    by = c("Punkt" = "name")
  ) %>%
  st_as_sf()

st_write(hum_sf, "hum.gpkg", delete_dsn = TRUE)
