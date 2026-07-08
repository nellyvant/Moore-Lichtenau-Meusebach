library(readxl)
library(ggplot2)
library(dplyr)
library(sf)


hum <- read_xlsx("Torfzersetzung.xlsx")
str(hum)

  
#hier werden Mitelwerte berechnet, obwohl die Skala eigentlich ordinal ist! also mit Vorsicht genießen! oder vlt lieber auf max oder min verlassen
hum_summary <- hum %>%
  filter(!is.na(hum_num)) %>%
  group_by(Punkt, site) %>%
  summarise(
    mean_hum = weighted.mean(hum_num, mächtigkeit),
    mean_hum_rounded = round(mean_hum),
    max_hum = max(hum_num),
    min_hum = min(hum_num),
    top_hum = hum_num[von == 0],
    torfmaechtigkeit = max(bis),
    .groups = "drop"
  )

#hum_summary %>%
 # group_by(site) %>%
  #summarise(
   # min_maechtigkeit = min(torfmaechtigkeit),
    #max_machtigkeit = max(torfmaechtigkeit),
    #area_min_hum = min(min_hum),
    #area_max_hum =max(max_hum),
    #area_mean_hum = mean())



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


hum_summary %>%
  ggplot(aes(x = factor(mean_hum_rounded),
             fill = factor(mean_hum_rounded))) +
  geom_bar() +
  facet_wrap(~site) +
  scale_fill_manual(values = hum_colors) +
  labs(
    x = "Humifizierungsgrad",
    y = "Anzahl",
    fill = "Hum"
  ) +
  theme_minimal()


#für die Plots der anderen Flächen ist MT durch MN oder L zu ersetzen

plot_data <- hum %>%
  filter(site == "mt" & !is.na(hum_num)) %>%
  mutate(
    Punkt = factor(Punkt),
    top = -von,
    bottom = -bis,
    hum_factor = factor(hum_num, names(hum_colors)
    )
  )
Hum_MT <- ggplot(plot_data) +
  geom_rect(aes(
    xmin = as.numeric(Punkt) - 0.4,
    xmax = as.numeric(Punkt) + 0.4,
    ymin = bottom,
    ymax = top,
    fill = hum_factor
  ),
  color = "black",
  show.legend =T,
  ) +
  
  scale_fill_manual(
    values = hum_colors,
    limits = names(hum_colors),
    breaks = names(hum_colors),
    name = "Humifizierung",
    drop = FALSE,
    labels = paste0("H", names(hum_colors)),
    na.translate = FALSE
  )+
  scale_x_continuous(
    breaks = seq_along(levels(plot_data$Punkt)),
    labels = stringr::str_wrap(
      levels(plot_data$Punkt),
      width = 12
    )
  ) +
  labs(
    x = "Bohrpunkt",
    y = "Tiefe [cm]"
  ) +
  
  theme_bw()

ggsave("hum_barplot_MT.png",Hum_MT,width = 30, height = 20, units = "cm")

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

#st_write(hum_sf, "hum.gpkg", delete_dsn = TRUE)
