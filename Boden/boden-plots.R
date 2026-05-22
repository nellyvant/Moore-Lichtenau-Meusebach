library(readxl)
data <- read_xlsx("Torfzersetzung.xlsx")
str(data)
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


library(ggplot2)
library(dplyr)

plot_data <- data %>%
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
    name = "Humifizierung"
  ) +
  
  scale_x_continuous(
    breaks = 1:length(levels(plot_data$Punkt)),
    labels = levels(plot_data$Punkt)
  ) +
  
  facet_grid(site ~ transekt, scales = "free_x", space = "free_x") +
  
  labs(
    x = "Bohrpunkt",
    y = "Tiefe [cm]"
  ) +
  
  theme_bw()





plot_data <- data %>%
  filter(site == "l", transekt %in% c("wo", "beide")) %>%
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

levels(plot_data$hum_factor)
names(hum_colors)
