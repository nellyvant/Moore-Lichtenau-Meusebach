#Logger Auswertung Temp & Licht
# am bsp Lichtenau Mitte

#daten einlesen
data<-read.csv("Lichtenau Mitte 2025-11-04 18_39_12 CET.csv")
str(data)
#package für datumsumwandlung
install.packages("lubridate")
library(lubridate)
data$Date.Time..CEST.CET. <- mdy_hms(data$Date.Time..CEST.CET.)
str(data)

#plot der gesamten zeitreihe
plot(
  data$Date.Time..CEST.CET.,
  data$Temperature.....C.,
  type = "l",      # "p" = Punkte, "l" = Linie, "b" = beides
  xlab = "Datum",
  ylab = "Wert"
)
min(data$Temperature.....C., na.rm =TRUE)
max(data$Temperature.....C., na.rm=TRUE)
#omin und imax als tag mit der extremtemperatur
i_min <- which.min(data$Temperature.....C.)
i_max <- which.max(data$Temperature.....C.)

data$Date.Time..CEST.CET.[i_min]
data$Date.Time..CEST.CET.[i_max]

#plot des tages mit der heißesten temperatur
tag <- as.Date("2025-07-02")

data_tag <- data[as.Date(data$Date.Time..CEST.CET.) == tag, ]

plot(
  data_tag$Date,
  data_tag$Temperature.....C.,
  type = "b"
)


#mitteltemperatur pro tag
tagesmittel <- aggregate(
  Temperature.....C. ~ as.Date(Date.Time..CEST.CET.),
  data = data,
  FUN = mean,
  na.rm = TRUE
)

colnames(tagesmittel) <- c("tag", "temp_mittel")
tagesmittel
plot(
  tagesmittel$tag, tagesmittel$temp_mittel,
  type = "b"
  )
