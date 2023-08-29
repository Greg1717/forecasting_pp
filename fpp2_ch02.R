library(fpp2)
library(ggplot2)

y <- ts(c(123,39,78,52,110), start = 2012)
y


z <- c(123,39,78,52,110)
y <- ts(z, start = 2003, frequency = 12)
y

data("melsyd")
?melsyd
str(melsyd)
head(melsyd)
tail(melsyd)
autoplot(melsyd[,"Economy.Class"]) +
  ggtitle("Economy class passengers: Melbourne-Sydney") +
  xlab("Year") +
  ylab("Thousands")


data(a10)
?a10
frequency(a10)
str(a10)
head(a10)
tail(a10)
autoplot(a10) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")


ggseasonplot(a10, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")


ggseasonplot(a10, polar=TRUE) +
  ylab("$ million") +
  ggtitle("Polar seasonal plot: antidiabetic drug sales")


ggsubseriesplot(a10) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")


data("elecdemand")
?elecdemand
str(elecdemand)
head(elecdemand)
tail(elecdemand)
autoplot(elecdemand[,c("Demand","Temperature")], facets = TRUE) +
  xlab("Year: 2014") + ylab("") +
  ggtitle("Half-hourly electricity demand: Victoria, Australia")



qplot(Temperature, Demand, data = as.data.frame(elecdemand)) +
  ylab("Demand (GW)") + xlab("Temperature (Celsius)")


data("visnights")
?visnights
str(visnights)
head(visnights)
tail(visnights)
autoplot(visnights[,1:5], facets = TRUE) +
  ylab("Number of visitor nights each quarter (millions)")


GGally::ggpairs(as.data.frame(visnights[,1:5]))

data("ausbeer")
?ausbeer
str(ausbeer)
head(ausbeer)
tail(ausbeer)
beer2 <- window(ausbeer, start = 1992)
gglagplot(beer2)


ggAcf(beer2)

data(elec)
?elec
str(elec)
head(elec)
tail(elec)
aelec <- window(elec, start = 1980)
autoplot(aelec) + xlab("Year") + ylab("GWh")


ggAcf(aelec, lag=48)


set.seed(30)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("White noise")


ggAcf(y)





