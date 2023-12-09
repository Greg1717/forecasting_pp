
# 2 Time Series Graphics ======================================================
## 2.1 ts objects =============================================================
library(fpp2)
library(ggplot2)

y <- ts(c(123,39,78,52,110), start = 2012)
y

z <- c(123,39,78,52,110)
y <- ts(z, start = 2003, frequency = 12)
y

remove(y)
remove(z)

## 2.2 Time plots =============================================================
### melsyd ====================================================================
data("melsyd")
?melsyd
str(melsyd)
head(melsyd)
tail(melsyd)

#### autoplot =================================================================
autoplot(melsyd[,"Economy.Class"]) +
  ggtitle("Economy class passengers: Melbourne-Sydney") +
  xlab("Year") +
  ylab("Thousands")

remove(melsyd)

### a10 =======================================================================
data(a10)
?a10
frequency(a10)
str(a10)
frequency(a10)
head(a10)
tail(a10)

#### autoplot =================================================================
autoplot(a10) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")

#### ggseasonplot =============================================================
ggseasonplot(a10, year.labels = TRUE, year.labels.left = TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")

#### ggseasonplot polar =======================================================
ggseasonplot(a10, polar = TRUE) +
  ylab("$ million") +
  ggtitle("Polar seasonal plot: antidiabetic drug sales")

#### ggsubseriesplot ==========================================================
ggsubseriesplot(a10) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")

remove(a10)

### elecdemand =================================================================
data("elecdemand")
?elecdemand
str(elecdemand)
head(elecdemand)
tail(elecdemand)
frequency(elecdemand)

#### autoplot =================================================================
autoplot(elecdemand[,c("Demand","Temperature")], facets = TRUE) +
  xlab("Year: 2014") + ylab("") +
  ggtitle("Half-hourly electricity demand: Victoria, Australia")

#### qplot ====================================================================
qplot(Temperature, Demand, data = as.data.frame(elecdemand)) +
  ylab("Demand (GW)") + xlab("Temperature (Celsius)")

remove(elecdemand)

### visnights =================================================================
data("visnights")
?visnights
str(visnights)
head(visnights)
tail(visnights)
frequency(visnights)

#### autoplot =================================================================
autoplot(visnights[,1:5], facets = TRUE) +
  ylab("Number of visitor nights each quarter (millions)")

#### GGally ===================================================================
GGally::ggpairs(as.data.frame(visnights[,1:5]))

remove(visnights)

### ausbeer ===================================================================
data("ausbeer")
?ausbeer
str(ausbeer)
frequency(ausbeer)
head(ausbeer)
tail(ausbeer)

#### window() =================================================================
beer2 <- window(ausbeer, start = 1992)
remove(ausbeer)
#### gglagplot() ==============================================================
gglagplot(beer2)
?gglagplot
#### ggAcf() ==================================================================
?ggAcf
ggAcf(beer2)
remove(beer)
remove(beer2)

### elec ======================================================================
data(elec)
?elec
str(elec)
frequency(elec)
head(elec)
tail(elec)
#### window() =================================================================
aelec <- window(elec, start = 1980)
#### autoplot =================================================================
autoplot(aelec) + xlab("Year") + ylab("GWh")
#### ggAcf() ==================================================================
ggAcf(aelec, lag = 48)
remove(elec)
remove(aelec)
### autocorrelation zero on random noise ======================================
set.seed(30)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("White noise")
ggAcf(y)
remove(y)
