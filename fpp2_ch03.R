library(fpp2)

# 3.1 Some simple forecasting methods =========================================

# meanf(y, h)

# y contains the time series
# h is the forecast horizon

# naive(y, h)
# rwf(y, h) # Equivalent alternative


# snaive(y, h)


# rwf(y, h, drift = TRUE)

# Set training data from 1992 to 2007
beer2 <- window(ausbeer,start = 1992,end = c(2007,4))

# Plot some forecasts
# the first three methods applied to the quarterly beer production data.
autoplot(beer2) +
  autolayer(meanf(beer2, h = 11),
            series = "Mean", PI = FALSE) +
  autolayer(naive(beer2, h = 11),
            series = "Naïve", PI = FALSE) +
  autolayer(snaive(beer2, h = 11),
            series = "Seasonal naïve", PI = FALSE) +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour = guide_legend(title = "Forecast"))


# non-seasonal methods are applied to a series of 200 days of the Google daily closing stock price.

autoplot(goog200) +
  autolayer(meanf(goog200, h=40),
            series="Mean", PI=FALSE) +
  autolayer(rwf(goog200, h=40),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(goog200, drift=TRUE, h=40),
            series="Drift", PI=FALSE) +
  ggtitle("Google stock (daily ending 6 Dec 2013)") +
  xlab("Day") + ylab("Closing Price (US$)") +
  guides(colour=guide_legend(title="Forecast"))




# 3.2 Transformations and adjustments =========================================

## Calendar adjustments ========

dframe <- cbind(Monthly = milk,
                DailyAverage = milk/monthdays(milk))

autoplot(dframe, facet=TRUE) +
  xlab("Years") + ylab("Pounds") +
  ggtitle("Milk production per cow")


## Population adjustments ========

# per head

## Inflation adjustments =========



## Mathematical transformations =========
# If the data show variation that increases or decreases with the level of the series, then a transformation can be useful.

(lambda <- BoxCox.lambda(elec))
#> [1] 0.2654
autoplot(BoxCox(elec,lambda))


## Bias adjustments =========

# One issue with using mathematical transformations such as Box-Cox transformations is that the back-transformed point forecast will not be the mean of the forecast distribution. In fact, it will usually be the median of the forecast distribution (assuming that the distribution on the transformed space is symmetric).

fc <- rwf(
  eggs,
  drift = TRUE,
  lambda = 0,
  h = 50,
  level = 80
)

fc2 <- rwf(
  eggs,
  drift = TRUE,
  lambda = 0,
  h = 50,
  level = 80,
  biasadj = TRUE
)

autoplot(eggs) +
  autolayer(fc, series="Simple back transformation") +
  autolayer(fc2, series="Bias adjusted", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast"))



# 3.3 Residual diagnostics ====================================================

# Fitted values: Each observation in a time series can be forecast using all previous observations.

# Residuals: The “residuals” in a time series model are what is left over after fitting a model. For many (but not all) time series models, the residuals are equal to the difference between the observations and the corresponding fitted values

autoplot(goog200) +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google Stock (daily ending 6 December 2013)")

res <- residuals(naive(goog200))

# plot residuals
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")

gghistogram(res) + ggtitle("Histogram of residuals")

ggAcf(res) + ggtitle("ACF of residuals")

## Portmanteau tests for autocorrelation =======

# lag=h and fitdf=K
Box.test(res, lag=10)


Box.test(res,lag=10, type="Lj")


checkresiduals(naive(goog200))

# 3.4 Evaluating forecast accuracy ============================================
## Training and test sets =====================================================
## Functions to subset a time series ==========================================

window(ausbeer, start=1995)

subset(ausbeer, start=length(ausbeer)-4*5)

subset(ausbeer, quarter = 1)

tail(ausbeer, 4*5)



## Forecast errors ============
# A forecast “error” is the difference between an observed value and its forecast.

## Scale-dependent errors =======
#    Mean absolute error: MAE
#    Root mean squared error: RMSE

## Percentage errors: Mean absolute percentage error: MAPE ======

## Scaled errors: MASE ======

beer2 <- window(ausbeer,start=1992,end=c(2007,4))

beerfit1 <- meanf(beer2,h=10)

beerfit2 <- rwf(beer2,h=10)

beerfit3 <- snaive(beer2,h=10)

autoplot(window(ausbeer, start=1992)) +
  autolayer(beerfit1, series="Mean", PI=FALSE) +
  autolayer(beerfit2, series="Naïve", PI=FALSE) +
  autolayer(beerfit3, series="Seasonal naïve", PI=FALSE) +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Forecasts for quarterly beer production") +
  guides(colour=guide_legend(title="Forecast"))

beer3 <- window(ausbeer, start=2008)
accuracy(beerfit1, beer3)
accuracy(beerfit2, beer3)
accuracy(beerfit3, beer3)


googfc1 <- meanf(goog200, h=40)
googfc2 <- rwf(goog200, h=40)
googfc3 <- rwf(goog200, drift=TRUE, h=40)
autoplot(subset(goog, end = 240)) +
  autolayer(googfc1, PI=FALSE, series="Mean") +
  autolayer(googfc2, PI=FALSE, series="Naïve") +
  autolayer(googfc3, PI=FALSE, series="Drift") +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google stock price (daily ending 6 Dec 13)") +
  guides(colour=guide_legend(title="Forecast"))

googtest <- window(goog, start=201, end=240)
accuracy(googfc1, googtest)
accuracy(googfc2, googtest)
accuracy(googfc3, googtest)

## Time series cross-validation ========

e <- tsCV(goog200, rwf, drift=TRUE, h=1)
sqrt(mean(e^2, na.rm=TRUE))
#> [1] 6.233
sqrt(mean(residuals(rwf(goog200, drift=TRUE))^2, na.rm=TRUE))
#> [1] 6.169


## Pipe operator ======


goog200 %>% tsCV(forecastfunction = rwf,
                 drift = TRUE,
                 h = 1) -> e

e ^ 2 %>%
  mean(na.rm = TRUE) %>%
  sqrt()

goog200 %>%
  rwf(drift = TRUE) %>%
  residuals() -> res

res ^ 2 %>%
  mean(na.rm = TRUE) %>%
  sqrt()


e <- tsCV(goog200, forecastfunction = naive, h = 8)
# Compute the MSE values and remove missing values
mse <- colMeans(e ^ 2, na.rm = T)
# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()



# 3.5 Prediction intervals ====================================================

autoplot(naive(goog200))

naive(goog200, bootstrap = TRUE)


# 3.6 The forecast package in R ===============================================

forecast(ausbeer, h=4)




