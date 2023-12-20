# 5 The forecaster's toolbox ==================================================


# 5.1 A tidy forecasting workflow =============================================

## Data preparation (tidy) =================
gdppc <- global_economy |>
  mutate(GDP_per_capita = GDP / Population)

## Plot the data (visualise) ===============

# GDP per capita data for Sweden from 1960 to 2017.
gdppc |>
  filter(Country == "Sweden") |>
  autoplot(GDP_per_capita) +
  labs(y = "$US", title = "GDP per capita for Sweden")

## Define a model (specify) ================

TSLM(GDP_per_capita ~ trend())

## Train the model (estimate) ==============

fit <- gdppc |>
  model(trend_model = TSLM(GDP_per_capita ~ trend()))

## Check model performance (evaluate) ======


## Produce forecasts (forecast) ============

fit |> forecast(h = "3 years")

fit |>
  forecast(h = "3 years") |>
  filter(Country == "Sweden") |>
  autoplot(gdppc) +
  labs(y = "$US", title = "GDP per capita for Sweden")



# 5.2 Some simple forecasting methods =========================================

bricks <- aus_production |>
  filter_index("1970 Q1" ~ "2004 Q4") |>
  select(Bricks)


## Mean method =============================

# Figure 5.3: Mean (or average) forecasts applied to clay brick production in Australia.
bricks |> model(MEAN(Bricks))


## Naïve method ============================

# Figure 5.4: Naïve forecasts applied to clay brick production in Australia.
bricks |> model(NAIVE(Bricks))


## Seasonal naïve method ===================

# Figure 5.5: Seasonal naïve forecasts applied to clay brick production in Australia.
bricks |> model(SNAIVE(Bricks ~ lag("year")))


## Drift method ============================
# Figure 5.6: Drift forecasts applied to clay brick production in Australia.
bricks |> model(RW(Bricks ~ drift()))



## Example: Australian quarterly beer production =========

# Figure 5.7 shows the first three methods applied to Australian quarterly beer production from 1992 to 2006, with the forecasts compared against actual values in the next 3.5 years.

# Set training data from 1992 to 2006
train <- aus_production |>
  filter_index("1992 Q1" ~ "2006 Q4")

# Fit the models
beer_fit <- train |>
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer)
  )

# Generate forecasts for 14 quarters
beer_fc <- beer_fit |> forecast(h = 14)

# Plot forecasts against actual values
beer_fc |>
  autoplot(train, level = NULL) +
  autolayer(
    filter_index(aus_production, "2007 Q1" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))


## Example: Google’s daily closing stock price =======
# Re-index based on trading days
google_stock <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) >= 2015) |>
  mutate(day = row_number()) |>
  update_tsibble(index = day, regular = TRUE)

# Filter the year of interest
google_2015 <- google_stock |> filter(year(Date) == 2015)

# Fit the models
google_fit <- google_2015 |>
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = NAIVE(Close ~ drift())
  )

# Produce forecasts for the trading days in January 2016
google_jan_2016 <- google_stock |>
  filter(yearmonth(Date) == yearmonth("2016 Jan"))
google_fc <- google_fit |>
  forecast(new_data = google_jan_2016)

# Plot the forecasts
google_fc |>
  autoplot(google_2015, level = NULL) +
  autolayer(google_jan_2016, Close, colour = "black") +
  labs(y = "$US",
       title = "Google daily closing stock prices",
       subtitle = "(Jan 2015 - Jan 2016)") +
  guides(colour = guide_legend(title = "Forecast"))




# 5.3 Fitted values and residuals =============================================

# The fitted values and residuals from a model can be obtained using the augment() function. In the beer production example in Section 5.2, we saved the fitted models as beer_fit. So we can simply apply augment() to this object to compute the fitted values and residuals for all models.

augment(beer_fit)


# 5.4 Residual diagnostics ====================================================
# A good forecasting method will yield innovation residuals with the following properties:
# The innovation residuals are uncorrelated. If there are correlations between innovation residuals, then there is information left in the residuals which should be used in computing forecasts.
# The innovation residuals have zero mean. If they have a mean other than zero, then the forecasts are biased.

# In addition to these essential properties, it is useful (but not necessary) for the residuals to also have the following two properties.
# The innovation residuals have constant variance. This is known as “homoscedasticity”.
# The innovation residuals are normally distributed.


## Example: Forecasting Google daily closing stock prices ======
# Figure 5.9: Daily Google stock prices in 2015
autoplot(google_2015, Close) +
  labs(y = "$US",
       title = "Google daily closing stock prices in 2015")

aug <- google_2015 |>
  model(NAIVE(Close)) |>
  augment()

autoplot(aug, .innov) +
  labs(y = "$US",
       title = "Residuals from the naïve method")

# Histogram of the residuals from the naïve method applied to the Google stock price. The right tail seems a little too long for a normal distribution.
aug |>
  ggplot(aes(x = .innov)) +
  geom_histogram() +
  labs(title = "Histogram of residuals")

# Figure 5.12: ACF of the residuals from the naïve method applied to the Google stock price. The lack of correlation suggesting the forecasts are good.
aug |>
  ACF(.innov) |>
  autoplot() +
  labs(title = "Residuals from the naïve method")


# A convenient shortcut for producing these residual diagnostic graphs is the gg_tsresiduals() function, which will produce a time plot, ACF plot and histogram of the residuals.

# Figure 5.13: Residual diagnostic graphs for the naïve method applied to the Google stock price.
google_2015 |>
  model(NAIVE(Close)) |>
  gg_tsresiduals()

## Portmanteau tests for autocorrelation

aug |> features(.innov, box_pierce, lag = 10)


aug |> features(.innov, ljung_box, lag = 10)

# For both  Q and Q∗, the results are not significant (i.e., the p-values are relatively large). Thus, we can conclude that the residuals are not distinguishable from a white noise series.


# An alternative simple approach that may be appropriate for forecasting the Google daily closing stock price is the drift method. The tidy() function shows the one estimated parameter, the drift coefficient, measuring the average daily change observed in the historical data.
fit <- google_2015 |> model(RW(Close ~ drift()))
tidy(fit)

# Applying the Ljung-Box test, we obtain the following result.
augment(fit) |> features(.innov, ljung_box, lag=10)

# As with the naïve method, the residuals from the drift method are indistinguishable from a white noise series.



# 5.5 Distributional forecasts and prediction intervals =======================

# Prediction intervals can easily be computed for you when using the fable package. For example, here is the output when using the naïve method for the Google stock price.

## Benchmark methods =======================
google_2015 |>
  model(NAIVE(Close)) |>
  forecast(h = 10) |>
  hilo()

google_2015 |>
  model(NAIVE(Close)) |>
  forecast(h = 10) |>
  autoplot(google_2015) +
  labs(title="Google daily closing stock price", y="$US" )



## Prediction intervals from bootstrapped residuals =========

fit <- google_2015 |>
  model(NAIVE(Close))
sim <- fit |> generate(h = 30, times = 5, bootstrap = TRUE)
sim
# Here we have generated five possible sample paths for the next 30 trading days. The .rep variable provides a new key for the tsibble. The plot below shows the five sample paths along with the historical data.

# Figure 5.15: Five simulated future sample paths of the Google closing stock price based on a naïve method with bootstrapped residuals.


google_2015 |>
  ggplot(aes(x = day)) +
  geom_line(aes(y = Close)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)),
            data = sim) +
  labs(title="Google daily closing stock price", y="$US" ) +
  guides(colour = "none")

# Then we can compute prediction intervals by calculating percentiles of the future sample paths for each forecast horizon. The result is called a bootstrapped prediction interval. The name “bootstrap” is a reference to pulling ourselves up by our bootstraps, because the process allows us to measure future uncertainty by only using the historical data.

# This is all built into the forecast() function so you do not need to call generate() directly.

fc <- fit |> forecast(h = 30, bootstrap = TRUE)
fc

# Figure 5.16: Forecasts of the Google closing stock price based on a naïve method with bootstrapped residuals.
autoplot(fc, google_2015) +
  labs(title="Google daily closing stock price", y="$US" )


# The number of samples can be controlled using the times argument for forecast(). For example, intervals based on 1000 bootstrap samples can be sampled with:

google_2015 |>
  model(NAIVE(Close)) |>
  forecast(h = 10, bootstrap = TRUE, times = 1000) |>
  hilo()


# 5.6 Forecasting using transformations =======================================



# 5.7 Forecasting with decomposition ==========================================



# 5.8 Evaluating point forecast accuracy ======================================



# 5.9 Evaluating distributional forecast accuracy =============================



# 5.10 Time series cross-validation ===========================================