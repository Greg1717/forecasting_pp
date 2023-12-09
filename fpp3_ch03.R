library(fpp3)
# library(tsibble)
# library(tsibbledata)
# library(magrittr)
# library(dplyr)
# library(feasts)
# library(lubridate)
# library(tidyr)

data(global_economy)
?global_economy
str(global_economy)
head(global_economy)
tail(global_economy)
global_economy[global_economy$Year == 2012,]

# 3.1 Transformations and adjustments =========================================

## Calendar adjustments =======================================================

## Population adjustments =====================================================
global_economy |>
  filter(Country == "Australia") |>
  autoplot(GDP/Population) +
  labs(title = "GDP per capita", y = "$US")


## Inflation adjustments ======================================================
aus_retail
?aus_retail
str(aus_retail)
head(aus_retail)
tail(aus_retail)

print_retail <- aus_retail |>
  filter(Industry == "Newspaper and book retailing") |>
  group_by(Industry) |>
  index_by(Year = year(Month)) |>
  summarise(Turnover = sum(Turnover))

print_retail

aus_economy <- global_economy |>
  filter(Code == "AUS")

# merge economic indicators into dataset, so that we can revalue turnover by using the Consumer Price Index
print_retail
aus_economy

print_retail |>
  left_join(aus_economy, by = "Year") |>
  mutate(Adjusted_turnover = Turnover / CPI * 100)

print_retail |>
  left_join(aus_economy, by = "Year") |>
  mutate(Adjusted_turnover = Turnover / CPI * 100) |>
  pivot_longer(c(Turnover, Adjusted_turnover),
               values_to = "Turnover")

print_retail |>
  left_join(aus_economy, by = "Year") |>
  mutate(Adjusted_turnover = Turnover / CPI * 100) |>
  pivot_longer(c(Turnover, Adjusted_turnover),
               values_to = "Turnover") |>
  mutate(name = factor(name,
                       levels = c("Turnover","Adjusted_turnover"))) |>
  ggplot(aes(x = Year, y = Turnover)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(title = "Turnover: Australian print media industry",
       y = "$AU")

## intro to features() ========================================================
tourism
?features
?features_by_pkg
?features_by_tag
tourism
tourism %>%
  features(Trips, features = list(mean = mean, sd = sd))

## Mathematical transformations ===============================================
data("aus_production")
aus_production
?pull
lambda <- aus_production |>
  features(Gas, features = guerrero) |>
  pull(lambda_guerrero)

aus_production |>
  autoplot(box_cox(Gas, lambda)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "Transformed gas production with $\\lambda$ = ",
         round(lambda,2))))


# 3.2 Time series components ==================================================
us_retail_employment <-
  us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)

us_retail_employment

autoplot(us_retail_employment, Employed) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

dcmp <- us_retail_employment |>
  model(stl = STL(Employed))

components(dcmp)


components(dcmp) |>
  as_tsibble() |>
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = trend), colour = "#D55E00") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

### components() ======
# TS decomposition into trend, season, remainder
components(dcmp) |>
  autoplot()

### Seasonally adjusted data ======
# If the seasonal component is removed from the original data, the resulting values are the “seasonally adjusted” data.

components(dcmp) |>
  as_tsibble() |>
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y=season_adjust), colour = "#0072B2") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")


# 3.3 Moving averages =========================================================
global_economy |>
  filter(Country == "Australia") |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Total Australian exports")

## Moving average smoothing ==============================
library(slider)
aus_exports <- global_economy |>
  filter(Country == "Australia") |>
  mutate(
    `5-MA` = slider::slide_dbl(Exports,
                               mean,
                               .before = 2,
                               .after = 2,
                               .complete = TRUE)
  )

aus_exports |>
  autoplot(Exports) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  labs(y = "% of GDP",
       title = "Total Australian exports") +
  guides(colour = guide_legend(title = "series"))

## Moving averages of moving averages =============
beer <- aus_production |>
  filter(year(Quarter) >= 1992) |>
  select(Quarter, Beer)

beer_ma <- beer |>
  mutate(
    `4-MA` = slider::slide_dbl(Beer, mean,
                               .before = 1, .after = 2, .complete = TRUE),
    `2x4-MA` = slider::slide_dbl(`4-MA`, mean,
                                 .before = 1, .after = 0, .complete = TRUE)
  )


## Estimating the trend-cycle with seasonal data =======
us_retail_employment_ma <-
  us_retail_employment |>
  mutate(
    `12-MA` = slider::slide_dbl(Employed, mean,
                                .before = 5, .after = 6, .complete = TRUE),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                                  .before = 1, .after = 0, .complete = TRUE)
  )

us_retail_employment_ma

us_retail_employment_ma |>
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")


# 3.4 Classical decomposition =================================================

## Additive decomposition =======
# A classical additive decomposition of US retail employment.
us_retail_employment |>
  model(
    classical_decomposition(Employed, type = "additive")
  ) |>
  components() |>
  autoplot() +
  labs(title = "Classical additive decomposition of total
                  US retail employment")


## Multiplicative decomposition =======

# 3.5 Methods used by official statistics agencies ============================

## X-11 method ========
# A multiplicative decomposition of US retail employment using X-11.
x11_dcmp <- us_retail_employment |>
  model(x11 = X_13ARIMA_SEATS(Employed ~ x11())) |>
  components()

autoplot(x11_dcmp) +
  labs(title =
         "Decomposition of total US retail employment using X-11.")


# US retail employment: the original data (grey), the trend-cycle component (orange) and the seasonally adjusted data (barely visible in blue).

x11_dcmp |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Employed, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )


# Seasonal sub-series plot of the seasonal component from the X-11 method applied to total US retail employment.
x11_dcmp |>
  gg_subseries(seasonal)

## SEATS method ==========================================

# A decomposition of US retail employment obtained using SEATS.

seats_dcmp <- us_retail_employment |>
  model(seats = X_13ARIMA_SEATS(Employed ~ seats())) |>
  components()

autoplot(seats_dcmp) +
  labs(title =
         "Decomposition of total US retail employment using SEATS")


# 3.6 STL decomposition =======================================================

# Total US retail employment (top) and its three additive components obtained from a robust STL decomposition with flexible trend-cycle and fixed seasonality.
us_retail_employment |>
  model(
    STL(Employed ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) |>
  components() |>
  autoplot()

