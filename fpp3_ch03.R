library(fpp3)
# library(tsibble)
# library(tsibbledata)
# library(magrittr)
# library(dplyr)
# library(feasts)
# library(lubridate)
# library(tidyr)

data(global_economy)
str(global_economy)


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
tail(aus_retail)

print_retail <- aus_retail |>
  filter(Industry == "Newspaper and book retailing") |>
  group_by(Industry) |>
  index_by(Year = year(Month)) |>
  summarise(Turnover = sum(Turnover))

print_retail

aus_economy <- global_economy |>
  filter(Code == "AUS")


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


## Mathematical transformations ===============================================

data("aus_production")
aus_production

?features
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


autoplot(us_retail_employment, Employed) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")


