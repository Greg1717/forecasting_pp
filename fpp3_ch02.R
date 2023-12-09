
library(fpp3)
# 2.1 Tsibble Objects =========================================================

## The index variable ==========================================================
z <- tsibble(
  Year = 2015:2019,
  Observation = c(123, 39, 78, 52, 110),
  index = Year
)

z

# z |>
#   mutate(Month = yearmonth(Month)) |>
#   as_tsibble(index = Month)

# Other time class functions can be used depending on the frequency of the observations.
# Frequency	Function
# Annual	start:end
# Quarterly	yearquarter()
# Monthly	yearmonth()
# Weekly	yearweek()
# Daily	as_date(), ymd()
# Sub-daily	as_datetime(), ymd_hms()

olympic_running
olympic_running |> distinct(Sex)

## Working with tsibble objects ================================================
PBS

PBS |>
  filter(ATC2 == "A10")

PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost)

PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost))

PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost)) |>
  mutate(Cost = TotalC/1e6)

PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost)) |>
  mutate(Cost = TotalC / 1e6) -> a10


## Read a csv file and convert to a tsibble ====================================

prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")

prison <- prison |>
  mutate(Quarter = yearquarter(Date)) |>
  select(-Date) |>
  as_tsibble(key = c(State, Gender, Legal, Indigenous),
             index = Quarter)

prison

# 2.2 Time Plots ==============================================================
ansett

melsyd_economy <- ansett |>
  filter(Airports == "MEL-SYD", Class == "Economy") |>
  mutate(Passengers = Passengers/1000)

melsyd_economy

autoplot(melsyd_economy, Passengers) +
  labs(title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney",
       y = "Passengers ('000)")

autoplot(a10, Cost) +
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales")

# 2.3 Time series patterns ====================================================

# 2.4 Seasonal plots ==========================================================

a10 |>
  gg_season(Cost, labels = "both") +
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales")

## Multiple seasonal periods ========

# Seasonal plot showing daily seasonal patterns for Victorian electricity demand.
vic_elec |> gg_season(Demand, period = "day") +
  theme(legend.position = "none") +
  labs(y="MWh", title="Electricity demand: Victoria")

# Seasonal plot showing weekly seasonal patterns for Victorian electricity demand.
vic_elec |> gg_season(Demand, period = "week") +
  theme(legend.position = "none") +
  labs(y="MWh", title="Electricity demand: Victoria")

# Seasonal plot showing yearly seasonal patterns for Victorian electricity demand.
vic_elec |> gg_season(Demand, period = "year") +
  labs(y="MWh", title="Electricity demand: Victoria")


# 2.5 Seasonal subseries plots ================================================

# Seasonal subseries plot of monthly antidiabetic drug sales in Australia.
a10 |>
  gg_subseries(Cost) +
  labs(
    y = "$ (millions)",
    title = "Australian antidiabetic drug sales"
  )

# Example: Australian holiday tourism
holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

holidays

# Time plots of Australian domestic holidays by state.
autoplot(holidays, Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")

# Season plots of Australian domestic holidays by state.
gg_season(holidays, Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")

# Subseries plots of Australian domestic holidays by state.
holidays |>
  gg_subseries(Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")


# 2.6 Scatterplots ============================================================

# Half hourly electricity demand in Victoria, Australia, for 2014.
vic_elec |>
  filter(year(Time) == 2014) |>
  autoplot(Demand) +
  labs(y = "GW",
       title = "Half-hourly electricity demand: Victoria")


# Half hourly temperature in Melbourne, Australia, for 2014.
vic_elec |>
  filter(year(Time) == 2014) |>
  autoplot(Temperature) +
  labs(
    y = "Degrees Celsius",
    title = "Half-hourly temperatures: Melbourne, Australia"
  )

# We can study the relationship between demand and temperature by plotting one series against the other.

vic_elec |>
  filter(year(Time) == 2014) |>
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  labs(x = "Temperature (degrees Celsius)",
       y = "Electricity demand (GW)")

## Scatterplot matrices =======================================================

visitors <- tourism |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

visitors

# Quarterly visitor nights for the states and territories of Australia.
visitors |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y") +
  labs(title = "Australian domestic tourism",
       y= "Overnight trips ('000)")

# To see the relationships between these eight time series, we can plot each time series against the others. These plots can be arranged in a scatterplot matrix, as shown in Figure 2.18. (This plot requires the GGally package to be installed.)

# A scatterplot matrix of the quarterly visitor nights in the states and territories of Australia.

visitors |>
  pivot_wider(values_from=Trips, names_from=State) |>
  GGally::ggpairs(columns = 2:9)


# 2.7 Lag plots ===============================================================

# the horizontal axis shows lagged values of the time series.
# Lagged scatterplots for quarterly beer production:
recent_production <- aus_production |>
  filter(year(Quarter) >= 2000)

recent_production

recent_production |>
  gg_lag(Beer, geom = "point") +
  labs(x = "lag(Beer, k)")


# 2.8 Autocorrelation =========================================================

# Just as correlation measures the extent of a linear relationship between two variables, autocorrelation measures the linear relationship between lagged values of a time series.

# There are several autocorrelation coefficients, corresponding to each panel in the lag plot.
recent_production |> ACF(Beer, lag_max = 9)

recent_production |>
  ACF(Beer) |>
  autoplot() + labs(title="Australian beer production")

## Trend and seasonality in ACF plots ========

# ACF of monthly Australian antidiabetic drug sales.
a10 |>
  ACF(Cost, lag_max = 48) |>
  autoplot() +
  labs(title="Australian antidiabetic drug sales")


# 2.9 White noise =============================================================

# Time series that show no autocorrelation are called white noise.

# A white noise time series:
set.seed(30)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
y |> autoplot(wn) + labs(title = "White noise", y = "")

y |>
  ACF(wn) |>
  autoplot() + labs(title = "White noise")
