library(tidyverse)
library(fpp3)
library(BVAR)

# 2.1 The Data
liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)
library(fpp3)
liquor |>
  autoplot(sales) +
  labs(
    title = "US Liquor Store Retail Sales",
    y = "Sales (millions of dollars)",
    x = "Month"
  )

liquor |>
  gg_season(log(sales)) +
  labs(
    title = "Log of Liquor Store Sales (Seasonal)",
    y = "Log of Sales"
  )
# The trend of liquor store sales in the first graph shows an increase in liquor store sales since January 1992. In terms of seasonal pattern, liquor sales decrease during January and then increase slowly until July, then decrease a little bit and then increase rapidly from Nov-Dec. The seasonal swings do grow with the level of the series, showing that taking the log of the graph was a good idea. 

# 2.2 Fit Both Models
liquor_fit <- liquor |>
  model(arima = ARIMA(log(sales)),
        ets   = ETS(log(sales)))

liquor_fit |> select(arima) |> report()
liquor_fit |> select(ets) |> report()
# 