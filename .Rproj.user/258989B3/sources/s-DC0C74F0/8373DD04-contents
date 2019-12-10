### Experimenting with fable

usethis::use_directory('data')

library(tidyverse)
library(tsibble)
library(lubridate)
library(fable)

raw_data <- read_csv('/Users/tomasjanik/Dropbox/datasets/time_series.csv')

## convert raw data to tsibble object with proper key, index values
raw_tsibble <- raw_data %>%
    mutate(fiscal_period = yq(fiscal_period)) %>%
    as_tsibble(., key = c(dealrange, geo, region, subregion),
               fiscal_period) %>%
    mutate(fiscal_period = yearquarter(fiscal_period))

geos <- raw_tsibble %>%
    filter(dealrange == 'COMPUTE',
           region == 'ALL',
           year(fiscal_period) <= 2019)

fit <- geos %>%
    model(
        snaive = SNAIVE(revenue ~ lag("year")),
        ets = ETS(revenue),
        arima = ARIMA(revenue)
    )

fc <- fit %>%
    forecast(h = "1 year")
fc

fc %>%
    filter(dealrange == "COMPUTE") %>%
    autoplot(geos, level = NULL) +
    xlab("Year") + ylab("Revenue (millions)")

## some accuracy
train <- geos %>%
    filter(year(fiscal_period) <= 2018)

fit <- train %>%
    model(
        ets = ETS(revenue),
        arima = ARIMA(revenue),
        snaive = SNAIVE(revenue)
    ) %>%
    mutate(mixed = (ets + arima + snaive) / 3)

fc <- fit %>% 
    forecast(h = "1 year")

fc %>%
    filter(dealrange == "COMPUTE") %>%
    autoplot(geos, level = NULL)

fc %>% accuracy(., geos)

fc_accuracy <- accuracy(fc, geos,
                        measures = list(
                            point_accuracy_measures,
                            interval_accuracy_measures,
                            distribution_accuracy_measures
                        )
)

fc_accuracy %>%
    group_by(.model) %>%
    summarise(
        RMSE = mean(RMSE),
        MAE = mean(MAE),
        MASE = mean(MASE),
        Winkler = mean(winkler),
        CRPS = mean(CRPS)
    ) %>%
    arrange(RMSE)

### ignore
fit <- train %>%
    model(
        ets = ETS(revenue),
        arima = ARIMA(revenue),
        snaive = SNAIVE(revenue),
        tslm = TSLM(revenue),
        var = VAR(revenue),
        nnetar = NNETAR(revenue))

fit <- train %>%
    model(
        arima = ARIMA(revenue ~ log(revenue)))

fc <- fit %>% 
    forecast(log(revenue), h = "1 year")

new_geos <- new_data(geos, n = 4) %>%
    mutate(revenue = last(geos$revenue))

