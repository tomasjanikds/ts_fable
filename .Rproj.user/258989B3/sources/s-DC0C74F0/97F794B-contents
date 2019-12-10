library(tidyverse)
library(tsibble)
library(lubridate)
library(fable)

## read data in (full data set by week)
raw_data <- read_csv('/Users/tomasjanik/Dropbox/datasets/timeseries.csv')

## select only qtoclose = 0
raw_qtoclose0 <- raw_data %>%
    filter(qtoclose == 0) %>%
    select(-c(snapqtr, qtoclose, geo, newregion, newsubregion, fq)) %>%
    group_by(series, fiscal_period, CoE) %>%
    summarise(bookings = sum(sum_revenue, na.rm = TRUE),
              pipe = sum(pipe, na.rm = TRUE)) %>%
    ungroup()

## transform to tsibble object
raw_tsibble <- raw_qtoclose0 %>%
    select(-CoE) %>%
    separate(., series, c('dealrange', 'geo', 'region', 'subregion'),
             sep = "\\$") %>%
    select(-subregion) %>%
    mutate(fiscal_period = yq(fiscal_period)) %>%
    as_tsibble(., key = c(dealrange, geo, region),
               fiscal_period) %>%
    mutate(fiscal_period = yearquarter(fiscal_period))

## select only COMPUTE
prod_compute <- raw_tsibble %>%
    filter(dealrange == 'COMPUTE')
           # year(fiscal_period) <= 2019)

## Fit and forecast
fit <- prod_compute %>%
    model(
        snaive = SNAIVE(bookings ~ lag("year")),
        ets = ETS(bookings),
        arima = ARIMA(bookings)
    )

fc <- fit %>%
    forecast(h = "1 year")
fc

fc %>%
    filter(region == "ALL") %>%
    autoplot(prod_compute, level = NULL) +
    xlab("Year") + ylab("Revenue (millions)")

## set the training data
train <- prod_compute %>%
    filter(yearquarter(fiscal_period) <= yq('19Q2'))

## any missing values ?
has_gaps(train, .full = TRUE)

prod_gaps <- train %>% 
    count_gaps(.full = TRUE)

prod_gaps

## plot missing values
ggplot(prod_gaps, aes(x = region, colour = region)) +
    geom_linerange(aes(ymin = .from, ymax = .to)) +
    geom_point(aes(y = .from)) +
    geom_point(aes(y = .to)) +
    coord_flip() +
    theme(legend.position = "bottom")

## replace missing values with mean
train %>%
    group_by_key() %>%
    fill_gaps(bookings = mean(bookings),
              .full = TRUE) -> train
    # fill_gaps(bookings = imputeTS::na_kalman(bookings),
    #           .full = TRUE)

## Fit and forecast with simple ensembling
fit <- train %>%
    model(
        ets = ETS(bookings),
        arima = ARIMA(bookings),
        snaive = SNAIVE(bookings)
    ) %>%
    mutate(mixed = (ets + arima + snaive) / 3)

fc <- fit %>% 
    forecast(h = "1 year") %>%
    group_by_key() %>%
    mutate(qtr = quarter(fiscal_period))

fc %>% accuracy(., prod_compute, by = "qtr")

fc_accuracy <- accuracy(fc, prod_compute,
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

## Time series cross-validation accuracy
train_tr <- train %>%
    slice(1:(n()-1)) %>%
    stretch_tsibble(.init = 3, .step = 1)

fc <- train_tr %>%
    model(SNAIVE(bookings)) %>%
    forecast(h=1)

fc %>% accuracy(train)

# Residual accuracy
train %>% model(SNAIVE(bookings)) %>% accuracy()

## forecast horizon accuracy
train <- prod_compute %>%
    filter(geo == 'ALL',
           yearquarter(fiscal_period) <= yq('19Q2'))

train %>%
    group_by_key() %>%
    fill_gaps(bookings = mean(bookings),
              .full = TRUE) -> train

train_tr <- train %>%
    slice(1:(n()-2)) %>%
    stretch_tsibble(.init = 4, .step = 1)

fc <- train_tr %>%
    model(SNAIVE(bookings)) %>%
    forecast(h=2) %>%
    group_by(.id) %>%
    mutate(h = row_number()) %>%
    ungroup()

fc %>%
    accuracy(train, by = "h") %>%
    ggplot(aes(x = h, y = RMSE)) + geom_point()

## forecast individual quarters (not finished)
train <- prod_compute %>%
    filter(geo == 'ALL',
           yearquarter(fiscal_period) <= yq('19Q2'))

train %>%
    group_by_key() %>%
    fill_gaps(bookings = mean(bookings),
              .full = TRUE) -> train

# train_tr <- train %>%
#     slice(1:(n()-2)) %>%
#     stretch_tsibble(.init = 4, .step = 1)

fc <- train %>%
    model(snaive = SNAIVE(bookings),
          ets = ETS(bookings)) %>%
    forecast(h=3) %>%
    mutate(qtr = quarter(fiscal_period)) %>%
    group_by(qtr) %>%
    mutate(h = row_number()) %>%
    ungroup()

fc %>%
    accuracy(prod_compute, by = "fiscal_period") # %>%
    ggplot(aes(x = h, y = RMSE)) + geom_point()
###

## features in time series
library(feasts)

## look at the seasonality vs. trend
no_nas <- raw_tsibble %>%
    filter(yearquarter(fiscal_period) <= yq('20Q2')) %>%
    fill_gaps(bookings = mean(bookings),
              .full = TRUE)

raw_tsibble %>%
    features(bookings, feat_stl) %>%
    ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
    geom_point() +
    ggtitle("Series with NAs") +
    facet_wrap(vars(dealrange))


no_nas %>%
    features(bookings, feat_stl) %>%
    ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
    geom_point() +
    ggtitle("Series with no NAs") +
    facet_wrap(vars(dealrange))

## look at the seasonality vs. trend by GEO
p <- raw_tsibble %>% 
    # filter(dealrange %in% c('MGMT + VCS', 'COMPUTE', 'NSX')) %>%
    features(bookings, feature_set(tags="stl")) %>%
    ggplot(aes(x=trend_strength, y=seasonal_strength_year, col=dealrange, label = region)) +
    geom_point() + 
    scale_color_viridis(discrete = TRUE) +
    # theme_bw() +
    facet_wrap(vars(geo))
ggplotly(p, tooltip = c('col', 'label'))

## look at the spikiness vs. linearity by GEO
p <- raw_tsibble %>% 
    filter(dealrange != 'ALL' & geo != 'ALL' & region != 'ALL') %>%
    features(bookings, feature_set(tags="stl")) %>%
    ggplot(aes(x=spikiness, y=linearity, col=dealrange, label = region)) +
    geom_point() + 
    scale_color_viridis(discrete = TRUE) +
    # theme_bw() +
    facet_wrap(vars(geo))
ggplotly(p, tooltip = c('col', 'label'))

## principal components
product_features <- no_nas %>%
    features(bookings, feature_set(pkgs="feasts"))

pcs <- product_features %>%
    na.omit() %>%
    select(-dealrange, -geo, -region) %>%
    prcomp(scale=TRUE) %>%
    augment(product_features %>% na.omit())

pcs %>%
    ggplot(aes(x=.fittedPC1, y=.fittedPC2, col=geo)) +
    geom_point() + 
    theme(aspect.ratio=1)    

p <- hdrcde::hdrscatterplot(pcs$.fittedPC1, pcs$.fittedPC2, noutliers=5)
p
## finding outliers (time-series, not values)
# yahoo <- tsfeatures::yahoo_data() %>%
#     as_tsibble() %>%
#     mutate(
#         Time = hms::hms(day = trunc(index) - 1L,
#                         hour = as.integer((round(24*(index-1))) %% 24))
#     ) %>%
#     as_tsibble(index = Time, key=key) %>%
#     select(Time, key, value)
# 
# yahoo_features <- bind_cols(
#     yahoo %>% features(value, features=list(
#         mean = ~ mean(., na.rm = TRUE),
#         var = ~ var(., na.rm = TRUE)
#     )),
#     yahoo %>% features(scale(value), features = list(
#         ~ feat_acf(.),
#         ~ feat_spectral(.),
#         ~ n_flat_spots(.),
#         ~ n_crossing_points(.),
#         ~ var_tiled_var(., .period = 24, .size = 24),
#         ~ shift_level_max(., .period = 24, .size = 24),
#         ~ shift_var_max(., .period = 24, .size = 24),
#         ~ shift_kl_max(., .period = 24, .size = 48),
#         ~ feat_stl(., .period = 24, s.window = "periodic", robust = TRUE)
#     ))
# ) %>%
#     rename(lumpiness = var_tiled_var) %>%
#     select(key, mean, var, acf1, trend_strength,
#            seasonal_strength_24, linearity, curvature,
#            seasonal_peak_24, seasonal_trough_24,
#            spectral_entropy, lumpiness, spikiness,
#            shift_level_max, shift_var_max,
#            n_flat_spots, n_crossing_points,
#            shift_kl_max, shift_kl_index)

product_features <- bind_cols(
    no_nas %>% filter(dealrange != 'ALL' & region != 'ALL') %>%
        features(bookings, features=list(
        mean = ~ mean(., na.rm = TRUE),
        var = ~ var(., na.rm = TRUE)
    )),
    no_nas %>% filter(dealrange != 'ALL' & region != 'ALL') %>%
        features(scale(bookings), features = list(
        ~ feat_acf(.),
        ~ feat_spectral(.),
        ~ n_flat_spots(.),
        ~ n_crossing_points(.),
        ~ var_tiled_var(., .period = 4, .size = 4),
        ~ shift_level_max(., .period = 4, .size = 4),
        ~ shift_var_max(., .period = 4, .size = 4),
        ~ shift_kl_max(., .period = 4, .size = 8),
        ~ feat_stl(., .period = 4, s.window = "periodic", robust = TRUE)
    ))
) %>%
    rename(lumpiness = var_tiled_var) %>%
    select(dealrange, geo, region, 
           mean, var, acf1, trend_strength,
           seasonal_strength_4, linearity, curvature,
           seasonal_peak_4, seasonal_trough_4,
           spectral_entropy, lumpiness, spikiness,
           shift_level_max, shift_var_max,
           n_flat_spots, n_crossing_points,
           shift_kl_max, shift_kl_index)

hwl_pca <- product_features %>%
    select(-dealrange, -geo, -region) %>%
    na.omit() %>%
    prcomp(scale=TRUE) %>%
    augment(na.omit(product_features))

hwl_pca %>%
    as_tibble() %>%
    ggplot(aes(x=.fittedPC1, y=.fittedPC2)) +
    geom_point()
    
p <- hdrcde::hdrscatterplot(hwl_pca$.fittedPC1, hwl_pca$.fittedPC2, noutliers=5)
ggplotly(p)

### ignore
ts_tst <- raw_tsibble %>% 
    filter(dealrange == 'VCPP' & geo == 'EMEA' & region == 'SEMEA') %>%
    select(-dealrange, -geo, -region, -pipe) %>%
    as.ts(., bookings, frequency = 4)

fit = stl(ts_tst, 
          s.window="periodic")
plot(fit)

bookings_data <- raw_tsibble %>% 
    filter(dealrange == 'VCPP' & geo == 'EMEA' & region == 'SEMEA') %>%
    select(-dealrange, -geo, -region) %>%
    as.ts(., bookings, frequency = 4)

pipe_data <- raw_tsibble %>% 
    filter(dealrange == 'VCPP' & geo == 'EMEA' & region == 'SEMEA') %>%
    select(-dealrange, -geo, -region) %>%
    as.ts(., pipe, frequency = 4)

library(dygraphs)
lungDeaths <- cbind(bookings_data, pipe_data)
dygraph(lungDeaths, main = "Time series of bookings and pipe") %>%
    dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))







