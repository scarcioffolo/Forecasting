library(fpp3)

## Example of estimating a model - In this case a ts linear model (TSLM)

gdppc <- global_economy %>%
  mutate(GDP_per_capita = GDP / Population) %>%
  select(Year, Country, GDP, Population, GDP_per_capita)
gdppc

gdppc %>%
  filter(Country == "Sweden") %>%
  autoplot(GDP_per_capita) +
  labs(title = "GDP per capita for Sweden", y = "$US")

fit <- gdppc %>%
  model(trend_model = TSLM(GDP_per_capita ~ trend()))

## Check the forecast ( 3 years)

fc <- fit %>% forecast(h = "3 years")

fc # fable with the distribution of the forecast

fc %>%
  filter(Country == "Sweden") %>%
  autoplot(gdppc) +
  labs(title = "GDP per capita for Sweden", y = "$US")

## 4 simple forecast methods 
## NAIVE, SNAIVE, DRIFT, MEAN

brick_fit <- aus_production %>%
  filter(!is.na(Bricks)) %>%
  model(
    Seasonal_naive = SNAIVE(Bricks),
    Naive = NAIVE(Bricks),
    Drift = RW(Bricks ~ drift()),
    Mean = MEAN(Bricks)
  )
#filter_index() function is a convenient shorthand for extracting a section of a time series.

brick_fit # mable with each column one model

brick_fc <- brick_fit %>%
  forecast(h = "5 years")

brick_fc %>%
  autoplot(aus_production, level = NULL) +
  labs(title = "Clay brick production in Australia",
       y = "Millions of bricks") +
  guides(colour = guide_legend(title = "Forecast"))

# level = NULL to not show the prediction interval

brick_fc %>%
  filter(.model == "Seasonal_naive") %>%
  autoplot(aus_production)

# check the prediction interval for the 95%
z <- brick_fc %>%
  hilo(level = 95)

#extract the data of the prediction interval
# the pull function extract a column of the data, and transform it into a vector
#getting the point forecast
zmean<-z%>% pull(`.mean`)

# getting the prediction interval
zpi<-z%>% pull(`95%`)

zpi$lower
zpi$upper

## ---- Facebook -------------------------------------------------------------------

# Extract training data
fb_stock <- gafa_stock %>%
  filter(Symbol == "FB") %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE)

fb_stock %>% autoplot(Close) +
  labs(
    title = "Facebook closing stock price",
    y = "$US"
  )

# Specify, estimate and forecast
fb_stock %>%
  model(
    Mean = MEAN(Close),
    Naive = NAIVE(Close),
    Drift = RW(Close ~ drift())
  ) %>%
  forecast(h = 42) %>%
  autoplot(fb_stock, level = NULL) +
  labs(
    title = "Facebook closing stock price",
    y = "$US"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

fit <- fb_stock %>% model(NAIVE(Close))

augment(fit) %>%
  filter(trading_day > 1100) %>%
  ggplot(aes(x = trading_day)) +
  geom_line(aes(y = Close, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted"))

augment(fit) %>%
  autoplot(.resid) +
  labs(
    y = "$US",
    title = "Residuals from na??ve method"
  )

augment(fit) %>%
  ggplot(aes(x = .resid)) +
  geom_histogram(bins = 150) +
  labs(title = "Histogram of residuals")

augment(fit) %>%
  ACF(.resid) %>%
  autoplot() +
  labs(title = "ACF of residuals")

gg_tsresiduals(fit)

augment(fit) %>%
  features(.resid, ljung_box, lag = 10, dof = 0)

fc <- fb_stock %>%
  model(
    Mean = MEAN(Close),
    Naive = NAIVE(Close),
    Drift = RW(Close ~ drift())
  ) %>%
  forecast(h = 42)

## forecast + transformation 
#EGG PRICES

eggs <- prices %>%
  filter(!is.na(eggs)) %>%
  select(eggs)

eggs %>%
  autoplot(log(eggs)) +
  labs(
    title = "Annual egg prices",
    y = "$US (adjusted for inflation)"
  )

fit <- eggs %>%
  model(rwdrift = RW(log(eggs) ~ drift()))
fit
fc <- fit %>%
  forecast(h = 50)
fc

fc %>% autoplot(eggs) +
  labs(
    title = "Annual egg prices",
    y = "US$ (adjusted for inflation)"
  )

fc %>%
  autoplot(eggs, level = 80, point_forecast = lst(mean, median)) +
  labs(
    title = "Annual egg prices",
    y = "US$ (adjusted for inflation)"
  )


## Forecast using decomposition  
#US RETAIL EMPLOYMENT

us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)

us_retail_employment %>%
  autoplot(Employed)

dcmp <- us_retail_employment %>%
  model(STL(Employed)) %>%
  components()

autoplot(dcmp)

dcmp <- dcmp %>% select(-.model)
dcmp %>% autoplot(season_adjust)

dcmp %>%
  model(NAIVE(season_adjust)) %>%
  forecast() %>%
  autoplot(dcmp) +
  labs(title = "Naive forecasts of seasonally adjusted data")

dcmp %>% autoplot(season_year)

dcmp %>%
  model(SNAIVE(season_year)) %>%
  forecast() %>%
  autoplot(dcmp) +
  labs(title = "Seasonal naive forecasts of seasonal component")

us_retail_employment %>%
  model(stlf = decomposition_model(
    STL(Employed),
    NAIVE(season_adjust),
    SNAIVE(season_year)
  )) %>%
  forecast() %>%
  autoplot(us_retail_employment)

## Training and Test data
#BEER PRODUCTION

recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)

recent_production %>% autoplot(Beer)

train <- recent_production %>%
  filter(year(Quarter) <= 2007)

beer_fit <- train %>%
  model(
    Mean = MEAN(Beer),
    Naive = NAIVE(Beer),
    Seasonal_naive = SNAIVE(Beer),
    Drift = RW(Beer ~ drift())
  )


beer_fc <- beer_fit %>%
  forecast(h = 10)

accuracy(beer_fc, recent_production)
#provides the accuracy of the prediction error, based on the test set

accuracy(beer_fit)
#provides the accuracy based on the residual (training set)

## CROSS-VALIDATION: FACEBOOK

fb_stock <- gafa_stock %>%
  filter(Symbol == "FB") %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE)

fb_stock %>%
  autoplot(Close)

# stretch_tsibble() Stretch with a minimum length of 3, growing by 1 each step. (creates test subsets)
fb_stretch <- fb_stock %>%
  stretch_tsibble(.init = 3, .step = 1) %>%
  filter(.id != max(.id))

fb_stretch

fit_cv <- fb_stretch %>%
  model(RW(Close ~ drift()))

fc_cv <- fit_cv %>%
  forecast(h = 1)

fc_cv

# cross-validated
fc_cv %>% accuracy(fb_stock)

# Training set
fb_stock %>%
  model(RW(Close ~ drift())) %>%
  accuracy()


###### Bootstrap

bricks<-aus_production %>%
  filter_index("1970 Q1" ~ "2004 Q4")

fit<-bricks%>%
  model(NAIVE(Bricks))

sim<-fit%>%generate(h=30, times=100, bootstrap = TRUE)

sim


bricks %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Bricks)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)),
            data = sim) +
  guides(colour = "none")


