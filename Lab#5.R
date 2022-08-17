# Lab #1
# Install fpp3 if you haven't done yet
# install.packages("fpp3")
rm(list=ls())
dev.off(dev.list()["RStudioGD"])

library(fpp3)


# Algerian Exports

algeria_economy <- global_economy %>%
  filter(Country == "Algeria")

algeria_economy %>% autoplot(Exports)

#OR

ggplot(algeria_economy,aes(x=Year, y= Exports))+
  geom_line()

# Let's assume 3 models - ETS(A,N,N); ETS(M,N,N); and the third, function will pick it A or M for the error automatically. 
fit <- algeria_economy %>%
  model(
    ANN = ETS(Exports ~ error("A") + trend("N") + season("N")),
    MNN = ETS(Exports ~ error("M") + trend("N") + season("N")),
    autoNN = ETS(Exports ~ trend("N") + season("N")),
  )
fit %>%
  select(ANN) %>%
  report()
fit %>%
  select(MNN) %>%
  report()
fit %>%
  select(autoNN) %>%
  report()

tidy(fit)
glance(fit) # the autoNN has chose M errors (looking at the AICc, it makes sense)

components(fit) %>% autoplot() # let's plot the states over time for each model # the main difference is the remainder 
                               #- make sense since the only difference between them is the type of error (A or M)

components(fit) %>%
  left_join(fitted(fit), by = c("Country", ".model", "Year")) # putting the the fitted data with the ETS states results 

fit %>%
  forecast(h = 5) %>%
  filter(.model == "MNN") %>%
  autoplot(algeria_economy) +
  ylab("Exports (% of GDP)") + xlab("Year")

fit %>%
  forecast(h = 5) %>%
  filter(.model == "ANN") %>%
  autoplot(algeria_economy) +
  ylab("Exports (% of GDP)") + xlab("Year") # much larger PI

# Australian population

aus_economy <- global_economy %>%
  filter(Code == "AUS") %>%
  mutate(Pop = Population / 1e6)

aus_economy %>% autoplot(Pop)

#automatic ETS
aus_economy %>%
  model(auto = ETS(Pop)) %>%
  report()

fit <- aus_economy %>%
  model(AAN = ETS(Pop ~ error("A") + trend("A") + season("N")))
report(fit)

#Plot the states (LEVEL, SEASON, ERROR)
components(fit) %>% autoplot()

components(fit) %>%
  left_join(fitted(fit), by = c("Country", ".model", "Year"))

fit %>%
  forecast(h = 10) %>%
  autoplot(aus_economy) +
  ylab("Population") + xlab("Year")

# In this model, the pop is set to increase forever. It does not represent reality
# So even when the ETS select a specific model, it only do so by looking at AICc. It does not have information on the data itself

aus_economy %>%
  model(holt = ETS(Pop ~ error("A") + trend("Ad") + season("N"))) %>%
  report()

aus_economy %>%
  model(holt = ETS(Pop ~ error("A") + trend("Ad") + season("N"))) %>%
  forecast(h = 10) %>%
  autoplot(aus_economy)
#Even having a larger AICc, it makes more sense to see a decreasing population rate over the years. 


aus_economy %>%
  filter(Year <= 2010) %>%
  autoplot(Pop)


fit <- aus_economy %>%
  filter(Year <= 2010) %>%
  model(
    ses = ETS(Pop ~ error("A") + trend("N") + season("N")),
    holt = ETS(Pop ~ error("A") + trend("A") + season("N")),
    damped = ETS(Pop ~ error("A") + trend("Ad") + season("N"))
  )

tidy(fit)
accuracy(fit) # the holt has better accuracy over the trainning data (before 2010)
glance(fit) # the damped was the choice for this subsample
forecast(fit) %>% accuracy(aus_economy) # the accuracy for the test sample, holt still present better accuracy


#estimating an ETS for every country
fit <- global_economy %>%
  model(
    ets = ETS(Population))

tidy(fit)
fit%>%filter(Country=="Brazil")%>%tidy() #ETS(A,A,N)

fc <- fit %>%
  forecast(h = 10)

fc%>%
  filter(Country=="Brazil")%>%
  autoplot(global_economy%>%filter(Country=="Brazil"))
  
  

## Aus holidays

aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Trips = sum(Trips)) # sum by quarter (tsibble)


aus_holidays %>% autoplot(Trips)

# try 3 different models
fit <- aus_holidays %>%
  model(
    additive = ETS(Trips ~ error("A") + trend("A") + season("A")),
    multiplicative = ETS(Trips ~ error("M") + trend("A") + season("M")),
    auto = ETS(Trips)
  )
fit
glance(fit)
tidy(fit)
accuracy(fit)

fit %>%
  select(multiplicative) %>%
  report()


fc <- fit %>% forecast()

fc %>%
  autoplot(aus_holidays) + xlab("Year") +
  ylab("Overnight trips (thousands)")

components(fit) %>% autoplot()

# look at the y-axis
fit %>%
  select(multiplicative) %>%
  components() %>%
  autoplot()

# Daily pedestrian data

sth_cross_ped <- pedestrian %>%
  filter(
    Date >= "2016-07-01",
    Sensor == "Southern Cross Station"
  ) %>%
  index_by(Date) %>%
  summarise(Count = sum(Count) / 1000)


# Often the single most accurate forecasting method for seasonal data (Holt-Winters damped method)
sth_cross_ped %>%
  filter(Date <= "2016-07-31") %>%
  model(
    hw = ETS(Count ~ error("M") + trend("Ad") + season("M"))
  ) %>%
  forecast(h = "2 weeks") %>%
  autoplot(sth_cross_ped %>% filter(Date <= "2016-08-14")) +
  labs(
    title = "Daily traffic: Southern Cross",
    y = "Pedestrians ('000)"
  )




## Example: Australian holiday tourism

holidays <- tourism %>%
  filter(Purpose == "Holiday")

fit <- holidays%>%
  model(ets = ETS(Trips))


fit %>%
  filter(Region == "Snowy Mountains") %>%
  report()


fit %>%
  filter(Region == "Snowy Mountains") %>%
  components(fit)%>%
  autoplot()


fit %>%
  filter(Region == "Snowy Mountains") %>%
  forecast(h=12) %>%
  autoplot(holidays, show_gap = FALSE) + # show_gap = FALSE connects the forecasting with the original data
  xlab("Year") + ylab("Overnight trips (thousands)")

# Sum over regions

aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Trips = sum(Trips))


aus_holidays %>% autoplot()

fit <- aus_holidays %>% model(ETS(Trips))
report(fit)


components(fit) %>%
  autoplot() +
  ggtitle("ETS(M,N,M) components")

fit %>% augment()

# For multiplicaive error models, the residual and innovation are different. Innovation residuals (.innov) are given by E^ while regular residuals (.resid) are yt − ˆyt−1.
fit %>% augment()

residuals(fit) # residuals
residuals(fit, type = "response")

fit %>%
  gg_tsresiduals()


## H02

h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost))

h02 %>%
  autoplot(Cost)

h02 %>%
  model(ETS(Cost)) %>%
  report()

h02 %>%
  model(ETS(Cost ~ error("A") + trend("A") + season("A"))) %>%
  report()

h02 %>%
  model(ETS(Cost)) %>%
  forecast(h=24) %>%
  autoplot(h02)

# ETS(A,N,M), ETS(A,A,M), ETS(A,Ad,M) can lead can lead to numerical difficulties - So usually we do not estimate them (forbidden)
fit <- h02 %>%
  model(
    auto = ETS(Cost),
    AAA = ETS(Cost ~ error("A") + trend("A") + season("A")),
    damped = ETS(Cost ~ trend("Ad")),
    forbidden = ETS(Cost ~ error("A") + trend("Ad") + season("M"))
  )

fit %>% accuracy()
fit %>% glance()
fit %>% tidy()

# Example of STL + ETS (do not worry about it)

h02 %>%
  model(
    decomposition_model(
      STL(Cost),
      ETS(season_adjust),
      SNAIVE(season_year))
  ) %>%
  forecast(h=24) %>%
  autoplot(h02)
