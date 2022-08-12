library(fpp3)

## GDP --------------------------------------------------------------------------

global_economy %>%
  filter(Country == "Australia") %>%
  autoplot(GDP)

global_economy %>%
  filter(Country == "Australia") %>%
  autoplot(GDP / Population)

## Print retail adjusted by CPI --------------------------------------------------

print_retail <- aus_retail %>%
  filter(Industry == "Newspaper and book retailing") %>%
  group_by(Industry) %>%
  index_by(Year = year(Month)) %>%
  summarise(Turnover = sum(Turnover))

print_retail %>% autoplot(Turnover)

aus_economy <- global_economy %>%
  filter(Code == "AUS")

# put both dataset together
#https://datasciencebox.org/course-materials/_slides/u2-d08-multi-df/u2-d08-multi-df.html#1
print_retail <- print_retail %>%
  left_join(aus_economy, by = "Year") %>%
  mutate(Adj_turnover = Turnover / CPI * 100)

# Let's make it look nicer (tidy) by pivoting it
#https://datasciencebox.org/course-materials/_slides/u2-d09-tidying/u2-d09-tidying.html#8
print_retail <- print_retail %>%
  pivot_longer(c(Turnover, Adj_turnover),
               names_to = "Type", values_to = "Turnover")

# Plot both on same graph
print_retail %>%
  ggplot(aes(x = Year, y = Turnover, col = Type)) +
  geom_line() +
  labs(
    title = "Turnover: Australian print media industry",
    y = "$AU"
  )

# Use faceting
print_retail %>%
 ggplot(aes(x = Year, y = Turnover)) +
  geom_line() +
  facet_grid(Type ~ ., scales = "free_y") +
  labs(
    title = "Turnover: Australian print media industry",
    y = "$AU"
  )

## Australian food retail --------------------------------------------------------
## Transforming the series !!
aus_retail%>%
  count()

food <- aus_retail %>%
  filter(Industry == "Food retailing") %>%
  summarise(Turnover = sum(Turnover))

food %>% autoplot(Turnover) +
  labs(y = "Turnover ($AUD)")

food %>% autoplot(sqrt(Turnover)) +
  labs(y = "Square root turnover")

food %>% autoplot(log(Turnover)) +
  labs(y = "Log turnover")

food %>%
  features(Turnover, features = guerrero)

food %>% autoplot(box_cox(Turnover, 0.0524)) +
  labs(y = "Box-Cox transformed turnover")

# Moving Averages
aus_exports <- global_economy %>%
  filter(Country == "Australia") %>%
  select(Exports)%>%
  mutate(`5-MA` = slider::slide_dbl(Exports, mean,
                                    .before = 2, .after = 2, .complete = TRUE))
aus_exports

aus_exports %>%
  autoplot(Exports) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  labs(y = "% of GDP",
       title = "Total Australian exports") +
  guides(colour = guide_legend(title = "series"))


aus_exports <-aus_exports%>%
  mutate(`7-MA` = slider::slide_dbl(Exports, mean,
                                    .before = 3, .after = 3, .complete = TRUE))%>%
  mutate(`9-MA` = slider::slide_dbl(Exports, mean,
                                    .before = 4, .after = 4, .complete = TRUE))

aus_exports %>%
  autoplot(Exports) +
  geom_line(aes(y = `7-MA`), colour = "#0072B2") +
  labs(y = "% of GDP",
       title = "Total Australian exports - Moving Average 7") +
  guides(colour = guide_legend(title = "series"))

aus_exports %>%
  autoplot(Exports) +
  geom_line(aes(y = `9-MA`), colour = "#009E73") +
  labs(y = "% of GDP",
       title = "Total Australian exports - Moving Average 9") +
  guides(colour = guide_legend(title = "series"))


## Moving averages of moving averages

beer <- aus_production %>%
  filter(year(Quarter) >= 1992) %>%
  select(Quarter, Beer)
beer_ma <- beer %>%
  mutate(`4-MA` = slider::slide_dbl(Beer, mean,.before = 1, .after = 2, .complete = TRUE),
         `2x4-MA` = slider::slide_dbl(`4-MA`, mean,.before = 1, .after = 0, .complete = TRUE))
beer_ma


us_retail_employment_ma <- us_retail_employment %>%
  mutate(
    `12-MA` = slider::slide_dbl(Employed, mean,
                                .before = 5, .after = 6, .complete = TRUE),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                                  .before = 1, .after = 0, .complete = TRUE)
  )

us_retail_employment_ma %>% autoplot(Employed, colour = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00") + labs(y = "Persons (thousands)", 
                                                           title = "Total employment in US retail")


## Times series components

us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)

us_retail_employment %>%
  autoplot(Employed) +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

dcmp <- us_retail_employment %>%
  model(stl = STL(Employed))
components(dcmp)

us_retail_employment %>%
  autoplot(Employed, color = "gray") +
  autolayer(components(dcmp), trend, color = "red") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

components(dcmp) %>% autoplot()

components(dcmp) %>% gg_subseries(season_year)

us_retail_employment %>%
  autoplot(Employed, color = "gray") +
  autolayer(components(dcmp), season_adjust, color = "blue") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

us_retail_employment %>%
  model(STL(Employed ~ season(window = 13) + trend(window = 7), robust = TRUE)) %>%
  components() %>%
  autoplot() +
  labs(title = "STL decomposition: US retail employment")


