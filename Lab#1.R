# Lab #1
# Install fpp3 if you haven't done yet
# install.packages("fpp3")
rm(list=ls())
dev.off(dev.list()["RStudioGD"])

library(fpp3)

# let's look at some data from star wars
df<-starwars

view(df) 
#or
glimpse(df)

# How many rows and columns does this dataset have?
nrow(df)
ncol(df)
dim(df) # row by col

# LET'S MAKE SOME GRAPHS

ggplot(data=df)+          # choosing the data 
  geom_point(mapping = aes(x=height, y=mass, color= sex))+    # The type of plot +  the aesthetic of the plotting (and what we want to plot, and other factors)
  labs(title = "Mass vs. Height of Starwars characters", x="height (cm)", y = "Weight (Kg)")

# instead of color, you could include `shape`, `alpha`. try it out
# you can also change the color of the data points

ggplot(data=df)+          # choosing the data 
  geom_point(mapping = aes(x=height, y=mass), color = "blue")+    # The type of plot +  the aesthetic of the plotting (and what we want to plot, and other factors)
  labs(title = "Mass vs. Height of Starwars characters", x="height (cm)", y = "Weight (Kg)")


# LOOK AT THIS BOOK FOR FURTHER EXAMPLES IN DATA VISUALISATION - https://r4ds.had.co.nz/data-visualisation.html

### DATA TRANSFORMATION
install.packages("nycflights13")
library(nycflights13)

flights # It is a tibble  - 336,776 obs and 19 variables

#int stands for integers.

#dbl stands for doubles, or real numbers.

#chr stands for character vectors, or strings.

# we will be looking at this functions:
#Pick observations by their values (filter()).
#Reorder the rows (arrange()).
#Pick variables by their names (select()).
#Create new variables with functions of existing variables (mutate()).
#Collapse many values down to a single summary (summarise())

flights%>%
  filter(month == 1, day == 1)

#or

jan1<-flights%>%
  filter(month == 1, day == 1)

#>, >=, <, <=, != (not equal), and == (equal).
#># & (and); |(or)

flights%>%
  filter(month == 2 | month == 3) # filter flights that occurred either month 2 or 3

# short-cut (x %in% y) -> This will select every row where x is one of the values in y

flights%>%
  filter(month %in% c(11,12))

origins<-c("EWR", "JFK", "LGA")


flights%>%
  filter(origin %in% origins)
# filter only origin in the vector origins

flights%>%
  filter(arr_delay <= 120, dep_delay <= 120)

### Arrange -arrange() works similarly to filter() except that instead of selecting rows, it changes their order.
flights%>%
  arrange(year, month, day)

# desc() to re-order by a column in descending order
flights%>%
  arrange(desc(month))

# NA are always sorted at the end


### Select columns (select())

flights%>%
  select(year, month, day) #or

flights%>%
  select(year:day)

flights%>%
  select(-(year:day))


# starts_with("abc"): matches names that begin with “abc”.

# ends_with("xyz"): matches names that end with “xyz”.

# contains("ijk"): matches names that contain “ijk”.



flights_sml <- flights%>%
  select(year:day, ends_with("delay"), distance, air_time)
flights_sml

### Mutate - Add new variables

flights_sml%>%
  mutate( gain_time = dep_delay - arr_delay, speed = distance / air_time * 60)

### Grouped summaries with summarise()

flights%>%
  summarise(delay = mean(dep_delay, na.rm = TRUE)) #overall delay 

# we should group the dataset by a given variable - to look at individual groups instead the whole dataset

by_day <- flights%>%
  group_by(year, month, day)

by_day # try to find the difference from the original dataset (flights)

by_day%>%
  summarise(delay = mean(dep_delay, na.rm = TRUE)) #delay by year, month, and day

by_day%>%
  count(origin, sort=TRUE)

by_day%>%
  ungroup()%>%
  count(dest, sort=TRUE)
  

#if there’s any missing value in the input, the output will be a missing value.
# all aggregation functions have an na.rm argument which removes the missing values prior to computation


by_dest <- flights%>%
  group_by(dest)

delay <- by_dest%>%
  summarise(count = n(), dist = mean(distance, na.rm = TRUE), delay = mean(arr_delay, na.rm = TRUE))
delay

delay2 <- delay%>%
  filter(count > 10000, dest != "HNL")%>%
  arrange(delay)



# To count the number of distinct (unique) values, use n_distinct(x)

flights %>% 
  group_by(dest)%>% 
  summarise(carriers = n_distinct(carrier))%>%
  filter(carriers>=6)%>%
  arrange(desc(carriers))

###################################################################################
#tsibble -  Time series for tibble

#creating a tsibble

t<-tsibble(
  Year=2015:2019,
  Obs = c(1,4,5,6,7),
  index = Year)

t
# index indicate the periodic of the dataset

z<-tibble(
  Month = c("2015 Jan", "2015 Feb", "2015 Mar", "2015 Apr", "2015 May"),
  Obs = c(5,6,7,8,10))

# let's convert this dataset into tsibble

z<-z%>%
  mutate(Month = yearmonth(Month))%>%
  as_tsibble(index= Month)

z

#Within tsiblle, we have to define the key variables
olympic_running
# Key variables are Length and Sex 
# It is yearly data, and It is available every 4 year [4Y]

######## PBS ----------------------------------
PBS<-PBS

PBS%>%filter(ATC2 == "A10") 


PBS%>% 
  select(Month, Concession, Type, Cost) 

PBS%>% 
  select(Month, Concession, Type, Cost)%>%
  summarise(total_cost = sum(Cost)) # summarise over what group?

a10<-PBS%>% 
  select(Month, Concession, Type, Cost)%>%
  summarise(total_cost = sum(Cost))%>%
  mutate(total_cost = total_cost / 1e6)

a10 %>%
  autoplot(total_cost)

#or

ggplot(data=a10)+
  geom_line(aes(x=Month, y= total_cost))


a10 %>% gg_season(total_cost, labels = "right")


a10 %>% gg_season(total_cost, labels = "both") +
  labs(
    y = "$ million",
    title = "Seasonal plot: antidiabetic drug sales"
  )

a10 %>%
  gg_subseries(total_cost) +
  labs(
    y = "$ million",
    title = "Subseries plot: antidiabetic drug sales"
  )


##### Reading excel file and transform it into tsibble. 
prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")

prison
# it is quarterly data

prison<- prison %>%
  mutate(Quarter = yearquarter(Date)) 

prison<-prison%>%
         select(-Date) %>%
         as_tsibble(
         index = Quarter,
         key = c(State, Gender, Legal, Indigenous))

# how many ATSI (aboriginal) people were sentenced at Australian Capital Territory (ACT)?

prison%>%
  filter(State == "ACT" & Legal == "Sentenced" & Indigenous == "ATSI")%>%
  ggplot()+
  geom_point(aes(x=Quarter, y= Count, color = Gender))

############################################


#### pivot_longer



## ANSETT ----------------------------------------------------------------------

ansett %>%
  autoplot(Passengers)

ansett %>%
  filter(Class == "Economy") %>%
  autoplot(Passengers)

ansett %>%
  filter(Airports == "MEL-SYD" & Class == "Economy") %>%
  autoplot(Passengers) +
  labs(title = "Melbourne-Sydney traffic")

## MAX TEMP and ELECTRICITY DEMAND ------------------------------------------------------------


vic_elec<-vic_elec
vic_elec %>% autoplot(Demand)
vic_elec %>% gg_season(Demand)
vic_elec %>% gg_season(Demand, period = "week")
vic_elec %>%gg_season(Demand, period = "day")

vic_elec %>% filter(year(Date) == "2013")%>%
  gg_season(Demand, period = "day")

vic_elec %>% filter(Date == "2014/08/20")%>%
  gg_season(Demand, period = "day") 


maxtemp <- vic_elec %>%
  index_by(Day = date(Time))%>%
  summarise(Temperature = max(Temperature))


maxtemp %>%
  autoplot(Temperature) +
  labs(y = "Max temperature")

## LOTS OF EXAMPLES -------------------------------------------------------------

aus_production %>%
  filter(year(Quarter) >= 1980) %>%
  autoplot(Electricity) +
  labs(
    y = "GWh",
    title = "Australian electricity production"
  )

aus_production %>%
  autoplot(Bricks) +
  labs(
    title = "Australian clay brick production",
    y = "million units"
  )

us_employment %>%
  filter(Title == "Retail Trade", year(Month) >= 1980) %>%
  autoplot(Employed / 1e3) +
  labs(
    title = "Retail employment, USA",
    y = "Million people"
  )

gafa_stock %>%
  filter(Symbol == "AMZN", year(Date) >= 2018) %>%
  autoplot(Close) +
  labs(
    title = "Amazon closing stock price",
    y = "$US"
  )

pelt %>%
  autoplot(Lynx) +
  labs(
    title = "Annual Canadian Lynx Trappings",
    y = "Number trapped"
  )

## BEER -------------------------------------------------------------------------

beer <- aus_production %>%
  select(Quarter, Beer) %>%
  filter(year(Quarter) >= 1992)
beer %>% autoplot(Beer)

beer %>% gg_season(Beer, labels = "right")
beer %>% gg_subseries(Beer)


beer %>% gg_lag(Beer, geom = "point", lags = 1:16)

## ACF - Autocorrelation function
t<-beer %>% ACF(Beer, lag_max = 20)
t%>%autoplot()

beer %>%
  ACF(Beer, lag_max = 20) %>%
  autoplot()


## HOLIDAYS --------------------------------------------------------------------

tourism

holidays <- tourism %>%
  mutate(State = recode(State,
                   "Australian Capital Territory" = "ACT",
                   "New South Wales" = "NSW",
                   "Northern Territory" = "NT",
                   "Queensland" = "QLD",
                   "South Australia" = "SA",
                   "Tasmania" = "TAS",
                   "Victoria" = "VIC",
                   "Western Australia" = "WA")
  ) %>%
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))

holidays

holidays %>%
  autoplot(Trips) +
  labs(
    y = "thousands of trips",
    title = "Australian domestic holiday nights"
  )

## RETAIL TRADE ------------------------------------------------------------------

retail <- us_employment %>%
  filter(Title == "Retail Trade", year(Month) >= 1980)

retail %>% autoplot(Employed)

retail %>%
  ACF(Employed, lag_max = 48) %>%
  autoplot()

## Google 2015 -------------------------------------------------------------------

#What is the seasonality of this data set?
google_2015 <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) == 2015) %>%
  select(Date, Close)


google_2018<-gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE) 

google_2015 %>% autoplot(Close)

google_2015 %>% ACF(Close)

google_2015 %>%
  ACF(Close, lag_max = 100) %>%
  autoplot()

# the confidence interval for 95% is calculate by +-2 * sigma. sigma = 1/sqrt(N)
#In our case, N = 252, so CI 95% [-2*1/(252^(1/2)), +2*1/(252^(1/2))] = [-0.1259, 0.1259]
## WHITE NOISE --------------------------------------------------------------------

set.seed(30) # just for reproducibility 

wn <- tsibble(t = seq(50), y = rnorm(50), index = t)

# Is there any pattern in here?

wn %>% autoplot(y)

wn %>% ACF(y, lag_max = 10)

wn %>%
  ACF(y) %>%
  autoplot()

## PIGS ---------------------------------------------------------------------------

pigs <- aus_livestock %>%
  filter(
    State == "Victoria", Animal == "Pigs",
    year(Month) >= 2014
  )


pigs %>% autoplot(Count / 1e3) +
  labs(
    y = "Thousands",
    title = "Number of pigs slaughtered in Victoria"
  )

#creating a variable with difference
pigs<-pigs %>%
  mutate(diff = difference(Count, lag = 2))

# What does this function do?

pigs%>%
  autoplot(diff)


## GOOGLE change in closing price ACF ---------------------------------------------

google_2015 %>%
  ACF(Close) %>%
  autoplot()

google_2015 %>%
  mutate(diff = difference(Close)) %>%
  autoplot(diff)

google_2015 %>%
  mutate(diff = difference(Close)) %>%
  ACF(diff) %>%
  autoplot()


