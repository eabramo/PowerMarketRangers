# load libraries
library ("dplyr")
library("lubridate")
library("ggplot2")

# read csvs
cg <- read.csv ("combinedgendata.csv", stringsAsFactors = FALSE)
c2 <- read.csv('hourlyweatherandlmp.csv', stringsAsFactors = FALSE) %>%
  mutate(date = ymd(date)) %>%
  select(-Date)

# fix dates
dfc1 <- cg %>%
  mutate(time = ymd_hms(PTime),
         date = date(time),
         hour = hour(time),
         month = month(time)) %>%
  select(-X.1, -X)

# merge
test <- full_join(c2, dfc1) %>%
  filter(node_id != "NA") %>%
  mutate(Wind.Chill..F. = as.numeric(Wind.Chill..F.)) %>%
  mutate(Heat.Index..F. = as.numeric(Heat.Index..F.))

# plot
ggplot(test, aes(date, houravglmp)) +
  geom_line(aes(color = node_id)) +
  facet_wrap(~node_id)

# explore
test %>%
  group_by(node_id, hour)
  summarize(var = var(houravglmp))

# cleanup
lm.df <- dplyr::select(test, -ba_name, -freq, -market, -PTime, -date, -n, -X, -timestamp, -time, -Wind.Chill..F. ,-lmp) %>%
  na.omit()
  
  
# stepwise regression
library(MASS)
 # mutate(node_id = as.factor(node_id),
       #  hour = as.factor(hour))
lm1 <- lm(houravglmp ~ ., data = lm.df)
lm1.stepwise <- stepAIC(lm1)

# make comment here explaining what this is
test %>%
  group_by(date, hour) %>%
  mutate(total_dh = wind_MW + solar_MW + other_MW) %>%
  transmute(pct_solar = solar_MW / total_dh,
         pct_wind = wind_MW / total_dh, 
         pct_other =other_MW / total_dh, 
         rt_load = (load_MW - total_dh),
         pct_rt = rt_load/total_dh) %>%
  summary()