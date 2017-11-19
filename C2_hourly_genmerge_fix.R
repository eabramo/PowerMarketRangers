# load libraries
library ("dplyr")
library("lubridate")
library("ggplot2")

# read csvs
cg <- read.csv ("combinedgendata.csv", stringsAsFactors = FALSE)
c2 <- read.csv('hourly_lmp_load_weather.csv', stringsAsFactors = FALSE) %>%
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

str(test)

# add in percentage by generation

#via mutation (not sure how to make into objects to add into columns)
#test %>%
  group_by(date, hour) %>%
  mutate(total_dh = wind_MW + solar_MW + other_MW) %>%
  mutate(pct_solar = solar_MW / total_dh,
           pct_wind = wind_MW / total_dh, 
           pct_other =other_MW / total_dh, 
           rt_load = (load_MW - total_dh),
           pct_rt = rt_load/total_dh) %>%
             
  summary()

#so i just did it the easy/slow/lazy way

test$total_dh <- test$wind_MW + test$solar_MW + test$other_MW
test$pct_solar <- test$solar_MW / test$total_dh
test$pct_wind <- test$wind_MW / test$total_dh
test$pct_other <- test$other_MW / test$total_dh
test$rt_load_MW <- test$load_MW - test$total_dh
test$pct_rt <- test$rt_load/test$total_dh

str(test)

#write to csv for use in data analysis
write.csv(test, file = "hourly_lmp_load_weather_gen_pct.csv")

## the below is also in DataVisualization_20171117.R

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

