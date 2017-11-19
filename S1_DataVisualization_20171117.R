#Data visualizations 
#11/17/2017

setwd("~/Box Sync/Yale/2017 Fall/Independent Study/Analysis Data") 

library("ggplot2")
library("reshape2")
library("ggthemes")
library("dplyr")
library("lubridate")
#library("plotly")


x <- read.csv('hourly_lmp_load_weather_gen_pct.csv')

summary(x)

#set categorical values as factors
as.factor(x$hour)
as.factor(x$node_id)
as.factor(x$month)

str(x)

#boxplots

box1 <- boxplot(lmp ~ node_id, data=x, main="boxplot1 lmp by node")

box2 <- boxplot(x$load_MW ~ node_id, data=x, main= "boxplot2 load (MW) by node")

box3 <- boxplot (houravglmp ~ node_id, data=x, main="boxplot3 hour avg lmp by node")

box4 <- boxplot(lmp ~ hour, data=x, main="boxplot4 lmp by hour")

box5 <- boxplot(load_MW ~ hour, data=x, main="boxplot5 load (MW) by hour")

box6 <- boxplot(houravglmp ~ hour, data=x, main="boxplot6 hourly avg lmp by hour")

box7 <- boxplot(lmp ~ hour*node_id, data=x, main="boxplot7 lmp by hour per node")

box8 <- boxplot (x$Temp..F. ~ hour, data=x, main="boxplot8 temp by hour")
box9 <- boxplot (x$Temp..F. ~ hour, load_MW ~ hour, data=x, col=c("blue","red"), main="boxplot9 temp and load by hour")
box10 <- boxplot (x$houravglmp ~ x$month, data = x, main = "boxplot10 avg hourly lmp by month")

box11 <- boxplot(lmp ~ x$month, data=x, main="boxplot11 lmp by month")
box12 <- boxplot (load_MW ~ x$month, data=x, main="boxplot12 load MW by month")
box13 <- boxplot (lmp ~ node_id*x$month, data=x, main="boxplot13 lmp by node and month")
box14 <- boxplot(houravglmp ~ pct_solar, data=x, main="boxplot14 hourlyavg lmp by percent solar")

intrxn1 <- interaction.plot(x$node_id, x$lmp, x$load_MW)
intrxn2 <- interaction.plot(x$hour, x$lmp, x$load_MW)
intrxn3 <- interaction.plot(x$node_id, x$hour, x$lmp)
intrxn4 <- interaction.plot(x$node_id, x$lmp, x$hour)
intrxn5 <- interaction.plot(x$node_id, x$load_MW, x$hour) #all the same for each hour
intrxn6 <- interaction.plot(x$hour, x$Temp..F., x$lmp)
intrxn7 <- interaction.plot(x$hour, x$Temp..F.,x$load_MW)
intrxn8 <- interaction.plot(x$node_id, x$pct_solar+x$pct_wind, x$houravglmp)
intrxn9 <- interaction.plot(x$node_id, x$pct_rt, x$houravglmp)

hist1 <- hist(x$load_MW)
hist2 <- hist(x$houravglmp)
hist3 <- hist(x$pct_solar)
hist4 <- hist(x$pct_wind)
hist5 <- hist(x$lmp)
hist6 <- hist(x$pct_rt)
hist7 <- hist(x$rt_load_MW)

plot1 <- plot(x$pct_rt, x$houravglmp)


## code worked on with Matt Moroney

# plot
ggplot(x, aes(date, houravglmp)) +
  geom_line(aes(color = node_id)) +
  facet_wrap(~node_id)

ggplot(x, aes(date, houravglmp)) +
  geom_line(aes(color = node_id)) +
  facet_wrap(~month)

ggplot(x, aes(pct_solar, houravglmp)) +
  geom_line(aes(color = node_id)) +
  facet_wrap(~hour)

ggplot(x, aes(pct_wind, houravglmp)) +
  geom_line(aes(color = node_id)) +
  facet_wrap(~hour)


ggplot(x, aes(pct_wind+pct_solar, houravglmp)) +
  geom_line(aes(color = node_id)) +
  facet_wrap(~hour)


ggplot(x, aes(pct_rt, houravglmp)) +
  geom_line(aes(color = node_id)) +
  facet_wrap(~hour)

# explore
x %>%
  group_by(x$node_id, x$hour)
summarize(var = var(x$houravglmp))

# cleanup
lm.df <- dplyr::select(x, -ba_name, -freq, -market, -PTime, -date, -n, -X, -timestamp, -time, -Wind.Chill..F. ,-lmp) %>%
  na.omit()


# stepwise regression
library(MASS)
# mutate(node_id = as.factor(node_id),
#  hour = as.factor(hour))
lm1 <- lm(houravglmp ~ ., data = lm.df)
summary (lm1)
lm1.stepwise <- stepAIC(lm1)

