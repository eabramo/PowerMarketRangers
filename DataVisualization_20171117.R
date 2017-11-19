#Data visualizations 
#11/17/2017

setwd("~/Box Sync/Yale/2017 Fall/Independent Study/Analysis Data") 

library("ggplot2")
library("reshape2")
library("ggthemes")
library("dplyr")
library("lubridate")
#library("plotly")


x <- read.csv('combinedhourly_weather_load_lmp_gen.csv')

summary(x)

#set categorical values as factors
as.factor(x$hour)
as.factor(x$node_id)
as.factor(x$month.x)

box1 <- boxplot(lmp ~ node_id, data=x, main="boxplot1 lmp by node")

box2 <- boxplot(x$load_MW ~ node_id, data=x, main= "boxplot2 load (MW) by node")

box3 <- boxplot (houravglmp ~ node_id, data=x, main="boxplot3 hour avg lmp by node")

box4 <- boxplot(lmp ~ hour, data=x, main="boxplot4 lmp by hour")

box5 <- boxplot(load_MW ~ hour, data=x, main="boxplot5 load (MW) by hour")

box6 <- boxplot(houravglmp ~ hour, data=x, main="boxplot6 hourly avg lmp by hour")

box7 <- boxplot(lmp ~ hour*node_id, data=x, main="boxplot7 lmp by hour per node")

hist1 <- hist(x$load_MW)

box8 <- boxplot (x$Temp..F. ~ hour, data=x, main="boxplot8 temp by hour")
box9 <- boxplot (x$Temp..F. ~ hour, load_MW ~ hour, data=x, col=c("blue","red"), main="boxplot9 temp and load by hour")
box10 <- boxplot (x$houravglmp ~ x$month.x, data = x, main = "boxplot10 avg hourly lmp by month")

box11 <- boxplot(lmp ~ x$month.x, data=x, main="boxplot11 lmp by month")
box12 <- boxplot (load_MW ~ x$month.x, data=x, main="boxplot12 load MW by month")
box13 <- boxplot (lmp ~ node_id*x$month.x, data=x, main="boxplot13 lmp by node and month")

intrxn1 <- interaction.plot(x$node_id, x$lmp, x$load_MW)
intrxn2 <- interaction.plot(x$hour, x$lmp, x$load_MW)
intrxn3 <- interaction.plot(x$node_id, x$hour, x$lmp)
intrxn4 <- interaction.plot(x$node_id, x$lmp, x$hour)
intrxn5 <- interaction.plot(x$node_id, x$load_MW, x$hour)
intrxn6 <- interaction.plot(x$hour, x$Temp..F., x$lmp)
intrxn7 <- interaction.plot(x$hour, x$Temp..F.,x$load_MW)

