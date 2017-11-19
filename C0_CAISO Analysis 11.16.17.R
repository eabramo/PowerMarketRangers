# Load libraries 

library("ggplot2")
library("reshape2")
library("ggthemes")
library("dplyr")
library("lubridate")
library("plotly")

# DO NOT EDIT

# Merged files

allfiles <- list.files(path = "~/R/powermarkets/LMP")

library(readr)
alldata <- list.files(
  path = "~/R/powermarkets/LMP", 
  full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows


# Convert time from UT to PT

timestamp <- alldata$timestamp 

pb.txt <- timestamp
pb.date <- as.POSIXct(pb.txt, tz="UTC")
attributes(pb.date)$tzone <- "America/Los_Angeles"

alldata$PTime <- pb.date

# Write to csv

write.csv(alldata, file = "final.all.data.11.16.csv")

# END DO NOT EDIT

# check to see if it's all there

z <- read.csv("~/R/powermarkets/LMP/final.all.data.11.16.csv")

summary(z)


# check for duplicates

El <- z[z$node_id == "ELCENTRO_2_N001",]
Ob <- z[z$node_id == "OBANION_2_N025",]
Pot <- z[z$node_id == "TBCPOT1_1_GN002",]
Ptb <- z[z$node_id == "TBCPTB1_2_GN002",]
TJI <- z[z$node_id == "TJI-230_2_N101",]

length(unique(El$timestamp)) == nrow(El)
length(unique(Ob$timestamp)) == nrow(Ob)
length(unique(Pot$timestamp)) == nrow(Pot)
length(unique(Ptb$timestamp)) == nrow(Ptb)
length(unique(TJI$timestamp)) == nrow(TJI)

# develop tables to ID where missing data/duplicates are

dfz <- z %>%
  mutate(time = ymd_hms(timestamp),
         date = date(time),
         hour = hour(time),
         month = month(time))

tabl <- count(dfz$date)

tabl[tabl$freq != 1440,]
length(tabl$x)

may29 <- dfz[dfz$date == "2017-05-29",]
may29[duplicated(may29),]
summary(may29)

nov6 <- dfz[dfz$date == "2016-11-06",]
nov6[duplicated(nov6)]
table(duplicated(nov6))

summary(nov6)

# combine the generation data

y <- read.csv ("combinedloaddata.csv")

summary(y)

x <- merge(z, y, by=c("timestamp", "node_id"), all = TRUE)

genx <- within(x, rm("ba_name.y", "freq.y", "market.y", "PTime.y"))

summary(genx)

write.csv(genx, file = "combinedloadlmpdata.csv")


