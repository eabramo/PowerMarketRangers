#Attempts at data analysis

h <- read.csv('hourlyweatherandlmp.csv')
h

library(ggplot2)

# !!!! TAKES A LONG TIME TO RUN THE FOLLOWING CODE !!!! #

# pairs(~date+hour+month+lmp+load_MW+houravglmp+Temp..F.+RH....+Dewpt..F.+Wind.Spd..mph.+Wind.Direction..deg.+Visibility..mi.+Precip..in.+Heat.Index..F., data = h, lower.panel= panel.smooth, upper.panel = panel.smooth, pch=20, main="Matrix2")

#set categorical variables as.factors; 
# temporal ordering - sort (earliest date)
#start with a GLM - estimate coeff >> basic starting point << assumes linear relationship, unrelated variable
# crossvalidation

#create dataframe so that generation data is in columns (1 time, 1 node, 3 columns of generation data)
#do we want to look at this by percent?

g <- read.csv('combined.gen.data.csv')

collapse_data <- data.frame()
uniq_times <- as.character(unique(g$timestamp))
for (time in uniq_times){
  time.idxs  <- which(g$timestamp == time)
  time.dat   <- g[time.idxs,]
  uniq.nodes <- as.character(unique(time.dat$node_id))
  
  for (node in uniq.nodes){
    node.dat <- time.dat[time.dat$node_id == node,]
    row.base <- node.dat[1,c('ba_name', 'freq', 'market','timestamp','node_id')]
    
    mw.vals <- as.data.frame(t(node.dat$gen_MW))
    print(mw.vals)
    
    names(mw.vals) <- paste0(node.dat$fuel_name, '_MW')
    row.base       <- cbind(row.base, mw.vals)
    collapse_data  <- rbind(collapse_data, row.base)
    row.base <- NULL
  }
}

#write to csv

write.csv(collapse_data, file = "collapse_data.csv")

# convert from UTC to Pacific Time

c_data <- read.csv('collapse_data.csv')

library(lubridate)

timestamp <- c_data$timestamp 

pb.txt <- timestamp
pb.date <- as.POSIXct(pb.txt, tz="UTC")
attributes(pb.date)$tzone <- "America/Los_Angeles"

c_data$PTime <- pb.date

# Write to csv

write.csv(c_data, file = "combinedgendata.csv")

library("dplyr")

cg <- read.csv('combinedgendata.csv')
dfc <- cg %>%
  mutate(time = ymd_hms(PTime),
         date = date(time),
         hour = hour(time),
         month = month(time)
  )

# merge with existing CAISO data

loadlmp5 <- read.csv('combinedloadlmpdata.csv') #i think this is the 5 min file - disregard. 

head(loadlmp)

t <- merge(loadlmp, dfc, by = c("timestamp", "node_id"), all = TRUE)

write.csv (t, file = "combinedhourly_load_lmp_gen_data.csv")

#merge with CAISO + weather data

c2 <- read.csv('hourlyweatherandlmp.csv')

mega <- merge(c2, dfc, by = c("date", "hour", "node_id"), all = TRUE)

write.csv(mega, file = 'combinedhourly_weather_load_lmp_gen.csv')
