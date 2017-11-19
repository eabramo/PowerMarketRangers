v <- read.csv('combinedloadlmpdata.csv')
v


library("lubridate")
library("dplyr")
library("ggplot2")

head(v)

v %>%
  head

v %>%
  head() %>%
  summary


head(head(v))


## merge price and volume

dfX <- v %>%
  mutate(time = ymd_hms(PTime.x),
         date = date(time),
         hour = hour(time),
         month = month(time)
         )

## calculate total value= price *  vol

## how will missing 5m incs impact this?

dfX2 <- dfX %>%
  group_by(node_id, month, date, hour) %>%
  summarize(lmp = sum(lmp),
            load_MW = sum(load_MW),
            n = n()
            )
  # sum value
  # sum units


dfX2 %>% 
  arrange(-n) %>%
  head

# calc avg price

dfX2$houravglmp <- (dfX2$lmp*dfX2$load_MW)/dfX2$load_MW


library("ggplot2")

ggplot(dfX2) +
  geom_line(aes(x = hour,
                y = houravglmp,
                group = factor(date)
                ),
            alpha = .2
            ) +
  facet_wrap(~ month)

ggplot(dfX2) +
  geom_line(aes(x = load_MW,
                y = lmp,
                group = factor(date)
  ),
  alpha = .2
  ) +
  facet_wrap(~ month)

# add in wind and solar data 

w <- read.csv('CAweatherhour2.csv')
head(w)
summary(w)
dim(w)

date <- w$Date
pb.txt <- date
pb.date <- as.POSIXct(pb.txt, tz="")

w$date <- pb.date


dfXW <- merge (dfX2, w, by = c("date", "hour"), all = TRUE)

write.csv(dfXW, file = 'hourlyweatherandlmp.csv')
