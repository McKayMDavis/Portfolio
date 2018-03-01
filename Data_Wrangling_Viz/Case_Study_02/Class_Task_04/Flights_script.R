library(nycflights13)
library(tidyverse)
library(ggplot2)
library(mosaic)

flights_noon <- subset(flights, dep_time < 1200)
flights_airline_noon <- merge(flights_noon, airlines)

ggplot(data = subset(flights_airline_noon, dep_delay < 150 & dep_delay > 0), aes(x = name, y = dep_delay, col = name)) +
  geom_boxplot(show.legend = F)  +
  theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1)) +
  facet_wrap(~origin, ncol = 4) +
  labs(title = "Origin Airport Departure Delay Times")

flights_airline <- merge(flights, airlines)
flights_airline_delta <- subset(flights_airline, name == "Delta Air Lines Inc.")

ggplot(data = subset(flights_airline_delta, arr_delay < 200 & arr_delay > 0), aes(x = origin, y = arr_delay, col = origin)) +
  geom_boxplot(show.legend = F) +
  labs(title = "Origin Airport Arrival Delay Times -- Delta")

bob <- data.frame(arr_delay = flights$arr_delay, dest = flights$dest)
sally <- bob[complete.cases(bob), ]
arrivals <- aggregate(sally$arr_delay, list(sally$dest), mean)
colnames(arrivals) <- c("dest", "arr_delay_mean")

ggplot(data = arrivals, aes(x = dest, y = arr_delay_mean, col = dest)) + 
  geom_point(show.legend = F) + 
  geom_text(aes(label = ifelse(arr_delay_mean > 40,as.character(dest),'')),hjust = -.5, vjust = 0, show.legend = F) +
  theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1)) +
  labs(title = "Worst Destination Airport By Arrival Delay")
