library(riem)
library(tidyverse)
library(lubridate)
library(forcats)
library(ggthemes)

#DO NOT RUN THIS CODE!!!
#measures = list()
#for (i in seq_along(riem_networks()$code)) {
#  measures[[i]] <- riem_measures(station = riem_stations(network = riem_networks()$code[i])$id[i], date_start = "2015-06-01", date_end = "2017-07-01")
#}

#measures_big <- dplyr::bind_rows(measures)
RXE <- riem_measures(station = "RXE", date_start = "2015-06-01", date_end = "2017-07-01")
#measures_big2 <- rbind(measures_big, RXE)

#saveRDS(measures_big2, "./Case_Study_07/Class_Task_13/lotsodata.Rds")
saveRDS(RXE, "./Case_Study_07/Class_Task_13/RXE.Rds")

RXE <- readRDS("./Case_Study_07/Class_Task_13/RXE.Rds")


orders <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
p1 <- RXE %>%
  ggplot(aes(x = factor(weekdays(.$valid, abbreviate = TRUE), levels = orders), y = tmpf)) +
    geom_violin() +
    theme_solarized_2(light = FALSE) +
    labs(x = "Week-Day", y = "Temperature (Farenheit)", title = "Day With Highest Distribution\nTemperature Mostly Inconclusive")

p2 <- RXE %>%
  filter(!is.na(tmpf)) %>% 
  mutate(weekday = factor(weekdays(.$valid, abbreviate = TRUE), levels = orders)) %>% 
  group_by(weekday) %>% 
  mutate(maxtemp = max(tmpf)) %>% 
  ggplot(aes(x = weekday, y = maxtemp)) +
  geom_point(color = "lightblue") +
  theme_solarized_2(light = FALSE) +
  labs(x = "Week-Day", y = "Temperature (Farenheit)", title = "Day With Highest Temperature")

  
p3 <- RXE %>%
  ggplot(aes(x = hour(.$valid), y = tmpf)) +
  geom_smooth() +
  geom_vline(aes(xintercept = 14), color = "lightblue") +
  theme_solarized_2(light = FALSE) +
  facet_grid(. ~ factor(weekdays(.$valid, abbreviate = TRUE), levels = orders)) +
  labs(x = "Hour (Military)", y = "Temperature (Farenheit)", title = "Wednesday with Lowest\nDistribution of Temperatures at 2pm")

ggsave("distTemp.png", plot = p1, path = "./Case_Study_07/Class_Task_13")
ggsave("maxTemp.png", plot = p2, path = "./Case_Study_07/Class_Task_13")
ggsave("days2pm.png", plot = p3, path = "./Case_Study_07/Class_Task_13")


