library(rio)
library(tidyverse)
library(lubridate)
library(riem)
library(ggthemes)

#---------------------------------------------------------------------------------------------------
#READ DATA
#---------------------------------------------------------------------------------------------------
carwash <- import("https://byuistats.github.io/M335/data/carwash.csv") %>% as_tibble()
weath <- riem_measures(station = "RXE",  date_start  = "2016-05-13",  date_end  =  "2016-07-08")

#---------------------------------------------------------------------------------------------------
#CLEANUP
#---------------------------------------------------------------------------------------------------
#bill <- carwash %>% 
 # mutate(Time = as_datetime(ymd_hms(gsub("[aA-zZ]", " ", .$time)))) %>% 
  #mutate(hours = ceiling_date(Time, unit = "hours")) %>% 
  #select(name, Time, hours, amount) %>% 
  #filter(amount != 0.00) %>% 
  #group_by(hours) %>% 
  #mutate(average = mean(amount))

bill <- carwash %>% 
  mutate(Time = with_tz(as.POSIXct(gsub("[aA-zZ]", " ", .$time), "GMT"), "America/Boise")) %>% 
  mutate(hours = ceiling_date(Time, unit = "hours")) %>% 
  select(name, Time, hours, amount) %>% 
  filter(amount != 0.00) %>% 
  group_by(hours) %>% 
  mutate(average = mean(amount))


matt <- weath %>% 
  mutate(time = with_tz(valid, "America/Boise")) %>% 
  mutate(hours = ceiling_date(time, unit = "hours"))

weath_car <- bill %>% 
  left_join(matt) %>% 
  select(name, Time, hours, amount, average, tmpf) %>% 
  filter(!is.na(tmpf))
weath_car$temp <- weath_car$tmpf
weath_car$temp[weath_car$tmpf < 50] <- "Cool"
weath_car$temp[weath_car$tmpf >= 50 & weath_car$tmpf < 70] <- "Moderate"
weath_car$temp[weath_car$tmpf >= 70] <- "Warm"

#---------------------------------------------------------------------------------------------------
#VISUALIZATION
#---------------------------------------------------------------------------------------------------
plot <- weath_car %>% 
  filter(hour(hours) > 10, amount > 1) %>% 
  group_by(temp) %>% 
  mutate(avamtmp = mean(amount)) %>% 
  ggplot(aes(y = average, x = hour(hours), col = tmpf)) +
    geom_jitter() +
    geom_smooth() +
    geom_hline(aes(yintercept = avamtmp), color = "darkblue") +
    theme_solarized_2() +
    facet_grid(. ~ temp) +
    coord_cartesian(ylim = c(0, 150)) +
    labs(x = "Hours (Military)", y = "Average Sale Amount", col = "Temp. (Farenheit)", title = "Temperatures and Sales:\nCool Probably Not Good for Carwash")
    
ggsave("weathCar.png", plot = plot, path = "./Case_Study_07/Class_Task_14", width = 20, height = 8)

plot1 <- weath_car %>% 
  filter(hour(hours) > 10, amount > 1) %>% 
  group_by(temp) %>% 
  mutate(avamtmp = mean(amount)) %>% 
  ggplot(aes(y = amount, x = hour(hours), col = tmpf)) +
  geom_jitter() +
  geom_smooth() +
  geom_hline(aes(yintercept = avamtmp), color = "darkblue") +
  theme_solarized_2() +
  facet_grid(. ~ temp) +
  coord_cartesian(ylim = c(0, 150)) +
  labs(x = "Hours (Military)", y = "Average Sale Amount", col = "Temp. (Farenheit)", title = "Temperatures and Sales:\nCool Probably Not Good for Carwash")

ggsave("weathCarTot.png", plot = plot1, path = "./Case_Study_07/Class_Task_14", width = 20, height = 8)

  
weath_car %>%
  filter(hour(hours) > 10, amount > 1) %>% 
  group_by(temp) %>% 
  mutate(avamtmp = mean(amount)) %>% 
  ggplot(aes(y = amount, x = temp, col = tmpf)) +
    geom_jitter() +
    geom_boxplot() +
    theme_solarized_2() +
    facet_grid(. ~ hour(hours)) +
    coord_cartesian(ylim = c(0, 150))
  

