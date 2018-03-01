library(readr)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(forcats)
library(knitr)

dirty.dat <- read_csv("https://byuistats.github.io/M335/data/sales.csv") %>% as_tibble()

clean.dat <- dirty.dat %>%
  filter(Name != "Missing", weekdays(Time, abbreviate = FALSE) != "Sunday" & weekdays(Time, abbreviate = FALSE) != "Saturday" ) %>% #consider removing sunday observations totalling $166
  mutate(Time = with_tz(Time, "America/Boise"))

clean.dat.hr <- clean.dat %>% 
  group_by(Name, Type, hour(Time), weekdays(Time, abbreviate = TRUE)) %>% 
  summarise(Amount = sum(Amount, na.rm = TRUE)) %>% 
  rename(Hour = `hour(Time)`, Day = `weekdays(Time, abbreviate = TRUE)`)

clean.dat.min <- clean.dat %>%
  group_by(Name, Type, hour(Time), minute(Time), weekdays(Time, abbreviate = TRUE)) %>% 
  summarise(Amount = sum(Amount, na.rm = TRUE)) %>% 
  rename(Hour = `hour(Time)`, Minute = `minute(Time)`, Day = `weekdays(Time, abbreviate = TRUE)`)

orders <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

#Total revenue over course of survey period
clean.dat.rev <- clean.dat %>%
  group_by(Name) %>% 
  summarise(`Total Revenue` = sum(Amount, na.rm = TRUE))
kable(clean.dat.rev)

#Total amount sold at each hour for each day of the week across full length of
#survey period.
clean.dat.hr %>% 
  ggplot(aes(y = Amount, x = Hour, col = Name)) +
    geom_point(size = .5) +
    geom_line() +
    facet_grid(Name ~ factor(Day, levels = orders)) +
    coord_cartesian(ylim = c(-200, 1500), xlim = c(7, 24)) +
    scale_x_continuous(breaks = c(7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)) +
    theme_fivethirtyeight() +
    theme(legend.position = "none", axis.text.x = element_text(size = 6)) +
    labs(title = "Hot Diggity\nHighest Total Hourly Revenue Across Days")

#Traffic
clean.dat %>%
  group_by(Name, Type) %>% 
  mutate(Hour = hour(Time), Day = weekdays(Time, abbreviate = TRUE)) %>% 
  filter(Hour < 16 & Hour > 8) %>% 
  ggplot(aes(x = Hour, col = Type)) +
    geom_density() +
    facet_grid(Name ~ factor(Day, levels = orders)) +
    theme_fivethirtyeight() +
    theme(axis.text.x = element_text(size = 6)) +
    guides(color = guide_legend(override.aes = list(size = 2))) +
    labs(title = "Density of Sales\nConsistent in Sales Densities | primetime")

clean.dat %>%
  group_by(Name, Type) %>% 
  mutate(Hour = hour(Time), Day = weekdays(Time, abbreviate = TRUE)) %>% 
  filter(Hour < 19 & Hour > 8) %>% 
  ggplot(aes(x = Hour, col = Type)) +
    geom_density() +
    facet_grid(Name ~ factor(Day, levels = orders)) +
    theme_fivethirtyeight() +
    theme(axis.text.x = element_text(size = 6)) +
    guides(color = guide_legend(override.aes = list(size = 2))) +
    labs(title = "Density of Sales\nConsistent in Sales Densities | till 5")

clean.dat.day <- clean.dat %>%
  group_by(Name, Type, yday(Time), weekdays(Time, abbreviate = TRUE)) %>% 
  summarise(Amount = sum(Amount), na.rm = TRUE) %>% 
  rename(Dayyr = `yday(Time)`, Day = `weekdays(Time, abbreviate = TRUE)`)

clean.dat.day %>% 
  filter(Dayyr > 125) %>% 
  ggplot(aes(y = Amount, x = Dayyr, col = Type)) +
    geom_point() +
    geom_smooth(color = "black") +
    facet_grid(. ~ Name) +
    theme_bw()

