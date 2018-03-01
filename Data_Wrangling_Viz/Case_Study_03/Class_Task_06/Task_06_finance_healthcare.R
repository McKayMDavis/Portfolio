library(ourworldindata)
library(tidyverse)
library(gridExtra)

p1 <- financing_healthcare %>%
  filter(country != "World") %>%
  rename(`Total Healthcare Expenditure` = health_exp_total, `Child Mortality` = child_mort, Year = year) %>%
  ggplot(aes(x = Year, y = `Child Mortality`, col = `Total Healthcare Expenditure`)) +
    geom_point()  +
    stat_smooth(color = "darkred") +
    theme_bw() +
    labs(title = "Child Mortality")
  
p2 <- financing_healthcare %>%
  filter(country != "World") %>%
  rename(`Total Healthcare Expenditure` = health_exp_total, `Child Mortality` = child_mort, Year = year) %>%
  ggplot(aes(x = Year, y = `Child Mortality`, col = `Total Healthcare Expenditure`)) +
    geom_jitter()  +
    stat_smooth(color = "darkred") +
    coord_cartesian(x = c(1990, 2017), y = c(-5, 400)) +
    theme_bw() +
    labs(title = "Child Mortality Zoomed")

grid.arrange(p1, p2, ncol = 1)

