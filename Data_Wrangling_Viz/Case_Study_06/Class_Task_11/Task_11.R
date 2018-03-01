library(tidyverse)
library(forcats)
library(readr)
library(ggplot2)
library(ggthemes)

stock <- readRDS("./Case_Study_04/Class_Task_08/tidyDowRds.Rds")

month_levels <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

stock %>%
  filter(!is.na(year_end)) %>%
  mutate(month_endI = factor(month_end, levels = month_levels)) %>%
  ggplot(aes(y = value, x = month_endI, col = variable)) +
    geom_point() +
    facet_wrap( ~ year_end) +
    theme_bw() +
    theme_fivethirtyeight() +
    theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1)) +
    labs(y = "returns", x = "year of return", title = "Index Return Accuracy of Darts vs Pros")

