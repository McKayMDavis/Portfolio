library(tidyverse)
library(ggplot2)

diamonds %>% 
  ggplot(aes(x = price, y = carat)) +
    geom_point(color = "limegreen") +
    geom_smooth() +
    facet_wrap(~ color)

diamonds %>% 
  ggplot(aes(y = price, x = carat)) +
    geom_point() +
    facet_grid(. ~ clarity)

diamonds %>% 
  ggplot(aes(y = carat, x = clarity)) +
    geom_boxplot()
