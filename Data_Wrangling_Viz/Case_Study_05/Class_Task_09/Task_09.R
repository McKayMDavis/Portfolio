library(Lahman)
library(tidyverse)
library(ggplot2)
library(blscrapeR)

infl <- inflation_adjust(2015)

#Descriptives
levels(as.factor(CollegePlaying$schoolID))


#USE Salaries, CollegePlaying
#FIND utah colleges @ https://github.com/chadwickbureau/baseballdatabank/blob/master/core/Schools.csv

CP <- CollegePlaying %>%
  select(playerID, schoolID)

colleges <- merge(Salaries, CP, by = c("playerID"))

coldat <- colleges %>%
  filter(schoolID %in% c("byu", "utah", "utahst", "utahvalley", "utdixie", "eutah", "sutah"))

infl <- infl %>%
  filter(year >= 1985, year <= 2015) %>%
  rename(yearID = year) %>%
  select(yearID, adj_value)

coldat <- merge(coldat, infl, by = "yearID")

coldat <- coldat %>%
  mutate(salary_adj = salary / adj_value)

saveRDS(coldat, "./Case_Study_05/Class_Task_09/bBall_College_and_Salary.Rds")


coldat %>%
  ggplot(aes(y = salary_adj, x = schoolID)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 5500000)) +
  theme_bw()

plot <- coldat %>%
  ggplot(aes(y = salary_adj, x = schoolID)) +
    geom_point() +
    geom_hline(data = filter(coldat, schoolID == "byu"), aes(yintercept = mean(salary_adj)), color = "blue") +
    geom_hline(data = filter(coldat, schoolID == "utdixie"), aes(yintercept = mean(salary_adj)), color = "red") +
    coord_cartesian(ylim = c(0, 6000000)) +
    theme_bw() +
    labs(title = "BYU Average Adjusted Salary Highest", y = "Inflation Adjusted Salary", x = "Utah Schools")

ggsave("bBallSalaries.png", plot = plot, path = "./Case_Study_05/Class_Task_09/")
