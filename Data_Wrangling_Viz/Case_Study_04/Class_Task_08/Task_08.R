library(readr)
library(readxl)
library(haven)
library(downloader)
library(tidyverse)

# read data
dowRds <- read_rds(gzcon(url("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.RDS")))
dowCsv <- read_csv("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.csv")
dowDta <- read_dta("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.dta")
dowSav <- read_sav("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.sav")
download("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.xlsx", "./Case_Study_04/Class_Task_07/dow.xlsx")
dowXlsx <- read_excel("./Case_Study_04/Class_Task_07/dow.xlsx")

# create month end and year end columns and remove contest_period
tidyDowRds <- dowRds %>%
  separate(contest_period, into = c("rids","rids2"), sep = "-") %>%
  separate(rids2, into = c("month_end", "year_end"), sep = -5) %>%
  select(-one_of("rids"))

# edit some misspelled months
tidyDowRds$month_end <- gsub("Dec[.]", "December", tidyDowRds$month_end)
tidyDowRds$month_end <- gsub("Febuary", "February", tidyDowRds$month_end)

# save resulting data.frame
saveRDS(tidyDowRds, "./Case_Study_04/Class_Task_08/tidyDowRds.Rds")

# return and mean return plotted by year
tidyDowRds %>%
  filter(!is.na(year_end)) %>%
  ggplot(aes(y = value, x = year_end, col = variable)) +
    geom_point() +
    geom_hline(data = subset(tidyDowRds, is.na(year_end)), aes(yintercept = value)) +
    facet_grid(. ~ variable) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1)) +
    theme(legend.position="none") +
    labs(y = "returns", x = "year of return", title = "Index Return Spread")

# density curve w/ mean return
tidyDowRds %>%
ggplot(aes(x=value)) + 
  #geom_histogram(aes(y = ..density.., fill = variable),      # Histogram with density instead of count on y-axis
                 #binwidth=2,
                 #colour="black", fill="white") +
  geom_density(alpha=.2, aes(fill = variable)) +
  geom_vline(data = subset(tidyDowRds, is.na(year_end)), aes(xintercept = value, col = variable), alpha = 0.4) +
  labs(y = "density", x = "returns", title = "Index Return Density with Mean Return", fill = "Index", col = "Index") +
  theme_bw()

# spread data with years as columns and months as rows
spreadTidyDowRds <- tidyDowRds %>%
  filter(variable == "DJIA" & !is.na(month_end)) %>%
  spread(key = year_end, value = value)

