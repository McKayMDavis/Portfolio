library(readr)
library(readxl)
library(haven)
library(downloader)
library(tidyverse)

dowRds <- read_rds(gzcon(url("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.RDS")))


download("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.xlsx", "./Case_Study_04/Class_Task_07/dow.xlsx")
dowXlsx <- read_excel("./Case_Study_04/Class_Task_07/dow.xlsx")


dowCsv <- read_csv("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.csv")


dowDta <- read_dta("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.dta")


dowSav <- read_sav("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.sav")


all.equal(dowRds, dowXlsx, check.attributes = FALSE)
all.equal(dowXlsx, dowCsv, check.attributes = FALSE)
all.equal(dowCsv, dowDta, check.attributes = FALSE)
all.equal(dowDta, dowSav, check.attributes = FALSE)

taskSevenPlot <-
ggplot(data = subset(dowRds, contest_period != "Average"), aes(x = variable, y = value)) +
  geom_boxplot(aes(col = variable)) +
  geom_point() +
  geom_point(data = subset(dowRds, contest_period == "Average"), size = 5, aes(col = variable)) +
  theme_bw()
    
ggsave("CT7.png", plot = taskSevenPlot, path = "./Case_Study_04/Class_Task_07", width = 15, units = "in")
  

