library(readr)
library(tidyverse)

gun <- read_csv("./Case_Study_03/analysis/full_data.csv")

gun$Season = NA

gun$Season[gun$month %in% c("03", "04", "05")] <- "Spring"
gun$Season[gun$month %in% c("06", "07", "08")] <- "Summer"
gun$Season[gun$month %in% c("09", "10", "11")] <- "Fall"
gun$Season[gun$month %in% c("12", "01", "02")] <- "Winter"
gun <- subset(gun, select = -c(X1))
gun <- subset(gun, !is.na(intent))
gun$police1 = NA
gun$police1[gun$police == 1] <- "By-Police"
gun$police1[gun$police == 0] <- "Non-Police"

#gun %>%
#  group_by(intent, police1, sex, race) %>% 
#  summarise(count = n()) %>%
#  ggplot(aes(x = intent, y = count, col = sex)) +
#    geom_jitter() +
#    scale_y_continuous(trans = "sqrt") +
#    facet_grid(race ~ police1) +
#    theme_bw() +
#    theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1))

# Deaths by type, race, gender, and whether it was by police
gun %>%
  ggplot(aes(x = intent, fill = police1)) +
    geom_bar() +
    facet_grid(race ~ sex) +
    scale_y_continuous(trans = "sqrt") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1)) +
    labs(title = "Death Intent by Gender and Police Homicide", y = "Deaths (sqrt trans.)", x = "Death Intent", fill = "Police Committed")

# Deaths by Type, Season and Race
gun %>%
  ggplot(aes(x = Season, fill = intent)) +
    geom_bar() +
    facet_grid(race ~ intent) +
    scale_y_continuous(trans = "sqrt") +
    theme_bw() + theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1)) +
    labs(title = "Death Season by Intent and Race", y = "Deaths (sqrt trans.)")


#gun %>%
#  group_by(intent, race, Season) %>% 
#  summarise(count = n()) %>%
#  ggplot(aes(x = Season, y = count, col = race)) +
#    geom_jitter(shape = 95, size = 5) +
#    scale_y_continuous(trans = "sqrt") +
#    facet_grid(. ~ intent) +
#    theme_bw() +
#    theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1))

# Death Type and Season
gun %>%
  ggplot(aes(x = Season)) +
  geom_bar() +
  facet_grid(. ~ intent) +
  scale_y_continuous(trans = "sqrt") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1)) +
  labs(title = "Death Season by Intent", y = "Deaths (sqrt trans.)")


# Deaths by Race and Type
gun %>%
  ggplot(aes(x = race)) +
  geom_bar() +
  facet_grid(. ~ intent) +
  scale_y_continuous(trans = "sqrt") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1)) +
  labs(title = "Death Race by Intent", y = "Deaths (sqrt trans.)", x = "Race")


#if (gun1$month %in% c("03", "04", "05")) {
 # gun1$Season <- "Spring"
#} else if (gun1$month %in% c("06", "07", "08")) {
 # gun1$Season <- "Summer"
#} else if (gun1$month %in% c("09", "10", "11")) {
 # gun1$Season <- "Fall"
#} else if (gun1$month %in% c("12", "01", "02")) {
 # gun1$Season <- "Winter"
#}


