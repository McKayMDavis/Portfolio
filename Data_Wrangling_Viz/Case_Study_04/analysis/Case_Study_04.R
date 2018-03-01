library(readxl)
library(haven)
library(tidyverse)
library(foreign)

#read in data
w20 <- read_xlsx("./Case_Study_04/analysis/Height.xlsx")

g19 <- read_dta("https://byuistats.github.io/M335/data/heights/germanconscr.dta")
b19 <- read_dta("https://byuistats.github.io/M335/data/heights/germanprison.dta")
g18 <- read.dbf("./Case_Study_04/analysis/B6090.DBF")

us20 <- read_csv("https://github.com/hadley/r4ds/raw/master/data/heights.csv")

nat <- read_sav("http://www.ssc.wisc.edu/nsfh/wave3/NSFH3%20Apr%202005%20release/main05022005.sav")



#format world 20th century
colnames(w20) <- w20[2,]
w20 <- w20[-c(1,2),]
w20 <- w20 %>%
  gather(3:length(w20), key = "year", value = "height") %>%
  rename(code = Code, country = `Continent, Region, Country`, year_decade = year, height_cm = height) %>%
  mutate(height_in = height_cm / 2.54) %>%
  separate(year_decade, into = c("century", "decade", "year"), sep = c(2,3,4), remove = FALSE) %>%
  select(code, country, century, decade, year, year_decade, height_in, height_cm)
  
  #mutate(study = "w20") %>%
  #rename(birth_year = year, height_cm = height) %>%
  #mutate(height_in = height_cm / 2.54) %>%
  #mutate(birth_year = as.character(birth_year), study = as.character(study), height_in = as.numeric(height_in), height_cm = as.numeric(height_cm)) %>%
  #select(birth_year, study, height_in, height_cm)

#format german 19th century
g19 <- g19 %>%
  mutate(study = "g19") %>%
  rename(birth_year = bdec, height_cm = height) %>%
  mutate(height_in = height_cm / 2.54) %>%
  mutate(birth_year = as.character(birth_year), study = as.character(study), height_in = as.numeric(height_in), height_cm = as.numeric(height_cm)) %>%
  select(birth_year, study, height_in, height_cm)
  
#format german 18th century
g18 <- g18 %>%
  mutate(study = "g18") %>%
  rename(birth_year = GEBJ, height_cm = CMETER) %>%
  mutate(height_in = height_cm / 2.54) %>%
  mutate(birth_year = as.character(birth_year), study = as.character(study), height_in = as.numeric(height_in), height_cm = as.numeric(height_cm)) %>%
  select(birth_year, study, height_in, height_cm)

#format bavarian 19th century
b19 <- b19 %>%
  mutate(study = "b19") %>%
  rename(birth_year = bdec, height_cm = height) %>%
  mutate(height_in = height_cm / 2.54) %>%
  mutate(birth_year = as.character(birth_year), study = as.character(study), height_in = as.numeric(height_in), height_cm = as.numeric(height_cm)) %>%
  select(birth_year, study, height_in, height_cm)

#format US 20th century
us20 <- us20 %>%
  mutate(study = "us20", birth_year = "1950") %>%
  rename(height_in = height) %>%
  mutate(height_cm = height_in * 2.54) %>%
  mutate(birth_year = as.character(birth_year), study = as.character(study), height_in = as.numeric(height_in), height_cm = as.numeric(height_cm)) %>%
  select(birth_year, study, height_in, height_cm)

#format national survey
nat <- nat %>%
  mutate(study = "nat") %>%
  rename(birth_year = DOBY, height_in = RT216I) %>%
  mutate(height_cm = height_in * 2.54) %>%
  mutate(birth_year = as.character(birth_year), study = as.character(study), height_in = as.numeric(height_in), height_cm = as.numeric(height_cm)) %>%
  select(birth_year, study, height_in, height_cm)
nat$birth_year <- paste("19", sep = "", nat$birth_year)


#concatenate data
heights <- bind_rows(b19, g18, g19, us20, nat)


#save data
saveRDS(w20, "Case_Study_04/analysis/World20.Rds")
saveRDS(heights, "Case_Study_04/analysis/StudyHeights.Rds")


#plots

w201 <- subset(w20, century != 20)
w201 %>%
  ggplot(aes(y = height_in, x = decade)) +
    geom_point() +
    facet_grid(. ~ century) +
    geom_point(data = subset(w201, country %in% c("German", "Germany")), aes(col = country)) +
    theme_bw()
    

heights %>%
  separate(birth_year, into = c("century", "decade", "year"), sep = c(2,3), remove = FALSE) %>%
  filter(decade != "-" & decade != "N") %>%
  ggplot(aes(y = height_in, x = decade, col = study)) +
    geom_jitter() +
    facet_grid(study ~ century) +
    theme_bw()




