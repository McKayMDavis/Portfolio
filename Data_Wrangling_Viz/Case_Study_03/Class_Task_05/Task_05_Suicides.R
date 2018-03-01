library(tidyverse)
library(ourworldindata)

#SUICIDE DATA

suicides <- read.csv("~/Documents/git/Math_335/Case_Study_03/Class_Task_05/male-and-female-suicide-rate.csv", header = TRUE)

suicides1 <- subset(suicides, Entity == "United States" | Entity == "Japan")

colnames(suicides1) <- c("Country", "Code", "Year", "Male", "Female")

suicides1 <- gather(suicides1, Gender, Suicide.Rate, Male:Female, factor_key=TRUE)

ggplot(data = suicides1, aes(y = Suicide.Rate, x = Year, col = Country, shape = Gender)) +
  geom_point() +
  geom_line(aes(group = interaction(Country, Gender)))


#CHILD MORTALITY DATA

#latlong <- read.csv("~/Documents/git/Math_335/Case_Study_03/Class_Task_05/Lat_Long.csv", header = TRUE)

#latlong$useless <- sapply(strsplit(as.character(latlong$Country), split = '[', fixed=TRUE), function(x) (x[2]))
#latlong$Country1 <- sapply(strsplit(as.character(latlong$Country), split = '[', fixed=TRUE), function(x) (x[1]))
#latlong$Country2 <- sapply(strsplit(as.character(latlong$Country), split = ']', fixed=TRUE), function(x) (x[2]))
#latlong$Country <- paste(latlong$Country1[!is.na(latlong$Country1)], " ", latlong$Country2[!is.na(latlong$Country2)])

#latlong <- data.frame(latlong$Lat, latlong$Long, latlong$Country)
#colnames(latlong) <- c("lat", "long", "country")

#Mortality <- merge(child_mortality, latlong)

#colnames(Mortality)[1] <- "region"

#ggplot() +
#geom_map(data = Mortality, map = Mortality, aes(x = long, y = lat, group = child_mort, map_id = region), fill="white", colour="#7f7f7f", size=0.5)


ggplot(data = child_mortality, aes(y = child_mort, x = year, col = continent)) +
  geom_point(size = 0.05) +
  facet_grid(. ~ continent)
  
  

  
  
  
  
  