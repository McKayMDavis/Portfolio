library(USAboundaries)
library(sf)
library(ggrepel)
library(ggplot2)
library(tidyverse)
library(ggforce)

cities <- us_cities() %>% 
  filter(state != "AK", state != "HI", state != "PR", lat != 0) %>%
  arrange(state, desc(population)) %>%
  group_by(state) %>% 
  filter(population >= nth(population, 3)) %>% 
  mutate("size_id" = rep(1:3))

my_proj <- "+proj=moll +lat_0=45 +lon_0=-115 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

idaho <- us_counties(states = "Idaho") %>% 
  st_transform(crs = my_proj)

my_proj <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

states <- us_states() %>% 
  filter(name != "Alaska", name != "Hawaii", name != "Puerto Rico") %>% 
  st_transform(crs = my_proj)



bob <- 
ggplot() +
  geom_sf(fill = NA, data = states) +
  geom_sf(fill = NA, data = idaho) +
  #geom_point(data = cities, aes(y = lat, x = lon, size = population/1000, col = size_id)) +
  #geom_label_repel(data = filter(cities, size_id == 1), size = 4, color = "darkblue", aes(y = lat, x = lon, label = city)) +
  #labs(y = "", x = "", size = "Population\n(1,000)") +
  #guides(color = FALSE) +
  theme_bw()

ggsave("bob.png", plot = bob, path = "./Case_Study_10/Class_Task_19", width = 18, height = 10)
