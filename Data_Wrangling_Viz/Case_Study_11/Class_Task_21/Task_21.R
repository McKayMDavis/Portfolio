library(leaflet)
library(USAboundaries)
library(tidyverse)
library(htmltools)

cities <- us_cities() %>% 
  filter(state != "AK", state != "HI", state != "PR", lat != 0) %>%
  arrange(state, desc(population)) %>%
  group_by(state) %>% 
  filter(population >= nth(population, 3)) %>% 
  mutate("size_id" = rep(1:3))

idaho <- us_counties(states = "Idaho")

states <- us_states() %>% 
  filter(name != "Alaska", name != "Hawaii", name != "Puerto Rico")


#--------------------------------------------------------------------------
#This doesn't work for some odd reason
getColor <- function(cities) {
  sapply(cities$size_id, function(size_id) {
    if (size_id = 1) {
      "darkblue"} 
    
    else if (size_id = 2) {
      "blue"}
    
    else {
      "lightblue"}
    })
}

#This doesn't work because the above doesn't work
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(cities)
)

leaflet(cities) %>%
  addTiles() %>%
  addMarkers(~lon, ~lat, label = ~htmlEscape(paste(city, " ", population)),
             clusterOptions = markerClusterOptions()) %>% 
  addProviderTiles(providers$Stamen.TonerLite) %>% 
  addPolygons(data = idaho, color = "black")
  

