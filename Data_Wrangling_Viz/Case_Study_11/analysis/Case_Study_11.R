library(leaflet)
library(USAboundaries)
library(tidyverse)
library(htmltools)
library(rio)
library(viridis)
library(ggthemes)
#--------------------------------------------------------------
# Visualize us totals
#--------------------------------------------------------------
permits_us <- import("Case_Study_10/analysis/permits.rda") %>% 
  filter(variable == "Single Family") %>% 
  mutate(county_name = sapply(strsplit(.$countyname, split = " ", fixed = TRUE), function(x) (x[1]))) %>% 
  rename(countyfp = county)

counties_us <- us_counties() %>% 
  mutate(countyfp = as.integer(countyfp)) %>% 
  rename(StateAbbr = state_abbr)

us_data <- permits_us %>% 
  left_join(counties_us, by = c("StateAbbr", "countyfp"))


us_data %>%
  group_by(year) %>% 
  summarise(vals = sum(value)) %>% 
  ggplot(aes(y = vals, x = year, col = year)) +
  geom_line(color = "black", size = 2) +
  geom_line(color = "white") +
  geom_line() +
  geom_vline(xintercept = 2005, size = 1, color = "white", alpha = 0.5) +
  geom_vline(xintercept = 2005) +
  scale_color_viridis() +
  theme_hc(bgcolor = "darkunica") +
  theme(legend.position = "none") +
  labs(y = "Building Permit Value", 
       x = "Year", 
       title = "Building Permit Value Plummets at 2005")
#--------------------------------------------------------------
# Visualize us map
#--------------------------------------------------------------
permits_us2 <- import("Case_Study_10/analysis/permits.rda") %>% 
  filter(variable == "Single Family") %>% 
  select(StateAbbr, year, value) %>% 
  group_by(StateAbbr, year) %>% 
  summarize(value = sum(value))

us_states <- us_states() %>% 
  rename(StateAbbr = state_abbr)

us_data2 <- permits_us2 %>% 
  left_join(us_states, by = c("StateAbbr")) %>% 
  filter(year %in% c(2005, 2006, 2007, 2008, 2009, 2010)) %>% 
  select(StateAbbr, year, value, geometry) %>% 
  spread(year, value) %>% 
  mutate(diff = `2005` - `2010`) %>%
  st_sf()

#2005-2010 diffs
bins <- c(-52, 0, 1000, 5000, 10000, 20000, 50000, 100000, 200000)
pal <- colorBin("viridis", domain = us_data2$diff, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g deficit",
  us_data2$StateAbbr, us_data2$diff
) %>% lapply(htmltools::HTML)

leaflet(us_data2) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~pal(diff),
              weight = 2,
              opacity = 1,
              color = "black",
              fillOpacity = 5,
              highlight = highlightOptions(weight = 5,
                                           color = "#666",
                                           dashArray = "",
                                           fillOpacity = 0.7,
                                           bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "15px",
                                          direction = "auto")) %>% 
  addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
            position = "bottomright") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>% 
  setView(-71.0382679, 42.3489054, zoom = 5)

#--------------------------------------------------------------
# Visualize AZ totals
#--------------------------------------------------------------
az_data <- us_data %>% 
  filter(StateAbbr == "AZ", county_name == "Maricopa")

az_data %>% 
  ggplot(aes(y = value, x = year, col = year)) +
  geom_line(color = "black", size = 2) +
  geom_line(color = "white") +
  geom_line() +
  geom_vline(xintercept = 2005, size = 1, color = "white", alpha = 0.5) +
  geom_vline(xintercept = 2005) +
  geom_text(aes(x = 2002, y = 25000, label = "2004"), color = "yellow") +
  geom_vline(xintercept = 2004, size = 1, color = "white", alpha = 0.5) +
  geom_vline(xintercept = 2004) +
  geom_text(aes(x = 2007, y = 45000, label = "2005"), color = "yellow") +
  scale_color_viridis() +
  theme_hc(bgcolor = "darkunica") +
  theme(legend.position = "none") +
  labs(y = "Building Permit Value", 
       x = "Year", 
       title = "Building Permit Values in Maricopa AZ",
       subtitle = "Downturn Possibly Begins in 2004")

