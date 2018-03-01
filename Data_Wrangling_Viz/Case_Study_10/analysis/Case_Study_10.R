library(tidyverse)
library(USAboundaries)
library(rio)
library(geofacet)
library(ggthemes)

permits_us <- import("Case_Study_10/analysis/permits.rda") %>% 
  filter(variable == "Single Family") %>% 
  mutate(county_name = sapply(strsplit(.$countyname, split = " ", fixed = TRUE), function(x) (x[1]))) %>% 
  rename(countyfp = county)

counties_us <- us_counties() %>% 
  mutate(countyfp = as.integer(countyfp)) %>% 
  rename(StateAbbr = state_abbr)

us_data <- left_join(permits_us, counties_us, by = c("StateAbbr", "countyfp"))

az_data <- us_data %>% 
  filter(StateAbbr == "AZ")

us <- 
us_data %>%
  group_by(StateAbbr, year) %>%
  ggplot(aes(x = year, y = value/10000, fill = year)) +
  geom_col() +
  geom_vline(xintercept = 2005, color = "firebrick") +
  facet_geo(~ StateAbbr, scales = "free_y") +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_blank()) +
  labs(x = "Year",
       y = "Permit Value\n(/1000)",
       title = "Country-Wide Housing Collapse",
       subtitle = "Single Family Building Permits|1980 to 2010")

az <- 
az_data %>% 
  filter(year >= 2000) %>% 
  ggplot() +
  geom_sf(aes(fill = value)) +
  facet_wrap(~ year) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "right",
        legend.direction = "vertical") +
  labs(title = "Arizona Counties Housing Collapse",
       subtitle = "Single Family Building Permits|2000 to 2010")

ggsave("az.png", plot = az, path = "Case_Study_10/analysis")
ggsave("us.png", plot = us, path = "Case_Study_10/analysis", width = 18, height = 10)


