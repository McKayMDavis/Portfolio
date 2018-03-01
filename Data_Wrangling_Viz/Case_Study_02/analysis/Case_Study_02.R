library(tidyverse)
library(gapminder)


View(gapminder)

gapminder1 <- gapminder

gapminder1 <- gapminder1 %>%
  group_by(year, continent) %>%
  mutate(wmgdp = weighted.mean(gdpPercap, pop))

colnames(gapminder1) <- c("Country", "Continent", "Year", "Life Expectancy", "Population (100k)", "GDP per capita", "wmgdp")

gapminder1$`Population (100k)` <- gapminder1$`Population (100k)` * .00001


p1 <-
ggplot(data = gapminder1, aes(x = `Life Expectancy`, y = `GDP per capita`, size = `Population (100k)`, col = Continent)) +
  geom_point()  +
  facet_grid(. ~ Year) +
  coord_cartesian(ylim = c(0, 50000)) +
  scale_y_continuous(trans = "sqrt") +
  theme_bw()


p2 <-
ggplot(data = gapminder1, aes(x = Year, y = `GDP per capita`)) +
  geom_point(aes(col = Continent, size = `Population (100k)`))  +
  geom_line(aes(group = Country, col = Continent)) +
  geom_point(aes(y = wmgdp, size = `Population (100k)`)) +
  geom_line(aes(y = wmgdp, group = Country)) +
  facet_grid(. ~ Continent) +
  scale_y_continuous(limits = c(0, 50000)) +
  theme_bw()

ggsave("CS2_P1.png", plot = p1, path = "./Case_Study_02/analysis", width = 15, units = "in")
ggsave("CS2_P2.png", plot = p2, path = "./Case_Study_02/analysis", width = 15, units = "in")





