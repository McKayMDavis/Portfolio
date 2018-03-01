#-------------------------------------------------------------------------------------
#Stock Script: Library
#-------------------------------------------------------------------------------------

library(tidyverse)
library(dygraphs)
library(tidyquant)
library(xts)
library(DT)
library(ggthemes)
library(forcats)

#-------------------------------------------------------------------------------------
#Read Data
#-------------------------------------------------------------------------------------
#ticks <- c("CXW", "F", "GM", "JCP", "KR", "WDC", "NKE","T", "WDAY", "WFC", "WMT")
ticks <- c("ACBFF","RAD")
stocks <-  ticks %>% 
  tq_get(get = "stock.prices", from = "1970-01-01")

#-------------------------------------------------------------------------------------
#Create Portfolio if more than 2 stocks
#-------------------------------------------------------------------------------------
stocksMonth <- stocks %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted, 
            mutate_fun = periodReturn, 
            period = "monthly",
            col_rename = "Ra")
wts = vector()
wts <- vapply(1:length(ticks), function(x) {1/length(ticks)}, FUN.VALUE = 0.0)

stocksPort <- stocksMonth %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = Ra,
               weights = wts,
               col_rename = "Ra")

#-------------------------------------------------------------------------------------
#dygraph
#-------------------------------------------------------------------------------------
#Portfolio
#dystockPort <- stocksPort %>% 
#  xts(order.by = .$date)

#dystockPort <- dystockPort[,-1]
#dygraph(dystockPort) 

#Adjusted

colors <- c("darkorange", 
  "darkred", 
  "darkblue", 
  "darkgreen", 
  "purple4", 
  "goldenrod3",
  "brown",
  "grey",
  "black",
  "firebrick4",
  "tan")

dystock <- stocks %>%
  select(date, symbol, adjusted) %>%
  spread(symbol, adjusted) %>% 
  xts(order.by = .$date)



dystock <- dystock[,-1]
dygraph(dystock) %>% 
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y", label = "adjusted") %>%
  dyOptions(axisLineColor = "navy", 
            gridLineColor = "lightblue",
            colors = colors)

#-------------------------------------------------------------------------------------
#ggplot
#-------------------------------------------------------------------------------------
#Portfolio
stocksPort %>% 
  ggplot(aes(x = date, y = Ra)) +
    geom_line() +
    geom_smooth() +
    theme_hc() +
    labs(y = "returns", title = "Portfolio of Returns\nEach Tick Bearing Equal Weight")

#Adjusted
stocks %>%
  ggplot(aes(x = date, y = adjusted, col = symbol)) +
    geom_line() +
    facet_wrap( ~ fct_reorder(symbol, adjusted)) +
    theme_hc() + 
    theme(axis.text.x = element_text(angle = 90, size = 8)) +
    theme(legend.position = "none") +
    scale_color_manual(values = colors) +
    labs(title = "Adjusted Stock Price Per Tick")

#Volume
stocks %>% 
  ggplot(aes(x = date, y = volume, col = symbol)) +
    geom_line(alpha = .2) +
    geom_smooth() +
    facet_wrap( ~ fct_reorder(symbol, adjusted)) +
    theme_hc() + 
    theme(axis.text.x = element_text(angle = 90, size = 8)) +
    theme(legend.position = "none") +
    coord_cartesian(ylim = c(-10000000, 250000000)) +
    scale_color_manual(values = colors)
    