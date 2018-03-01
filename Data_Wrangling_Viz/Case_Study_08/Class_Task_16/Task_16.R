library(tidyverse)
library(dygraphs)
library(tidyquant)
library(xts)
library(DT)

KR <- tq_get("KR", get = "stock.prices", from = "2012-01-01") %>%
  xts(order.by = .$date)

datatable(KR, colnames = c("Time" = 1), extensions = 'Scroller', options = list(
  deferRender = TRUE,
  scrollY = 200,
  scroller = TRUE
)) %>% 
  formatStyle('adjusted',  color = 'darkblue', backgroundColor = 'yellow', fontWeight = 'bold')

KR <- KR[,7]
dygraph(KR, main = "Kroger Adjusted Stock Prices") %>% 
  dySeries("adjusted", label = "Adjusted") %>% 
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y", label = "Adjusted") %>%
  dyOptions(axisLineColor = "navy", 
            gridLineColor = "lightblue",
            fillGraph = TRUE) %>% 
  dyAnnotation("2014-2-6", text = "A", tooltip = "Unemployment Benefit Cashed, Less than Anticipated\nLeads to increase") %>%
  dyAnnotation("2016-11-9", text = "B", tooltip = "Trump Wins Election") %>% 
  dyAnnotation("2017-6-14", text = "C", tooltip = "Kroger Pricing a Problem as Walmart Competes") %>% 
  dyRangeSelector(height = 20)
