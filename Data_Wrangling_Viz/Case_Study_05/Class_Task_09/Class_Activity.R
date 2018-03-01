#install.packages('htmltab')
# https://cran.r-project.org/web/packages/htmltab/vignettes/htmltab.html
library(htmltab)
library(tidyverse)
library(readr)
library(purrr)
library(lubridate)
library(downloader)

# has functions kml_coordinate, kml_points, kml_polygons
source("https://gist.githubusercontent.com/briatte/18a4d543d1ccca194b2a03ac512be2b4/raw/5cd241ab780a33ec9a3ae6297a48f9035cda811d/get_points.r")

#kml file download http://ldschurchtemples.org/maps/
bob <- tempfile()
download("http://ldschurchtemples.org/maps/downloads/kml.php", bob)
temple.locs <- kml_points(bob) %>% select(name, longitude, latitude)

url_size <- "http://ldschurchtemples.org/statistics/dimensions/"
url_time <- "http://ldschurchtemples.org/statistics/timelines/"

dimensions <- htmltab(doc = url_size, 2) %>% as.tibble() 

times_AnGrbr <- htmltab(doc = url_time, 3) %>% select(-Duration) %>% as.tibble() 
times_GrbrDed <- htmltab(doc = url_time, 4) %>% select(-Duration) %>% as.tibble()



#Utilize longest df to left_join
bob <- temple.locs %>%
  rename(Temple = name) %>%
  left_join(times_GrbrDed) %>%
  left_join(times_AnGrbr) %>%
  left_join(dimensions)
  
#Check that there are no extra NA's
#Subtract largest df count() from smallest
#Check NA's in new df col to determine same # as dif between small and large df
dif <- count(temple.locs) - count(times_GrbrDed)
na <- sum(is.na(bob$`Ground Broken`))
#&
dif2 <- count(temple.locs) - count(dimensions)
na2 <- sum(is.na(bob$OrdinanceRooms))

#Look for inconsistencies
