library(tidyverse)
library(stringi)
library(readr)
library(stringr)

savname <- read_csv("./Case_Study_06/Class_Task_12/lds-scriptures.csv")

getAvNumWords <- function(x, y){
  jo <- filter(savname, volume_title == x)
  bob <- stri_stats_latex(jo$scripture_text)[1]
  john <- bob/length(jo$verse_id)
  jo2 <- filter(savname, volume_title == y)
  bob2 <- stri_stats_latex(jo$scripture_text)[1]
  john2 <- bob2/length(jo2$verse_id)  
  
  jose <- data.frame(vector1 = john, vector2 = john2)
  return(jose)
}
getAvNumWords("Book of Mormon", "New Testament")

getJesus <- function(x, y){
  jo <- filter(savname, volume_title == x)
  bob <- !is.na(str_locate_all(unlist(jo$scripture_text), "Jesus"))
  bill <- sum(bob)
  jo2 <- filter(savname, volume_title == y)
  bob2 <- !is.na(str_locate_all(unlist(jo2$scripture_text), "Jesus")) 
  bill2 <- sum(bob2)
  
  jose <- data.frame(row.names = "NumJesus", vector1 = bill, vector2 = bill2)
  return(jose)
}
getJesus("Book of Mormon", "New Testament")

#How do the verses from each book in the Book of Mormon compare in average word length?
#This question only wants average length so I could technically just table the averages.
#A better response would be to plot all verse lengths for each book. This would give an 
#explanation of spread.
#For time purposes I am tabling the averages.

