#Convert list of names to grep commands. Or iterate through long names and replace with unique name (assign column of unique numbers for each name and replace each name with number reference. then in original data, just merge to assign names to numbers.)
library(rio)
library(tidyverse)
library(stringr)
library(stringi)

scrip <- import("http://scriptures.nephi.org/downloads/lds-scriptures.csv.zip")
savnames <- import("https://byuistats.github.io/M335/data/BoM_SaviorNames.rds")

#filter to bom
scripfilter <- scrip %>% 
  filter(volume_title == "Book of Mormon")

#mutate unique numbers column
savfilter <- savnames %>%
  mutate(unique_id = c(1:111))


#Need to replace all special characters in the scripture_text with " " and then wrap 
#each savior name in " " This gets rid of issue where "Many" is replaced with "111y"
scripfilter$scripture_text <- gsub("[^aA-zZ]", "~", scripfilter$scripture_text)
savfilter$name <- gsub(" ", "~", savfilter$name)
savfilter$name <- paste("~", savfilter$name, "~")

#replace names in scriptures with unique numbers !ISSUE! Replaces "Man" in "Manasseh" and alike things
names <- unlist(savfilter$name)
names <- gsub(" ", "", names)
countvar <- 1
for (i in names) {
  unique_id <- paste0("!", countvar, "!")
  scripfilter$scripture_text <- gsub(i, unique_id, scripfilter$scripture_text)
  
  ben <- str_locate_all(scripfilter$scripture_text, unique_id)
  bob$unique_id[[i]] <- ben[1] #count?
  
  
  
  
  
  
  
  
  
  
  
  
  
  countvar <- countvar + 1
}

#replace each "~" with " " and each "~~" with " " (special characters no longer exist)
scripfilter$scripture_text <- gsub("~~", " ", scripfilter$scripture_text)
scripfilter$scripture_text <- gsub("~", " ", scripfilter$scripture_text)
scripfilter$scripture_text <- gsub("(\\s$)", "", scripfilter$scripture_text)

#Here we really go
for (i in seq_along(names)) {
  ben <- str_locate_all(scripfilter$scripture_text, "![0-9]+!")
  bob[,i] <- ben
}

