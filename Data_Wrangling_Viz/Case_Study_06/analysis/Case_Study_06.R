#Soooooooo.... I worked on this for hours and hours trying to get what brother hathaway had. I failed.
#I decided that I've done sufficient work for this project. I'm going to modify brother hathaway's graphic.
#My code is below brother hathaways.
#____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
#____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________

#' # Case Study 6 
#' Brother Hathaway's code to tackle the distance between Savior names. My commenting structure allows me to build a notebook from this script ([see articles from r script](http://rmarkdown.rstudio.com/articles_report_from_r_script.html))

#' ## Libraries to load
library(downloader)
library(readr)
library(dplyr)
library(stringr)
library(stringi)
library(forcats)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(zoo)
library(rio)
library(tibble)
library(ggforce)

#' ## Notes
#' 
#' Find the average number of words between savior names in the book of mormon
#' Provide a few visualizations of savior name occurances accross books in the book of mormon.

#' Could do the following sequence
#' 1. Find longest savior name
#' 2. str_locate_all and add count column for word
#' 3. str_replace_all with "xfound[#}" where number is the savior name number from the list.
#' 4. repeat 1-3 with next savior name

#' ## Code
#' 

scriptures <- import("http://scriptures.nephi.org/downloads/lds-scriptures.csv.zip") %>% as.tibble()
bmnames <- import("https://byuistats.github.io/M335/data/BoM_SaviorNames.rds")

#' Create data for each book
bm <- scriptures %>% filter(volume_short_title == "BoM")
nt <- scriptures %>% filter(volume_short_title == "NT")

#' need to loop through largest to smallest savior names
bmnames <- bmnames %>% arrange(desc(nchar))

#' I added a column to have a key for each name
bmnames$word_label <- 1:nrow(bmnames)
#' I am going to alter text in this column.  I want to keep the original text in a seperate column
bm$scripture_sub <- bm$scripture_text


#'  ### The Loop
#' 
#' For each name 
#' 1. find out how many times it occurs in each verse
#' 2. Add the counts of per verse as a column
#' 3. Replace the name with a key to the specific name

#' So I end up with a column for each savior name key that has a count for each verse.  Then I create two new columns after the for loop finishes.  I include a print(i) statement to get a picture of how fast it is going.

#+ include=FALSE
for (i in seq_along(bmnames$name)){
  
  sname <- bmnames$name[i] 
  replace_name <- paste0("xfound_",i)
  
  bm_locs <- bm$scripture_sub %>% 
    str_locate_all(sname) %>% 
    lapply(function(x) nrow(x)) %>%
    unlist()
  
  bm[,paste0("name_", i)] <- bm_locs
  
  bm$scripture_sub <- bm$scripture_sub %>% str_replace_all(sname, replace_name)
  print(i)
} 

bm$count_name <- apply(bm[,colnames(bm) %in% paste0("name_", 1:111)],1, sum)
bm$cum_name <- cumsum(bm$count_name)

#' ### The name verses
#' 
#' This next chunk of code creates a subset of the original BOM where only verses with savior names are kept. I used duplicated but count_name == 0 could have worked as well. Note the final mutate function that maps cum_name to order.  I am going to use order to merge to the split data.

bm_merge <- filter(bm, !duplicated(cum_name)) %>% 
  select(cum_name, book_id, chapter_id, verse_id, book_title, chapter_number, verse_number, verse_title, verse_short_title ) %>% 
  mutate(order = cum_name)


#' ### The splitting
#' 
#' The process to get the distances between savior name references.
#' 
#'  A. Concatinate the sentences into one long phrase and then str_split by "xfound[0-9]+"
#'  B. str_extract by same split to put the name at the end of the split phrase
#'  C. Apply word count to each split element

#' The full text of the book of mormon with with the substituted names
all_text <- paste(bm$scripture_sub, collapse = " ")

#' The split text chunks
name_broke <- str_split(all_text, "xfound_[0-9]{1,3}")[[1]]
#' The keys for each split
names_label <- str_extract_all(all_text, "xfound_[0-9]{1,3}")[[1]]
names_label <- c(as.numeric(str_replace(names_label, "xfound_", "")), 9999)


# I have one more word after application than I did before.  Need to think about this.
stri_stats_latex(all_text)
length(names_label) + stri_stats_latex(name_broke)[["Words"]]


#' The Recombinging into Tidy Data
#' 
#' Now create a data frame that has the order the name happened, the name key, and the text.
bm_split <- data.frame(order = 1:length(names_label), word_label = names_label, scripture_text = name_broke, stringsAsFactors = FALSE)

#' This next line combines the names table into our bm_split dataset.
bm_split <- left_join(bm_split, bmnames) %>% 
  select(order, word_label, name, nchar, words, reference, Book, chapter_verse, scripture_text)

#' The next object goes line by line and counts the number of words in each split.
counts_split <- bm_split %>% 
  group_by(order) %>% 
  summarise(words_between = stri_stats_latex(scripture_text)["Words"])

#' My summarise dropped information that I want in bm_split, so I am merging bm_split info back in.
bm_split <- left_join(counts_split, bm_split)

#' Here is the big line that creates the final data set. It is doing a few things.  I think I have the logic correct
#' 1. I rename a few columns about the first view.  Maybe I don't even need to keep these.
#' 2. Now the bm_merge left join will connect the verse reference to the verse were the split word occured.
#' 3. We will have some issues with verses with two references in the same verse so the final mutate fills in the blanks.
#'     a. Sinse I have a cum count for each verse then if a line is missing information from the bm_merge data it should be replaced with the line that follows.  This only works due to how I formatted bm_merge and that we have order.
#'     
bm_split <- bm_split %>% 
  rename(reference_first = reference, book_first = Book, chapter_verse_first = chapter_verse) %>% 
  left_join(bm_merge) %>% 
  mutate(verse_short_title = na.locf(verse_short_title, fromLast = TRUE, na.rm = FALSE),
         verse_title = na.locf(verse_title, fromLast = TRUE, na.rm = FALSE),
         verse_number = na.locf(verse_number, fromLast = TRUE, na.rm = FALSE),
         chapter_number = na.locf(chapter_number, fromLast = TRUE, na.rm = FALSE),
         book_title = na.locf(book_title, fromLast = TRUE, na.rm = FALSE)
  )

#' The average.
mean(bm_split$words_between)

#' ### The Visualizations
#' 
#' This data set will allow me to draw the lines marking each book.
bm_counts_book <- bm %>% 
  group_by(book_id, book_title) %>% 
  summarise(word_count = stri_stats_latex(scripture_text)["Words"], cum_name = sum(count_name)) %>% ungroup() %>%
  mutate(cum_word_count = cumsum(word_count),
         cum_name_count = cumsum(cum_name))

#' We have a classic case of heavy right skew.  We will need to figure out how to show the big differences and the the predominate small distance patterns.  I have elected to show to graphs in my visualization.

base_plot <- bm_split %>% 
  ggplot(aes(x = order, y = words_between)) + 
  geom_point() + 
  geom_smooth() + 
  geom_vline(data = bm_counts_book, aes(xintercept = cum_name_count), color = "skyblue", size = 1.1, lty = 2) +
  theme_bw()

full_plot <- base_plot + 
  labs(x = "", y = "Number of Words Between use of Savior Name") + 
  geom_label(data = filter(bm_counts_book, book_title %in% c("1 Nephi", "2 Nephi", "Mosiah", "Alma", "Helaman", "3 Nephi", "Ether", "Moroni")), 
             aes(x = cum_name_count, label = book_title), y = 3000) + 
  geom_hline(yintercept = 150, color = "darkgrey", size = 1.3) +
  geom_text_repel(data = filter(bm_split, words_between > 1200), aes(label = verse_short_title), size = 3)

zoom_plot <- base_plot + 
  coord_cartesian(ylim = c(0,150)) + 
  labs(x = "Chronilogical Occurence of Savior Name", y = "Words Between") +
  geom_hline(yintercept = 40.5*1.7, color = "darkblue")

#+ fig.width=14, fig.height=7
#grid.arrange(full_plot, zoom_plot, ncol = 1, heights = c(3,2))


#jpeg(file = "wordcount_saviornames.jpg", width = 14, height = 7, units = "in", res = 150)
#grid.arrange(full_plot, zoom_plot, ncol = 1, heights = c(3,2))
#dev.off()

#' * Need to do it for the new testament
#' * Make int interactive and have each dot open a link to the verses on LDS.org
#' * Check that really short names are not being found in longer words that aren't names.
#' * I know that . Father Lehi will count in mine.




#My Graphic:

zoom <- full_plot +
  facet_zoom(y = words_between <= 200)

ggsave("zoom.png", plot = zoom, path = "./Case_Study_06/analysis", width = 20, height = 8)



























#DONT RUN THIS


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
  mutate(word_label = c(1:111))


#Need to replace all special characters in the scripture_text with " " and then wrap 
#each savior name in " " This gets rid of issue where "Many" is replaced with "111y"
scripfilter$scripture_text2 <- scripfilter$scripture_text
scripfilter$scripture_text2 <- gsub("[^aA-zZ]", "~", scripfilter$scripture_text2)
scripfilter$scripture_text2 <- gsub("~~", "~", scripfilter$scripture_text2)
savfilter$name <- gsub(" ", "~", savfilter$name)
savfilter$name <- paste("~", savfilter$name, "~")
savfilter$name <- gsub(",", "", savfilter$name)


#replace names in scriptures with unique numbers !ISSUE! Replaces "Man" in "Manasseh" and alike things
names <- unlist(savfilter$name)
names <- gsub(" ", "", names)
countvar <- 1
for (i in names) {
  scripfilter$scripture_text2 <- gsub(i, paste0("!", countvar, "!"), scripfilter$scripture_text2)
  countvar <- countvar + 1
  
  name <- savfilter$name[i]
  
  namelocs <- scripfilter$scripture_text2 %>% 
    str_locate_all(name) %>% 
    lapply(function(x) nrow(x)) %>% 
    unlist()
  
  scripfilter[,paste0("name_", i)] <- namelocs
}

scripfilter$count_name <- apply(scripfilter[,colnames(scripfilter) %in% paste0("name_", 1:111)],1, sum)
scripfilter$cumulative_name <- cumsum(scripfilter$count_name)

#replace each "~" with " " and each "~~" with " " (special characters no longer exist)
scripfilter$scripture_text2 <- gsub("~~", " ", scripfilter$scripture_text2)
scripfilter$scripture_text2 <- gsub("~", " ", scripfilter$scripture_text2)
scripfilter$scripture_text2 <- gsub("(\\s$)", "", scripfilter$scripture_text2)

#Continue referencing Brother Hathaways to finish.
#__________________________________________________________________________________________________________

scrip_merge <- filter(scripfilter, !duplicated(cumulative_name)) %>% 
  select(cumulative_name, book_id, chapter_id, verse_id, book_title, chapter_number, verse_number, verse_title, verse_short_title ) %>% 
  mutate(order = cumulative_name)

#' The full text of the book of mormon with with the substituted names
all_text <- paste(scripfilter$scripture_text2, collapse = " ")

#' The split text chunks
name_broke <- str_split(all_text, "![0-9]+!")[[1]]
#' The keys for each split
names_label <- str_extract_all(all_text, "![0-9]+!")[[1]]
names_label <- c(as.numeric(gsub("!", "", names_label)), 9999)

# Words equal on both sides
stri_stats_latex(all_text)
stri_stats_latex(name_broke)[["Words"]] # don't add length of names_label because no spaces between savnames and reg words in all_text


#' The Recombining into Tidy Data
#' 
#' Now create a data frame that has the order the name happened, the name key, and the text.
scrip_split <- data.frame(order = 1:length(names_label), word_label = names_label, scripture_text = name_broke, stringsAsFactors = FALSE)

#' This next line combines the names table into our scrip_split dataset.
scrip_split <- left_join(scrip_split, savfilter) %>% 
  select(order, word_label, name, nchar, words, reference, Book, chapter_verse, scripture_text)
scrip_split$name <- gsub("~", " ", scrip_split$name)

#' The next object goes line by line and counts the number of words in each split.
counts_split <- scrip_split %>% 
  group_by(order) %>% 
  summarise(words_between = stri_stats_latex(scripture_text)["Words"])

#' My summarise dropped information that I want in bm_split, so I am merging bm_split info back in.
scrip_split <- left_join(counts_split, scrip_split)

#' Here is the big line that creates the final data set. It is doing a few things.  I think I have the logic correct
#' 1. I rename a few columns about the first view.  Maybe I don't even need to keep these.
#' 2. Now the bm_merge left join will connect the verse reference to the verse were the split word occured.
#' 3. We will have some issues with verses with two references in the same verse so the final mutate fills in the blanks.
#'     a. Sinse I have a cum count for each verse then if a line is missing information from the bm_merge data it should be replaced with the line that follows.  This only works due to how I formatted bm_merge and that we have order.
#'     
scrip_split <- scrip_split %>% 
  rename(reference_first = reference, book_first = Book, chapter_verse_first = chapter_verse) %>% 
  left_join(scrip_merge) %>% 
  mutate(verse_short_title = na.locf(verse_short_title, fromLast = TRUE, na.rm = FALSE),
         verse_title = na.locf(verse_title, fromLast = TRUE, na.rm = FALSE),
         verse_number = na.locf(verse_number, fromLast = TRUE, na.rm = FALSE),
         chapter_number = na.locf(chapter_number, fromLast = TRUE, na.rm = FALSE),
         book_title = na.locf(book_title, fromLast = TRUE, na.rm = FALSE)
  )

#' The average.
mean(bm_split$words_between)

#' ### The Visualizations
#' 
#' This data set will allow me to draw the lines marking each book.
bm_counts_book <- bm %>% 
  group_by(book_id, book_title) %>% 
  summarise(word_count = stri_stats_latex(scripture_text)["Words"], cum_name = sum(count_name)) %>% ungroup() %>%
  mutate(cum_word_count = cumsum(word_count),
         cum_name_count = cumsum(cum_name))

#' We have a classic case of heavy right skew.  We will need to figure out how to show the big differences and the the predominate small distance patterns.  I have elected to show to graphs in my visualization.

base_plot <- bm_split %>% 
  ggplot(aes(x = order, y = words_between)) + 
  geom_point() + 
  geom_smooth() + 
  geom_vline(data = bm_counts_book, aes(xintercept = cum_name_count), color = "skyblue", size = 1.1, lty = 2) +
  theme_bw()

full_plot <- base_plot + 
  labs(x = "", y = "Number of Words Between use of Savior Name") + 
  geom_label(data = filter(bm_counts_book, book_title %in% c("1 Nephi", "2 Nephi", "Mosiah", "Alma", "Helaman", "3 Nephi", "Ether", "Moroni")), 
             aes(x = cum_name_count, label = book_title), y = 3000) + 
  geom_hline(yintercept = 150, color = "darkgrey", size = 1.3) +
  geom_text_repel(data = filter(bm_split, words_between > 1200), aes(label = verse_short_title), size = 3)

zoom_plot <- base_plot + 
  coord_cartesian(ylim = c(0,150)) + 
  labs(x = "Chronilogical Occurence of Savior Name", y = "Words Between") +
  geom_hline(yintercept = 40.5*1.7, color = "darkblue")

#+ fig.width=14, fig.height=7
grid.arrange(full_plot, zoom_plot, ncol = 1, heights = c(3,2))


jpeg(file = "wordcount_saviornames.jpg", width = 14, height = 7, units = "in", res = 150)
grid.arrange(full_plot, zoom_plot, ncol = 1, heights = c(3,2))
dev.off()

#' * Need to do it for the new testament
#' * Make int interactive and have each dot open a link to the verses on LDS.org
#' * Check that really short names are not being found in longer words that aren't names.
#' * I know that . Father Lehi will count in mine.













#Get AVERAGE
john <- scripfilter %>% 
  {paste(unlist(.$scripture_text2), collapse = " ")} %>% 
  str_split(., "![0-9]+!") %>% 
  {data.frame(strings = unlist(.))}

for (i in seq_along(john$strings)) {
  john$countstr[i] <- stri_stats_latex(john$strings[i])[4]
}
average <- sum(john$countstr)/length(john$countstr)








#Here we really go
bob = list()
for (i in seq_along(names)) {
  ben <- str_locate_all(scripfilter$scripture_text, "![0-9]+!")
  bob[[i]] <- ben
}

dat <- bind_rows(bob)













for (i in seq_along(bmnames$name)){
  
  sname <- bmnames$name[i] 
  replace_name <- paste0("xfound_",i)
  
  bm_locs <- bm$scripture_sub %>% 
    str_locate_all(sname) %>% 
    lapply(function(x) nrow(x)) %>%
    unlist()
  
  bm[,paste0("name_", i)] <- bm_locs
  
  bm$scripture_sub <- bm$scripture_sub %>% str_replace_all(sname, replace_name)
  print(i)
} 

bm$count_name <- apply(bm[,colnames(bm) %in% paste0("name_", 1:111)],1, sum)
bm$cum_name <- cumsum(bm$count_name)

