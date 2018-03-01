#Find Message
readr::read_lines("https://byuistats.github.io/M335/data/randomletters.txt") %>%
{stringr::str_extract_all(., ".", simplify = TRUE)[c(1, seq(from = 0, to = nchar(.), by = 1700))]} %>%
  paste(., collapse = "") %>%
  gsub("[^.]+$", "", .)

library(tidyverse)
findMessage <- function(url) {
  readr::read_lines(url) %>% 
    {stringr::str_extract_all(., ".", simplify = TRUE)[c(1, seq(from = 0, to = nchar(.), by = 1700))]} %>%
    paste(., collapse = "") %>%
    gsub("[^.]+$", "", .)
}

#Convert Numbers to Letters
readr::read_lines("https://byuistats.github.io/M335/data/randomletters_wnumbers.txt") %>%
  stringr::str_extract_all(., "[0-9]+") %>%
  unlist() %>%
  parse_number() %>%
  {paste(letters[.], collapse = "")}

convertToLetters <- function(url) {
  readr::read_lines(url) %>%
    stringr::str_extract_all(., "[0-9]+") %>%
    unlist() %>%
    parse_number() %>%
    {paste(letters[.], collapse = "")}
}

#Find longest list of vowels
readr::read_lines("https://byuistats.github.io/M335/data/randomletters.txt") %>%
  gsub("[\ .]", "", .) %>%
  stringr::str_extract_all("[aeiou]+") %>%
  {unlist(.)[which.max(nchar(unlist(.)))]}

longestList <- function(url) {
  readr::read_lines(url) %>%
    gsub("[\ .]", "", .) %>%
    stringr::str_extract_all("[aeiou]+") %>%
    {unlist(.)[which.max(nchar(unlist(.)))]}
}


findMessage <- function(url, replaceNums = FALSE) {
  if (replaceNums == FALSE) {
    readr::read_lines(url) %>% 
    {stringr::str_extract_all(., ".", simplify = TRUE)[c(1, seq(from = 0, to = nchar(.), by = 1700))]} %>%
      paste(., collapse = "") %>%
      gsub("[^.]+$", "", .)
  }
  else {
    readr::read_lines(url) %>%
      stringr::str_extract_all(., "[0-9]+") %>%
      unlist() %>%
      parse_number() %>%
      {paste(letters[.], collapse = "")}
  }

}

