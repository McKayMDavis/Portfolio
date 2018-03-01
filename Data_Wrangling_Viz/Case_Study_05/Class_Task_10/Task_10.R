#Find the hidden message
readr::read_lines("https://byuistats.github.io/M335/data/randomletters.txt") %>%
  {gsub("[^.]+$", "", paste(stringr::str_extract_all(., ".", simplify = TRUE)[c(1, seq(from = 0, to = nchar(.), by = 1700))], collapse = ""))}

#Find the hidden message (simplified)
readr::read_lines("https://byuistats.github.io/M335/data/randomletters.txt") %>%
  {stringr::str_extract_all(., ".", simplify = TRUE)[c(1, seq(from = 0, to = nchar(.), by = 1700))]} %>%
  paste(., collapse = "") %>%
  gsub("[^.]+$", "", .)

#Convert numbers to letters
readr::read_lines("https://byuistats.github.io/M335/data/randomletters_wnumbers.txt") %>%
  {paste(letters[as.numeric(unlist(stringr::str_extract_all(., "[0-9]+")))], collapse = "")}

#Convert numbers to letters (simplified)
readr::read_lines("https://byuistats.github.io/M335/data/randomletters_wnumbers.txt") %>%
  stringr::str_extract_all(., "[0-9]+") %>%
  unlist() %>%
  parse_number() %>%
  {paste(letters[.], collapse = "")}


#Find longest list of vowels
readr::read_lines("https://byuistats.github.io/M335/data/randomletters.txt") %>%
{gsub("[\ .]", "", unlist(stringr::str_extract_all(., "[aeiou]+")))[which.max(nchar(gsub("[\ .]", "", unlist(stringr::str_extract_all(., "[aeiou]+")))))]}

#Find longest list (simplified)
readr::read_lines("https://byuistats.github.io/M335/data/randomletters.txt") %>%
  gsub("[\ .]", "", .) %>%
  stringr::str_extract_all("[aeiou]+") %>%
  {unlist(.)[which.max(nchar(unlist(.)))]}



#Dylan's code (given to me after I completed the task) to compare
read_lines("https://byuistats.github.io/M335/data/randomletters.txt") %>% 
{gsub("[^.]+$", "", paste(unlist(strsplit(.,""))[c(1, seq(0, nchar(.), 1700))], collapse=""))}

read_lines("https://byuistats.github.io/M335/data/randomletters_wnumbers.txt") %>%
{paste(letters[na.omit(as.numeric(unlist(strsplit(., "[^0-9]+"))))], collapse = "")}

read_lines("https://byuistats.github.io/M335/data/randomletters.txt") %>%
  gsub("[\\. ]+", "", .) %>% 
  str_extract_all("[aeiou]+") %>% 
  {unlist(.)[which.max(nchar(unlist(.)))]}
