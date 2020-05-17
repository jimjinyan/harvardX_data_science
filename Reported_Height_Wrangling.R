library(tidyverse)
library(rvest)
library(dslabs)
data(reported_heights)

x <- as.numeric(reported_heights$height)

reported_heights%>%mutate(new_height = as.numeric(height))%>%
  filter(is.na(new_height))


reported_heights%>%mutate(height = as.numeric(height))%>%filter(height<200)%>%
  ggplot(aes(height))+
  geom_histogram()

inch_input<- reported_heights%>%mutate(height = as.numeric(height))%>%filter(height<90&height>45)%>%.$height

inch_input

alpha <- 1/10^6
tallest<- qnorm(1-alpha/2, avg, se)
smallest<- qnorm(alpha/2, avg, se)


not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}

problems<- reported_heights%>%filter(not_inches(height, smallest, tallest))%>%.$height

problems


#pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
#str_subset(problems, pattern) %>% head(n=10) %>% cat


#pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
#str_subset(problems, pattern) %>% head(n=10) %>% cat


#ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) 
#problems[ind] %>% head(n=10) %>% cat


convert_format<- function(s){
  s%>%
    str_replace("feet|foot|ft", "'")%>%
    str_replace_all("inches|in|''|\"|cm|and", "")%>%
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")%>%
    str_replace("^([56])'?$", "\\1'0")%>%
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")%>%
    str_trim()
}

words_to_numbers <- function(s){
  str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}

converted<- problems%>%words_to_numbers%>%convert_format
remining_problems<- converted[not_inches_or_cm(converted)]
pattern<- "^[4-7]\\s*\\d+\\.?\\d*$"
index<- str_detect(remining_problems, pattern)
remining_problems[!index]