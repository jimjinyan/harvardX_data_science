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


not_inches<- function(x, smallest, tallest){
  inches<- suppressWarnings(as.numeric(x))
  ind<- is.na(inches)|inches<smallest|inches>tallest
  ind
}

problems<- reported_heights%>%filter(not_inches(height, smallest, tallest))%>%.$height

problems


pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% head(n=10) %>% cat


pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat


ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) 
problems[ind] %>% head(n=10) %>% cat