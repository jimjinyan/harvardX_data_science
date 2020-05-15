library(tidyverse)
library(dslabs)
data("research_funding_rates")


totals <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(sum) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) 

funding_rate<- totals%>% summarize(percent_total = (yes_men+yes_women)/(yes_men+no_men+yes_women+no_women))%>%.$
  percent_total
two_by_two<- tibble(awarded = c("no", "yes"), 
                    men=c(totals$no_men, totals$yes_men), 
                    women = c(totals$no_women, totals$yes_women))

tibble(awarded = c("no", "yes"),
       men = (totals$no_men+totals$yes_men)*c(1-funding_rate, funding_rate),
       women = (totals$no_women+totals$yes_women)*c(1-funding_rate, funding_rate))

chisq_test<- two_by_two%>%select(-awarded)%>%chisq.test()
chisq_test
