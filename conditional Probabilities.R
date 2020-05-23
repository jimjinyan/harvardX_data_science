library(tidyverse)
library(caret)
library(dslabs)

p_t<- 0.85*0.02+0.1*0.98
p_t_d<- 0.85
p_d<- 0.02
p_t_d*p_d/p_t


set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

#What is the probability that a test is positive?

mean(test)

#What is the probability that an individual has the disease if the test is negative?

mean(disease[test==0])

#What is the probability that you have the disease if the test is positive?

mean(disease[test==1]==1)

#If a patient's test is positive, how much does that increase their risk of having the disease?
# First calculate the probability of having the disease given a positive test, then divide by the probability of having the disease.


mean(disease[test==1]==1)/mean(disease==1)