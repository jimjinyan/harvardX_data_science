library(dslabs)
library(tidyverse)
library(broom)
data("UCBAdmissions")

admissions

admissions%>%group_by(gender)%>%summarize(percentage = 
                                            round(sum(admitted*applicants)/sum(applicants),1))

admissions%>%group_by(gender)%>%
  summarize(total_admitted = round(sum(admitted/100*applicants)),
            not_admitted = sum(applicants)-sum(total_admitted))%>%
  select(-gender) %>%
  do(tidy(chisq.test(.)))


admissions%>%select(major, gender, admitted)%>%
  spread(gender, admitted)%>%
  mutate(women_minus_men = women - men)

admissions%>%group_by(major)%>%
  summarize(major_selectivity = sum(admitted*applicants)/sum(applicants),
            percent_women_applicants = sum(applicants*(gender == "women"))/sum(applicants)*100)%>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major))+
  geom_text()

admissions%>%
  mutate(yes = round(admitted/100*applicants), no = applicants-yes)%>%
  select(-applicants, -admitted) %>%
  gather(admission, number_of_students, -c("major", "gender"))%>%
  ggplot(aes(gender, number_of_students, fill = admission))+
  geom_bar(stat = "identity", position = "stack")+
  facet_wrap(.~ major)


admissions %>%
  mutate(percent_admitted = admitted*applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major))+
  geom_bar(stat = "identity", position = "stack")

admissions%>%ggplot(aes(major, admitted, col = gender, size = applicants))+
  geom_point()

admissions%>%group_by(gender)%>%summarize(average = mean(admitted))