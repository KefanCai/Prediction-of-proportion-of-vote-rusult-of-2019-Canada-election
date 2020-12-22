#### Preamble ####
# Purpose:Prediction of proportion of vote rusult of 2019 Canada election 
# Author:Kefan Cai 1004819949
# Data: Dec.22nd, 2020
# Contact:kefan.cai@mail.utoronto.ca
# License:Samantha-Jo Caetano

library(janitor)
library(cesR)
library(tidyverse)
library(visdat)
library(skimr)
get_ces("ces2019_phone")
raw_data = ces2019_phone

#select interested data
reduced_data <- 
  raw_data %>% 
  select(q2,
         interviewer_gender_CES,
         language_CES,
         q11,
         q4)


#clean age data
reduced_data$age <- as.integer(reduced_data$q2)
reduced_data <- reduced_data %>%
  mutate(age = as.character(2020-q2))%>%
  mutate(age_group = case_when(age <= '24' ~ 'youth',
                               '59' < age  ~'senior',
                               age >= '25'~ 'adult'))


#clean language data
reduced_data <- reduced_data %>% 
  mutate(language_CES = as.character(language_CES)) %>%
  mutate(language_CES = case_when(language_CES == '1' ~ 'English',
                              language_CES == '2' ~ 'French',
                              language_CES == '3' ~ 'Other',
                              language_CES == '-7' ~ 'Other'))

#clean vote choice data
reduced_data <- reduced_data %>% 
  mutate(province = as.character(q4)) %>%
  mutate(province = case_when(province == '1' ~ 'Newfoundland and Labrador',
                              province == '2' ~ 'Prince Edward Island',
                              province == '3' ~ 'Nova Scotia',
                              province == '4' ~ 'New Brunswick',
                              province == '5' ~ 'Quebec',
                              province == '6' ~ 'Ontario',
                              province == '7' ~ 'Manitoba',
                          province == '8' ~ 'Saskatchewan',
                          province == '9' ~ 'Alberta',
                          province == '10' ~ 'British Columbia',
                          province == '11' ~ 'Northwest Territories',
                          province == '12' ~ 'Yukon',
                          province == '13' ~ 'Nunavut',
                          province == '-7' ~ 'Other',
                          province == '-8' ~ 'Other',
                          province == '-9' ~ 'Other'))



#clean vote choice data
reduced_data<-reduced_data %>%
  mutate(q11 = as.character(q11)) %>%
  mutate(q11 = case_when(q11 == '1'~1,
                                  q11 != '1'~0)) 

#clean gender data                
reduced_data <- 
  reduced_data %>%
  count(age_group,interviewer_gender_CES,language_CES,province,q11) %>%
  group_by(age_group,interviewer_gender_CES,language_CES,province,q11) 

names(reduced_data)[2] <- "gender" 
names(reduced_data)[3] <- "language"
names(reduced_data)[5] <- "vote_Liberal"

#save as csv
write_csv(reduced_data, "survey_data.csv")

