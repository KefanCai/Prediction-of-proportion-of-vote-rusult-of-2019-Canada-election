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
read_csv("gss.csv")

original_data= read_csv("gss.csv")

#select interested data
reduced_data1 <- 
  original_data %>% 
  select(age,
         sex,
         language_home,
         province)

#clean age data
reduced_data1$age <- as.integer(reduced_data1$age)
reduced_data1 <- reduced_data1 %>%
  mutate(age = as.character(age))%>%
  mutate(age_group = case_when(age <= '24' ~ 'youth',
                               '59' < age  ~'senior',
                               age >= '25'~ 'adult'))

#clean language data
reduced_data1 <- reduced_data1 %>%
  mutate(language_home = as.character(language_home))%>%
  mutate(language_home = case_when(language_home == 'English' ~ 'English',
                                   language_home == 'Frence' ~ 'French'
                                   ))
    
#clean vote choice data
reduced_data1 <- reduced_data1 %>% 
  mutate(province = as.character(province)) %>%
  mutate(province = case_when(province == 'Newfoundland and Labrador' ~ 
                                'Newfoundland and Labrador',
                              province == 'Prince Edward Island' ~ 
                                'Prince Edward Island',
                              province == 'Nova Scotia' ~ 'Nova Scotia',
                              province == 'New Brunswick' ~ 'New Brunswick',
                              province == 'Quebec' ~ 'Quebec',
                              province == 'Ontario' ~ 'Ontario',
                              province == 'Manitoba' ~ 'Manitoba',
                              province == 'Saskatchewan' ~ 'Saskatchewan',
                              province == 'Alberta' ~ 'Alberta',
                              province == 'British Columbia' ~ 
                                'British Columbia',
                              province == 'Northwest Territories' ~ 
                                'Northwest Territories',
                              province == 'Yukon' ~ 'Yukon',
                              province == 'Nunavut' ~ 'Nunavut'
                              ))






#clean sex data
reduced_data1 <- 
  reduced_data1 %>%
  count(age_group,sex,language_home,province) %>%
  group_by(age_group,sex,language_home,province) 

names(reduced_data1)[2] <- "gender" 
names(reduced_data1)[3] <- "language"

#save as csv
write_csv(reduced_data1, "census_data.csv")                              
                                   



