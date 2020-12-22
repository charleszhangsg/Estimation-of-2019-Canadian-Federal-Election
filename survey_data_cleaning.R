 #### Preamble ####
# Purpose: Prepare and clean the survey data from 2019ces
# Author: Min Zhang
# Data: 22 Dec 2020
# Contact: mincharles.zhang@mail.utoronto.ca
# License: MIT
# Pre-requisites: run STA304_final_project.rmd before this file as it contain survey data. 
# -

 # survey data: 
library(haven)
library(tidyverse)

library(cesR)
library(labelled)
library(dplyr)
devtools::install_github("hodgettsp/cesR")
get_ces("ces2019_phone")

reduced_data <- ces2019_phone %>% 
  select(c(q2,q3,q4,q11,q61))


# variable names updates:
colnames(reduced_data)[1] <- "age"
colnames(reduced_data)[2] <- "gender"
colnames(reduced_data)[3] <- "province/territory"
colnames(reduced_data)[4] <- "vote for party"
colnames(reduced_data)[5] <- "education"

# update age:
as.numeric(reduced_data$age)


for (i in 1:length(reduced_data$age)){
  reduced_data$age[i]=2019-reduced_data$age[i]
}


# update gender:
reduced_data <- reduced_data %>% 
  filter(gender != 3)

reduced_data$gender <- as.character(reduced_data$gender)
reduced_data$gender[reduced_data$gender=='1'] <- 'Male'

reduced_data$gender <- as.character(reduced_data$gender)
reduced_data$gender[reduced_data$gender=='2'] <- 'Female'

# update province/territories:

reduced_data$`province/territory` <- as.character(reduced_data$`province/territory`)
reduced_data$`province/territory`[reduced_data$`province/territory`=='1'] <- 'Newfoundland and Labrador'

reduced_data$`province/territory` <- as.character(reduced_data$`province/territory`)
reduced_data$`province/territory`[reduced_data$`province/territory`=='2'] <- 'Prince Edward Island'

reduced_data$`province/territory` <- as.character(reduced_data$`province/territory`)
reduced_data$`province/territory`[reduced_data$`province/territory`=='3'] <- 'Nova Scotia'

reduced_data$`province/territory` <- as.character(reduced_data$`province/territory`)
reduced_data$`province/territory`[reduced_data$`province/territory`=='4'] <- 'New Brunswick'

reduced_data$`province/territory` <- as.character(reduced_data$`province/territory`)
reduced_data$`province/territory`[reduced_data$`province/territory`=='5'] <- 'Quebec'

reduced_data$`province/territory` <- as.character(reduced_data$`province/territory`)
reduced_data$`province/territory`[reduced_data$`province/territory`=='6'] <- 'Ontario'

reduced_data$`province/territory` <- as.character(reduced_data$`province/territory`)
reduced_data$`province/territory`[reduced_data$`province/territory`=='7'] <- 'Manitoba'

reduced_data$`province/territory` <- as.character(reduced_data$`province/territory`)
reduced_data$`province/territory`[reduced_data$`province/territory`=='8'] <- 'Saskatchewan'

reduced_data$`province/territory` <- as.character(reduced_data$`province/territory`)
reduced_data$`province/territory`[reduced_data$`province/territory`=='9'] <- 'Alberta'

reduced_data$`province/territory` <- as.character(reduced_data$`province/territory`)
reduced_data$`province/territory`[reduced_data$`province/territory`=='10'] <- 'British Columbia'

reduced_data$`province/territory` <- as.character(reduced_data$`province/territory`)
reduced_data$`province/territory`[reduced_data$`province/territory`=='11'] <- 'Northwest Territories'

reduced_data$`province/territory` <- as.character(reduced_data$`province/territory`)
reduced_data$`province/territory`[reduced_data$`province/territory`=='12'] <- 'Yukon'

reduced_data$`province/territory` <- as.character(reduced_data$`province/territory`)
reduced_data$`province/territory`[reduced_data$`province/territory`=='13'] <- 'Nunavut'


# eliminate "unqualified" voters

reduced_data <- reduced_data %>%
  filter(`vote for party` != 7) %>% 
  filter(`vote for party` != 9) %>%
  filter(`vote for party` != 8) %>% 
  filter(`vote for party` != 10) %>%
  filter(`vote for party` != -8) %>%
  filter(`vote for party` != -9)  

# change party name
reduced_data$`vote for party` <- as.character(reduced_data$`vote for party`)
reduced_data$`vote for party`[reduced_data$`vote for party`=='1'] <- 'Liberal'

reduced_data$`vote for party` <- as.character(reduced_data$`vote for party`)
reduced_data$`vote for party`[reduced_data$`vote for party`=='2'] <- 'Conservatives'

reduced_data$`vote for party` <- as.character(reduced_data$`vote for party`)
reduced_data$`vote for party`[reduced_data$`vote for party`=='3'] <- 'NDP'

reduced_data$`vote for party` <- as.character(reduced_data$`vote for party`)
reduced_data$`vote for party`[reduced_data$`vote for party`=='4'] <- 'Bloc Québécois'

reduced_data$`vote for party` <- as.character(reduced_data$`vote for party`)
reduced_data$`vote for party`[reduced_data$`vote for party`=='5'] <- 'Green Party'

reduced_data$`vote for party` <- as.character(reduced_data$`vote for party`)
reduced_data$`vote for party`[reduced_data$`vote for party`=='6'] <- 'People’s Party'
  
# update education

reduced_data <- reduced_data %>% 
  filter(education != -8) %>% 
  filter(education != -9)

reduced_data$education <- as.character(reduced_data$education)
reduced_data$education[reduced_data$education== 1] <- 'Less than high school diploma or its equivalent'

reduced_data$education[reduced_data$education== 2] <- 'Less than high school diploma or its equivalent'

reduced_data$education[reduced_data$education== 3] <- 'Less than high school diploma or its equivalent'

reduced_data$education[reduced_data$education== 4] <- 'Less than high school diploma or its equivalent'

reduced_data$education[reduced_data$education== 5] <- 'High school diploma or a high school equivalency certificate'

reduced_data$education[reduced_data$education== 6] <- 'College, CEGEP or other non-university certificate or diploma'

reduced_data$education[reduced_data$education== 7] <- 'University certificate or diploma below the bachelor level'

reduced_data$education[reduced_data$education== 8] <- 'University certificate or diploma below the bachelor level'

reduced_data$education[reduced_data$education== 9] <- 'Bachelor degree (e.g. B.A., B.Sc., LL.B.)'

reduced_data$education[reduced_data$education== 10] <- 'University certificate, diploma or degree above the bachelor level'

reduced_data$education[reduced_data$education== 11] <- 'University certificate, diploma or degree above the bachelor level'

# Dummy variable for each party: 

reduced_data <- reduced_data %>% 
  mutate(Liberal_popular_vote=case_when(
     `vote for party`=="Liberal" ~ 1,
    TRUE ~ 0))

reduced_data <- reduced_data %>% 
  mutate(Conservative_popular_vote=case_when(
    `vote for party`=="Conservatives" ~ 1,
    TRUE ~ 0))

reduced_data <- reduced_data %>% 
  mutate(NDP_popular_vote=case_when(
    `vote for party`=="NDP" ~ 1,
    TRUE ~ 0))

reduced_data <- reduced_data %>% 
  mutate(Bloc_Québécois_popular_vote=case_when(
    `vote for party`=="Bloc Québécois" ~ 1,
    TRUE ~ 0))

reduced_data <- reduced_data %>% 
  mutate(Green_popular_vote=case_when(
    `vote for party`=="Green Party" ~ 1,
    TRUE ~ 0))

reduced_data <- reduced_data %>% 
  mutate(People_popular_vote=case_when(
    `vote for party`=="	People’s Party" ~ 1,
    TRUE ~ 0))

view(reduced_data)

