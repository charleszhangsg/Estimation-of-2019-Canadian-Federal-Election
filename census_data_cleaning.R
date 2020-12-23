#### Preamble ####
# Purpose: Prepare and clean the census data downloaded from 2017 GSS
# Author: Min Zhang
# Data: 22 Dec 2020
# Contact: mincharles.zhang@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded gss.csv
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
library(stringr)
# Read in the raw data.

raw_data <- read_csv("gss.csv")  # census data

# Add the labels
raw_data <- labelled::to_factor(raw_data)

reduced_data <- 
  raw_data %>% 
  select(c(age,
           sex,
          province,
          education)) # variables we are interested

reduced_data$age <- as.integer(reduced_data$age)

reduced_data <-reduced_data %>% 
  filter(age >= 18)  #remove people under 18 who are ineligible

colnames(reduced_data)[2] <- "gender"


#update education:

reduced_data <- reduced_data %>% 
  filter(education != 'NA')

reduced_data$education[reduced_data$education== 'Trade certificate or diploma' ] <- 'College, CEGEP or other non-university certificate or diploma'

reduced_data$education[reduced_data$education== 'College, CEGEP or other non-university certificate or di...' ] <- 'College, CEGEP or other non-university certificate or diploma'

reduced_data$education[reduced_data$education== 'University certificate, diploma or degree above the bach...' ] <- 'University certificate, diploma or degree above the bachelor level'

reduced_data$education[reduced_data$education== "University certificate or diploma below the bachelor's level"] <- 'University certificate or diploma below the bachelor level'

reduced_data$education[reduced_data$education== "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)"] <- 'Bachelor degree (e.g. B.A., B.Sc., LL.B.)'

reduced_data$education[reduced_data$education== 'High school diploma or a high school equivalency certificate'] <- 'High school diploma or a high school equivalency certificate'


reduced_data <- reduced_data %>% 
  count(age,gender,province,education) %>% 
  group_by(age)


write_csv(reduced_data, "census_data.csv")

