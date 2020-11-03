#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from IPUMS USA
# Author: Jo-Yen Lin
# Data: November 2, 2020
# Contact: julianne.lin@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("/Users/juliannelin/Desktop/Fall 2020/STA304 A3")
raw_data <- read_dta("usa_00001.dta.gz")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(region,
         age,
         race, 
         #marst, 
         #bpl,
         #citizen,
         educd)
         #labforce,
         #labforce
# Clean the data 
# Age
levels(reduced_data$age)[1] <- 0 
levels(reduced_data$age)[91] <- 90
levels(reduced_data$age)[101] <- 100
levels(reduced_data$age)[113] <- 112
levels(reduced_data$age)[116] <- 115

# Reformat different races according to survey_data
reduced_data <- reduced_data %>% mutate(race = case_when(
  race %in% c("two major races","three or more major races","other race, nec" ) ~ "Some other race",
  race == "white" ~ "White",                          
  race == "black/african american/negro" ~ "Black, or African American", 
  race == "american indian or alaska native" ~ "American Indian or Alaska Native",
  race == "chinese" ~ "Chinese",                       
  race == "japanese" ~ "Japanese", 
  race == "other asian or pacific islander" ~ "Other Asian or Pacific Islander"
))

# Clean the region 

reduced_data <- reduced_data %>% mutate(census_region = case_when(
  region %in% c("new england division", "middle atlantic division", "mixed northeast divisions (1970 metro)") ~ "Northeast",
  region %in% c("east north central div", "west north central div","mixed midwest divisions (1970 metro)" ) ~ "Midwest", 
  region %in% c("south atlantic division", "east south central div","west south central div","mixed southern divisions (1970 metro)") ~ "South",
  region %in% c("mountain division", "pacific division", "mixed western divisions (1970 metro)") ~ "West",
  region %in% c("military/military reservations" ,"puma boundaries cross state lines-1% sample", "state not identified", "not identified") ~ "Other"
))

reduced_data <- reduced_data %>% mutate(education = case_when(
  educd %in% c("n/a or no schooling","n/a","no schooling completed", "nursery school to grade 4",
               "nursery school, preschool","kindergarten", "grade 1, 2, 3, or 4" , "grade 1",
               "grade 2","grade 3" ) ~ "3rd Grade or less", 
  educd %in% c("grade 4","grade 5, 6, 7, or 8", "grade 5 or 6" , "grade 5" ,"grade 6",
               "grade 7 or 8", "grade 7", "grade 8" ) ~ "Middle School - Grades 4 - 8", 
  educd %in% c("grade 9" , "grade 10", "grade 11","grade 12","12th grade, no diploma") ~ "Completed some high school",
  educd %in% c("regular high school diploma", "ged or alternative credential") ~ "High school graduate",
  educd %in% c("some college, but less than 1 year" ,"1 year of college",
               "1 or more years of college credit, no degree" ,"2 years of college", 
               "3 years of college","4 years of college", "5+ years of college", 
               "6 years of college (6+ in 1960-1970)","7 years of college",
               "8+ years of college" ) ~ "Completed some college, but no degree",
  educd %in% c("associate's degree, type not specified", "associate's degree, 
               occupational program","associate's degree, academic program") ~ "Associate Degree",
  educd == "bachelor's degree" ~ "College Degree (such as B.A., B.S.)", 
  educd %in% c("master's degree", "professional degree beyond a bachelor's degree") ~ "Masters degree",
  educd == "doctoral degree" ~ "Doctorate degree", 
  educd == "missing" ~ NA_character_
))

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)

new_reduced_data <- 
  reduced_data %>%
  count(race, census_region, education) %>%
  group_by(race) 

# Saving the census data as a csv file in my
# working directory
write_csv(new_reduced_data, "census_data.csv")



         