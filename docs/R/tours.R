# Builds tours filtered by MSA and age
# writes an rds saved in data folder

# filter people that are in regular sized cities
# filter people that are between 18 and 65
library(tidyverse)
library(nhts2017)
library(lubridate)
source("R/functions.R")

# This rds file comes from 01_combine.R
# Join persons, households, and trips together into one data set.
# select only the relevant columns (easy to add more variables later)
trips_edited <- read_rds("data/nhts_data.rds")
persons_edited <- read_rds("data/persons.rds")



# ===================================================================
# Write activities with tour attributes and person attributes.
nhts_activities <- trips_edited %>%
  filter(r_age > 18,
         r_age < 65, 
         msasize == "04") %>%
  
  build_activities() %>%
  add_tours() 

persons_edited %>%
  filter(r_age > 18,
         r_age < 65, 
         msasize == "04") %>%
  left_join(nhts_activities) %>%
  # for those who did not make a trip, show that they stayed home
  # and maybe made cookies
  mutate(tour_class = ifelse(is.na(activity), "H", tour_class)) %>%
  mutate(DAP = ifelse(is.na(activity), "H", DAP)) %>%
  # this takes a while to run, lets at least save it
  write_rds("data/activities_msa.rds") 


# ===================================================================
# write tours with person attributes
# list of persons

full_tours <- read_rds("data/activities_msa.rds") %>%
  build_tours() 

# joins tours onto persons (includes persons that make 0 tours aka "H")
persons_edited %>%
  filter(r_age > 18,
         r_age < 65, 
         msasize == "04") %>%
  select(houseid, personid, Ability, r_age, Age, Worker, hhfaminc, 
         Income, msasize, health, wtperfin) %>%
  left_join(full_tours, by = c("houseid", "personid")) %>%
  
  write_rds("data/tours.rds")
  

