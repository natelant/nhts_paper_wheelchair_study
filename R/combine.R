## Combine

## This script attempts to combine all of the data on one script.
# the goal is to do the following
# 1. merge persons, households, and trips, only including the necessary variables
# 2. create new variables (Ability, Worker, Age, etc.)
# 3. create the grid to count number of trips per purpose.

# install.packages("devtools")
# devtools::install_github("byu-transpolab/nhts2017")
library(nhts2017)
library(tidyverse)

 

# Join persons, households, and trips together into one data set.
# select only the relevant columns (easy to add more variables later)
# then write into an .rds file and store in data folder
nhts_persons %>%
  select(houseid, personid, w_chair, w_mtrchr, medcond6, medcond, r_age, wkftpt, wtperfin) %>%
  left_join(nhts_households %>% 
              # I only need a couple variables from the household file
              select(houseid, hhfaminc, hhstate), 
            by = "houseid") %>%
  # because households doesn't have a personid, i join persons-households first, then I can 
  # create the hhpersonid to join with trips.
  mutate(hhpersonid = paste(houseid, personid, sep = "-")) %>%
  # now I can join with trips
  left_join(nhts_trips %>%
              # mutate first, so that i can exclude the houseid and personid when I join with persons
              mutate(
                hhpersonid = paste(houseid, personid, sep = "-"),
                trpmiles = as.double(trpmiles)
                     ) %>%
              select(hhpersonid, trpmiles, trippurp, trptrans, trvlcmin, psgr_flg),
            # maybe i will need to filter the trip data (miles > 0, exclude the -9s and -1s and stuff)
            by = "hhpersonid") %>%
  
  
  # This is the MUTATE section where new variables are created.
  mutate(
    # labels every individual as wheelchair, disabled, or abled
    Ability = case_when(w_chair == "07" | w_mtrchr == "08" ~ "Wheelchair",
                        medcond6 == "02" | medcond6 == "03" ~ "Disabled",
                        medcond == "02" ~ "Abled"
                        ),
    # groups ages into bins
    Age = case_when(r_age >= 00 & r_age < 10 ~ "0 - 10",
                    r_age >= 10 & r_age < 20 ~ "10 - 20",
                    r_age >= 20 & r_age < 30 ~ "20 - 30",
                    r_age >= 30 & r_age < 40 ~ "30 - 40",
                    r_age >= 40 & r_age < 50 ~ "40 - 50",
                    r_age >= 50 & r_age < 60 ~ "50 - 60",
                    r_age >= 60 & r_age < 70 ~ "60 - 70",
                    r_age >= 70 & r_age < 80 ~ "70 - 80",
                    r_age >= 80 & r_age < 90 ~ "80 - 90",
                    r_age >= 90 & r_age < 100 ~ "90 - 100"
                    ),
    # groups income into four bins
    # Low is less than $25,000
    # Mid-Low = $25,000 - $50,000
    # Mid-High = $50,000 - $100,000
    # High = More than $100,000 
    Income = case_when(hhfaminc == "01" | hhfaminc == "02" | hhfaminc == "03" ~ "Low",
                       hhfaminc == "04" | hhfaminc == "05" ~ "Mid-Low",
                       hhfaminc == "06" | hhfaminc == "07" ~ "Mid-High",
                       hhfaminc == "08" | hhfaminc == "09" | hhfaminc == "10" | hhfaminc == "11" ~ "High"
                       ),
    # reorganizes the income levels into the proper order
    Income = fct_relevel(Income, "Low", "Mid-Low", "Mid-High", "High"
                         ),
    # create the employment variable
    Worker = case_when(wkftpt == "01" ~ "Full-Time",
                       wkftpt == "02" ~ "Part-Time",
                       # it is safe to assume that whoever isn't partime or fulltime is unemployed
                       wkftpt == "-1" ~ "Unemployed"
                       )
  ) %>% 
  
  
  filter(
    # by filtering out Ability "NA", I exclude 1931 / 264233 respondents (0.73%)
    Ability != "NA",
    Income != "NA",
    Worker != "NA",
  ) 


                      