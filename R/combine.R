## Combine

# for reference to the NHTS codebook https://nhts.ornl.gov/assets/codebook_v1.1.pdf

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
my_nhts <- nhts_persons %>%
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
              select(hhpersonid, trippurp, trptrans, psgr_flg),
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
                       ),
    # create groups for all of the modes
    Mode = case_when(psgr_flg == "02" & trptrans == "03" | trptrans == "04" |
                       trptrans == "06"  ~ "Car (Driver)",
                     psgr_flg == "01" & trptrans == "03" | trptrans == "04" | 
                       trptrans == "06"  ~ "Car (Passenger)",
                     psgr_flg == "02" & trptrans == "05" ~ "Van (Driver)",
                     psgr_flg == "01" & trptrans == "05" ~ "Van (Passenger)",
                     trptrans == "01" ~ "Walk",
                     trptrans == "12" ~ "Paratransit",
                     trptrans == "17" ~ "Taxi (Including Uber/Lyft)",
                     trptrans == "11" | trptrans == "16"  ~ "Local Transit",
                     TRUE  ~ "Other"
                     ),
    # reorders the modes (helpful for graphing)
    Mode = fct_relevel(Mode, "Car (Driver)", "Car (Passenger)", "Van (Driver)", 
                       "Van (Passenger)", "Walk", "Local Transit", "Paratransit", 
                       "Taxi (Including Uber/Lyft)", "Other")
  ) %>% 
  
  
  filter(
    # by filtering out Ability "NA", I exclude 1931 / 264233 respondents (0.73%)
    Ability != "NA",
    Income != "NA",
    Worker != "NA",
  ) 

#--------------------------------------------------------------------------------------------------
## BUILDING THE GRID
# This data will contain all trips (all households, persons, etc.)
# whereas in the example Dr. Macfarlane sent me, he filtered the households by urban size, 
# msa size, travel day, etc.

# First find all trips that belong to an hhpersonid, group them by purpose and count them
my_trips <- nhts_trips %>% 
  # the trips data file also has houseid and personid. first combine these into one id
  mutate(hhpersonid = paste(houseid, personid, sep = "-")) %>%
  # only use the trips from the hhpersonid in the persons_edited file
  filter(hhpersonid %in% my_nhts$hhpersonid) %>%
  group_by(hhpersonid, trippurp) %>%
  # make a count of repeating person/trip purpose combinations. For example, how many times did
  # 300...07-01 make an HBO trip.
  tally() %>%
  # anything bigger than 10 trips will be counted as 10 trips.
  mutate(n = ifelse(n > 10, 10, n))


# this grid has over 1 million rows. it has each purpose for each person
my_grid <- expand_grid(
  # uses only the unique hhpersonids found in the new_trips data
  hhpersonid = unique(my_trips$hhpersonid), 
  # uses only HBO, HBW, HBSOCREC, NHB, and HBSOP
  trippurp = unique(my_trips$trippurp)
) %>%
  filter(trippurp != "-9")


# combine the grid and the trips
# this new data set has over 1 million rows. each row is a trip purpose per person and has all of the 
# person data attached with it.
# n is the number of trips for a specific person and purpose
my_trips %>% 
  right_join(my_grid) %>%
  # if no trips were taken show they took 0 trips
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  # add all of the person data (including Ability, Age, Worker, Income groups etc.)
  # this data set is over 4 million rows deep.
  # it doubles each person and trip purpose combo because there are two trip miles values per trip
  left_join(my_nhts, "hhpersonid") %>%
  
  
  write_rds("data/combined.rds")




#--------------------------------------------------------------------------------------------------
                      