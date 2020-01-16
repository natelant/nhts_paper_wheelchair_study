# building a grid to count trips
library(nhts2017)
library(tidyverse)

# take the data from the .rds file created in cleaning.R
# Creating variables and filtering the persons population
persons_edited <- read_rds("data/nhts_data.rds") 

# This data will contain all trips (all households, persons, etc.)
# whereas in the example Dr. Macfarlane sent me, he filtered the households by urban size, 
# msa size, travel day, etc.

# First find all trips that belong to an hhpersonid, group them by purpose and count them
new_trips <- nhts_trips %>% 
  # the trips data file also has houseid and personid. first combine these into one id
  mutate(hhpersonid = paste(houseid, personid, sep = "-")) %>%
  # only use the trips from the hhpersonid in the persons_edited file
  filter(hhpersonid %in% persons_edited$hhpersonid) %>%
  group_by(hhpersonid, trippurp) %>%
  # make a count of repeating person/trip purpose combinations. For example, how many times did
  # 300...07-01 make an HBO trip.
  tally() %>%
  # anything bigger than 10 trips will be counted as 10 trips.
  mutate(n = ifelse(n > 10, 10, n))



# this grid has over 1 million rows. it has each purpose for each person
new_grid <- expand_grid(
  # uses only the unique hhpersonids found in the new_trips data
  hhpersonid = unique(new_trips$hhpersonid), 
  # uses only HBO, HBW, HBSOCREC, NHB, and HBSOP
  trippurp = unique(new_trips$trippurp)
) %>%
  filter(trippurp != "-9")


# combine the grid and the trips
# this new data set has over 1 million rows. each row is a trip purpose per person and has all of the 
# person data attached with it.
# n is the number of trips for a specific person and purpose
new_trips %>% 
  right_join(new_grid) %>%
  # if no trips were taken show they took 0 trips
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  # add all of the person data (including Ability, Age, Worker, Income groups etc.)
  left_join(persons_edited, "hhpersonid") 


