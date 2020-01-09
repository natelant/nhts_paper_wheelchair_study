# install.packages("devtools")
# devtools::install_github("byu-transpolab/nhts2017")
# library(nhts2017)
# library(tidyverse)

# start with slightly formatting the trips file
# then join it to persons
nhts_trips <- nhts_trips %>% 
  mutate(hhpersonid = paste(houseid, personid, sep = "-"),
         trpmiles = as.double(trpmiles))

# Edit the persons file by creating new variables mixed with household data
# then write into an .rds file and stores in data folder
nhts_persons %>%
  left_join(nhts_households %>% 
              # I only need a couple variables from the household file
              select(houseid, hhfaminc, hhstate), 
            by = "houseid") %>%
  
  # Organize the categorical variables
  mutate(
    # create a unique id for every individual
    hhpersonid = paste(houseid, personid, sep = "-"
                       ),
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
         ) %>%
  
  # write into an .rds file
  write_rds("data/nhts_data.rds")