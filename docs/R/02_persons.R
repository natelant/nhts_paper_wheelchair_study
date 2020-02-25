# This persons.R file is to build the same attributes as combine.R but exludes trips
# writes the file as persons.rds

nhts_persons %>%
  select(houseid, personid, w_chair, w_mtrchr, w_scootr, medcond6, 
         medcond, r_age, wkftpt, wtperfin, health, worker
         ) %>%
  left_join(nhts_households %>% 
              # I only need a couple variables from the household file
              select(houseid, hhfaminc, hhstate, msasize), 
            by = "houseid") %>%
  

  
  
  # This is the MUTATE section where new variables are created.
  mutate(
    # after all the joining is complete, use a combined house-person ID for ease
    hhpersonid = paste(houseid, personid, sep = "-"),
    
    # labels every individual as wheelchair, disabled, or abled
    Ability = case_when(w_chair == "07" | w_mtrchr == "08" | w_scootr == "06" ~ "Wheelchair",
                        medcond == "01" ~ "Disabled",  
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
    pop_size = case_when(msasize == "01" ~ "Less than 250,000",
                         msasize == "02" ~ "250,000 - 499,999",
                         msasize == "03" ~ "500,000 - 999,999",
                         msasize == "04" ~ "1,000,000 - 2,999,999",
                         msasize == "05" ~ "More than 3,000,000",
                         TRUE ~ "Not in an MSA"
                         ),
    #reorders the factors of msa pop_size
    pop_size = fct_relevel(pop_size, "Less than 250,000", "250,000 - 499,999", "500,000 - 999,999", 
                           "1,000,000 - 2,999,999", "More than 3,000,000", "Not in an MSA"
                           ),
    # changes the 1, 2, 3, to Excellent, Good, Fair etc.
    health = as_factor(health)
    
  ) %>%
  
  write_rds("data/persons.rds")