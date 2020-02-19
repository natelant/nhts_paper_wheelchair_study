#' Build an activities dataframe from the NHTS Trips dataset
#'
#' @param trips The NHTS Trips dataset, e.g. `nhts_trips` or a subset.
#'
#' @return A `tibble` with one row per activity for each person in the NHTS dataset.
#'
#'
# library(tidyverse)
# library(nhts2017)
# library(lubridate)

# make data easier to test
 trips <- nhts_trips %>%
   filter(houseid %in% c("40793049", "40793216", "40793969", "40793969"))
 
 trips <- nhts_trips %>%
   filter(houseid == "30000539")


# Necessary fields include houseid, personid, strttime, endtime, whyfrom, whyto

build_activities <- function(trips) {
  
  # create list of events labeled arrive or depart
  events <- trips %>%
    # exlude all other trip attributes
    select(houseid, personid, strttime, endtime, whyfrom, whyto) %>%
    # gather times
    gather(strtend, time, strttime, endtime) %>%
    group_by(houseid, personid) %>%
    # there have been errors where a time stamp is wrongly labeled and
    # wrongly arranged here...and again on line 57
    arrange(time, .by_group = TRUE) %>%
    mutate(
      event = ifelse(strtend == "strttime", "depart", "arrive"),
      activity = ifelse(event == "depart", whyfrom, whyto)
    )
  
  
  first_activity <- events %>%
    # take the starting point of each person
    slice(1) %>%
    # creates a starting time for each person at 4:00:00 am
    transmute(activity = as.character(whyfrom),
              time = as_datetime("2017-10-10 4:00:00"),
              event = "arrive")
  
  last_activity <- events %>%
    slice(n()) %>%
    transmute(activity = as.character(whyto),
              time = as_datetime("2017-10-11 4:00:00"),
              event = "depart")
  
  # combine events with first and last activity
  # spread by either arrive or depart
  events %>% full_join(first_activity) %>% full_join(last_activity) %>%
    arrange(time, .by_group = TRUE) %>%
    select(-whyfrom, -whyto, -strtend) %>%
    group_by(houseid, personid, event) %>%
    mutate(activity_number = as.integer(factor(time))) %>%
    spread(event, time) %>%
    arrange(arrive, .by_group = TRUE)
  
  
}



# This function takes the activities list and numbers and classifies the tours each 
# person takes during the day
# output is a list of activities with a tour classification column

add_tours <- function(activities) {
  
  # create columns for tour count 
  activity_list <- activities %>% 
    mutate(
      home_status = ifelse(activity == "01", 1, 0),
      tour_count = cumsum(home_status),
      # this goes in mutate to make home activites not a part of a tour
      tour_count = ifelse(home_status, NA, tour_count)
    ) %>%
    # no longer need home_status
    select(-home_status)
    
    # create the tour classification
    # create column of each classification.
    # outputs list of tours (not activities)
    tour_list <- activity_list %>% 
      # include group_by tour_count so the list is by tours not persons
      group_by(houseid, personid, tour_count) %>%
      summarise(tour_list = paste(activity, collapse = " ")) %>%
      mutate(tour_class = case_when(
                           str_detect(tour_list, "03") == T ~ "W",
                           str_detect(tour_list, "04") == T ~ "W",
                           str_detect(tour_list, "08") == T ~ "S",
                           str_detect(tour_list, "01") == T ~ "home",
                           TRUE ~ "NM"
                           )) %>%
      # filter out the tours that are "home" because technically they aren't even tours.
      filter(tour_class != "home"
             # when tour_count = 0, the person started at somewhere other than home
             # and they will be cut out of the data
             # tour_count != 0)
      )
      # collapse by tour_class add column for w1 and w2
    daps <- tour_list %>%
      summarise(tours_row = paste(tour_class, collapse = "-")) %>%
      mutate(DAP = case_when(
        str_detect(tours_row, "W-W") == T ~ "W_2",
        str_detect(tours_row, "W") == T ~ "W_1",
        str_detect(tours_row, "S-S") == T ~ "S_2",
        str_detect(tours_row, "S") == T ~ "S_1",
        TRUE ~ "NM"
        # still leaves the question, what about a tour like H-W-W-W == W-2?
        # this would need to be figured out somewhere else...
      ))
      
      # join back onto activites to create the classification column
    activity_list %>%
      left_join(tour_list, by = c("houseid", "personid", "tour_count")) %>%
      #include the dap column
      left_join(daps,  by = c("houseid", "personid")) %>%
      # eliminate tour_list and tours_row
       select(-tour_list, -tours_row)
      
  
}


# Name: build_tours
# output: list of tours
# input: list of activities
# attributes: houseid, personid, tour_list, DAP

build_tours <- function(activities){
  
  # adds column that starts counting tours
  mytour_count <- myactivities %>%
    # eliminate unnecessary columns
    select(houseid, personid, activity, activity_number, arrive, depart) %>%
    group_by(houseid, personid) %>%
    # set a home status to create a cumulative count
    mutate(
      home_status = ifelse(activity == "01", 1, 0),
      tour_count = cumsum(home_status),
      # this goes in mutate to make home activites not a part of a tour
      tour_count = ifelse(home_status, NA, tour_count)
    )
  
  # collapse tours into persons with DAPs
  dap_class <- mytour_count %>% 
    # include group_by tour_count so the list is by tours not persons
    group_by(houseid, personid, tour_count) %>%
    summarise(tour_list = paste(activity, collapse = " ")) %>%
    mutate(tour_class = case_when(
      str_detect(tour_list, "03") == T ~ "W",
      str_detect(tour_list, "04") == T ~ "W",
      str_detect(tour_list, "08") == T ~ "S",
      str_detect(tour_list, "01") == T ~ "home",
      TRUE ~ "NM"
    )) %>%
    # filter out the tours that are "home" because technically they aren't even tours.
    filter(tour_class != "home"
           # when tour_count = 0, the person started at somewhere other than home
           # and they will be cut out of the data
           # tour_count != 0)
    ) %>%
  # collapse by tour_class add column for w1 and w2
    summarise(tours_row = paste(tour_class, collapse = "-")) %>%
    mutate(DAP = case_when(
      str_detect(tours_row, "W-W") == T ~ "W_2",
      str_detect(tours_row, "W") == T ~ "W_1",
      str_detect(tours_row, "S-S") == T ~ "S_2",
      str_detect(tours_row, "S") == T ~ "S_1",
      TRUE ~ "NM"
      # still leaves the question, what about a tour like H-W-W-W == W-2?
      # this would need to be figured out somewhere else...
    ))
  
  ## ======================================================================
  # joins persons with activities lists (tours) with the daps created
  mytour_count %>%
    # collapse the activities into the day_plan column
    group_by(houseid, personid) %>%
    summarise(tour_list = paste(activity, collapse = " ")) %>%
    left_join(dap_class, by = c("houseid", "personid"))
}



# This function is designed to create a table of distributions by ability type for each factor in 
# a variable (i.e. income, age, worker status etc.)
distributions <- function(data, variable){
  
  quote_var <- enquo(variable)
  
  data %>% 
    filter(Ability != "NA", 
           !!quote_var != "NA") %>%
    # Calculate number of people in each combination of income and ability.
    group_by(Ability, !!quote_var) %>%
    summarise(population = sum(wtperfin)) %>%
    mutate(Population = population,
           `Distribution(%)` = 
             percent(population/sum(population), 
                     accuracy = 0.1),
           Variable = as.factor(!!quote_var)) %>%
    select(Ability, Variable, `Distribution(%)`) %>%
    spread(Ability, `Distribution(%)`)
  
}