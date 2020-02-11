# Builds tours filtered by MSA and age
# writes an rds saved in data folder

# filter people that are in regular sized cities
# filter people that are between 18 and 65
trips_edited %>%
  filter(r_age > 17,
         r_age < 65,
         msasize == "03") %>%
  build_activities() %>%
  build_tours() %>%
  left_join(persons_edited) %>%
  
  write_rds("data/tours_msa.rds") 