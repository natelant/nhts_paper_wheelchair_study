# this page is to practice left join and right join

# practice one very basic
base <- tibble(
  ID = c(1:10),
  color = c("b", "g", "r", "b", "g", "g", "g", "r", "b", "g"),
  age = c(3, 4, 5, 4, 4, 5, 6, 6, 5, 8)
)

add <- tibble(
  ID = c(1:8),
  weight = c(23, 35, 46, 24, 25, 23, 34, 45)
)

base %>% left_join(add, by = "ID")
add %>% right_join(base, by = "ID")

base %>% right_join(add, by = "ID")
add %>% left_join(base, by = "ID")








# Practice two mini nhts blend

#persons
persons <- tibble(
  house = c(1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5),
  ID = c(1, 1, 2, 3, 1, 2, 3, 4, 1, 2, 3, 1, 2),
  color = c("red", "green", "blue", "blue", "blue", "green", "red", "green", "blue", "yellow", "brown", "red", "blue")
) %>%
  mutate(hhpersonid = paste(house, ID, sep = "-"))

#house
houses <- tibble(
  house = c(1:5),
  inc = c(456, 456, 567, 345, 234)
)

#trips
trips <- tibble(
  house = c(1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 5, 5, 5, 5),
  ID = c(1, 1, 2, 2, 3, 3, 3, 1, 1, 3, 3, 4, 4, 4, 1, 1, 1, 1),
  trip_count = c(1, 1, 1, 2, 1, 2, 3, 1, 2, 1, 2, 1, 2, 3, 1, 2, 3, 4),
  min = c(34, 456, 23, 345, 567, 5678, 6, 34, 45, 67, 67, 23, 34, 45, 234, 345, 56, 45)
) %>%
  mutate(hhpersonid = paste(house, ID, sep = "-"))

# I think this is my problem.
persons %>% left_join(houses, by = "house") %>% left_join(trips, by = "hhpersonid") %>% print(n=60)

persons %>% left_join(houses, by = "house") %>% left_join(trips, by = c("house", "ID")) %>% print(n=60)









d1 <- tibble(
  x = letters[1:3],
  y = LETTERS[1:3],
  a = rnorm(3)
)

d2 <- tibble(
  x2 = letters[3:1],
  y2 = LETTERS[3:1],
  b = rnorm(3)
)

left_join(d1, d2, by = c("x" = "x2", "y" = "y2"))

d1 %>% left_join(d2, by = c())



