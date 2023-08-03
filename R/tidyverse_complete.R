# tidyr complete function

library(tidyverse)

df <- tibble(
  group = c(1:2, 1, 2),
  item_id = c(1:2, 2, 3),
  item_name = c("a", "a", "b", "b"),
  value1 = c(1, NA, 3, 4),
  value2 = 4:7
)

# Combinations --------------------------------------------------------------
# Generate all possible combinations of `group`, `item_id`, and `item_name`
# (whether or not they appear in the data)
df %>% complete(group, item_id, item_name)

# Cross all possible `group` values with the unique pairs of
# `(item_id, item_name)` that already exist in the data
df %>% complete(group, nesting(item_id, item_name))

# Within each `group`, generate all possible combinations of
# `item_id` and `item_name` that occur in that group
df %>%
  dplyr::group_by(group) %>%
  complete(item_id, item_name)

# Supplying values for new rows ---------------------------------------------
# Use `fill` to replace NAs with some value. By default, affects both new
# (implicit) and pre-existing (explicit) missing values.
df %>%
  complete(
    group,
    nesting(item_id, item_name),
    fill = list(value1 = 0, value2 = 99)
  )

# Limit the fill to only the newly created (i.e. previously implicit)
# missing values with `explicit = FALSE`
df %>%
  complete(
    group,
    nesting(item_id, item_name),
    fill = list(value1 = 0, value2 = 99),
    explicit = FALSE
  )


df <- tibble(
  year     = c(2020, 2020, 2020, 2020, 2021, 2021, 2021, 2021),
  haul     = c(   1,    2,    2,    2,    1,    2,    3,    4),
  species  = c( "A",  "A",  "B",  "C",  "A",   "B",  "A", "D"),
  value    = c(   1,    2,    3,    3,    2,     3,    1,   1)
)

df %>% 
  group_by(year) %>% 
  complete(haul, species, fill=list(value=0))
