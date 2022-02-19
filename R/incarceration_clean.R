library(tidyverse)
library(mosaic)
library(dplyr)
library(here)

# import raw dataset
incarceration = read_csv(here("data/incarceration.csv"))
  
# identify and assign NA values
incarceration = mutate(incarceration, across(starts_with("E"), ~case_when(
  .x < 0   ~ NA_real_,
  .x == 99 ~ NA_real_,
  TRUE     ~ .x
)))

# filter observations with NA values across the entire year
incarceration = filter(incarceration, if_any(starts_with("E"), ~!is.na(.x)))

# sum across months with rowwise
incarceration = rowwise(incarceration)
incarceration = mutate(incarceration, total_arrests = sum(c_across(starts_with("E")), na.rm = TRUE))
incarceration = ungroup(incarceration)

# recode gender variable
incarceration = mutate(incarceration, gender = if_else(R0536300 == 1, "Male", "Female"))

# recode race variable
incarceration = mutate(incarceration, race = case_when(
  R1482600 == 1 ~ "Black",
  R1482600 == 2 ~ "Hispanic",
  R1482600 == 3 ~ "Mixed Race (Non-Hispanic)",
  R1482600 == 4 ~ "Non-Black / Non-Hispanic",
))

# keep only relevant variables
incarceration_clean = select(incarceration, race, gender, total_arrests)

# export clean data as a csv file
write.csv(incarceration_clean, here("data/incarceration_clean.csv"))
