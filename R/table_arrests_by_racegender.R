library(tidyverse)
library(mosaic)
library(dplyr)
library(here)
library(ggthemes)
library(float)
library(kableExtra)
library(snakecase)


read_csv(here("data/incarceration_clean.csv")) %>%
  
  # summarize arrests by race and gender
  group_by(race, gender) %>%
  summarize(total_arrests = mean(total_arrests)) %>%
  
  # pivot the values from race into columns
  pivot_wider(names_from = race, values_from = total_arrests) %>%
  
  # rename columns using snakecase
  rename_with(to_title_case) %>%
  
  # create the kable object. Requires booktabs and float LaTeX packages
  kbl(
    caption = "Mean arrests in 2002 by Race and Gender",
    booktabs = TRUE,
    format = "latex",
    label = "tab:summarystats"
  ) %>%
  kable_styling(latex_options = c("striped", "HOLD_position")) %>%
  
  write_lines(here("tables/arrests_by_racegender.tex"))