library(tidyverse)
library(mosaic)
library(dplyr)
library(here)
library(ggthemes)

# create bar graph of months incarcerations by race and gender using clean dataset
read_csv(here("data/incarceration_clean.csv")) %>%
  group_by(race, gender) %>%
  summarize(total_arrests = mean(total_months_icarcerated)) %>%
  ggplot(aes(race, total_arrests, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Race", 
    y = "Mean Months Incarcerated", 
    fill = "Gender",
    title = "Mean Number of Months Incarcerated in 2002 by Race and Gender") +
  theme_minimal() +
  scale_fill_economist()

ggsave(here("figures/incarceration_by_racegender.png"), width=8, height=4.5)
