
# Setup -------------------------------------------------------------------

library(tidyverse)
library(lme4)
library(readxl)
library(janitor)

input_data <- read_excel('data/raw_data/Data_density dependent.xlsx') %>%
  clean_names() %>%
  mutate(larval_density = gsub('L', '', larval_density),
         larval_density = as.numeric(larval_density),
         period_of_adult_pupation_day = as.numeric(period_of_adult_pupation_day),
         species = tolower(species))
str(input_data)

ggplot(input_data, aes(x = larval_density, y = period_of_pupation_day)) +
  geom_point() +
    theme_bw() +
  facet_wrap(. ~ species)
