
# Setup -------------------------------------------------------------------

library(tidyverse)
library(lme4)
library(readxl)
library(janitor)

input_data <- read_excel('data/raw_data/Data_density dependent.xlsx') %>%
  clean_names()
str(input_data)

