# code by Dave Hemprich-Bennett and Talya Hackett, 
# working on data by Andreas Kudom
# analyses the effect of Anopheles gambiae larval density
# on larval mortality


# Setup -------------------------------------------------------------------

library(tidyverse)
larval_df <- read_csv('data/processed_data/density_and_growth.csv')
