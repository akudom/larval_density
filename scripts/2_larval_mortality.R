# code by Dave Hemprich-Bennett and Talya Hackett, 
# working on data by Andreas Kudom
# analyses the effect of Anopheles gambiae larval density
# on larval mortality


# Setup -------------------------------------------------------------------

library(tidyverse)
larval_df <- read_csv('data/processed_data/density_and_growth.csv')

str(larval_df)

survival_df <- larval_df %>%
  # remove any rows where theres an NA on the eclosion
  # date, that way we only have rows for larvae that
  # successfully eclosed (i.e. survived to adulthood)
  filter(!is.na(period_of_eclosion_day)) %>%
  # make a column per grouping variable stating how
  # many survived
  group_by(date_of_experiment, larval_density,
           replicate_no, species) %>%
  summarise(n_survived = n()) %>%
  # make a column showing how many died
  mutate(n_died = larval_density - n_survived,
         proportion_survival = n_survived / larval_density)
            
            
