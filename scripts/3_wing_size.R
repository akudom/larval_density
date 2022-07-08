# code by Dave Hemprich-Bennett and Talya Hackett, 
# working on data by Andreas Kudom
# analyses the effect of Anopheles gambiae larval density
# on adult wing size


# Setup -------------------------------------------------------------------


library(tidyverse)
library(here)
library(readxl)

if(!dir.exists(here('figures'))){
  dir.create(here('figures'))
}
if(!dir.exists(here('figures', 'wing_size'))){
  dir.create(here('figures', 'wing_size'))
}

if(!dir.exists(here('results'))){
  dir.create(here('results'))
}
if(!dir.exists(here('results', 'wing_size'))){
  dir.create(here('results', 'wing_size'))
}

wingsize_df <- read_csv(here('data', 'raw_data', 'wing_data_tidied.csv'))

# when creating some summary statistics and plots, it's easier for the
# data to be in a 'long' rather than 'wide' format. Transform it
wingsize_longformat <- wingsize_df %>%
  # make the data 'long' format
  pivot_longer(cols = c('10L', '60L', '200L'),
               names_to = 'larval_density',
               values_to = 'winglength_um') %>%
  # remove any rows with an NA in the 'winglength_um' 
  # column. These exist because there was uneven sampling
  # in the initial spreadsheet
  filter(!is.na(winglength_um)) %>%
  # the column larval_density is formatted alphabetically as a character
  # by default, which (wrongly) orders our variables '10L, '200L', '60L'.
  # Lets fix that
  mutate(larval_density = fct_relevel(larval_density,
                                      # put 200L last ('after infinity', apparently)
                                      "200L", after = Inf))




# Plot --------------------------------------------------------------------


wingsize_boxplot <- ggplot(wingsize_longformat, aes(x = larval_density,
                                y = winglength_um)) +
  geom_boxplot()

wingsize_boxplot

ggsave(filename = here('figures', 'wing_size', 'wing_size_boxplot.pdf'), 
       plot = wingsize_boxplot)

# Summary stats -----------------------------------------------------------


larval_summary_stats <- wingsize_longformat %>%
  group_by(larval_density) %>%
  summarise(`Number of samples` = n(),
            `Mean winglength (um)` = mean(winglength_um),
            `Winglength standard deviation` = sd(winglength_um)) %>%
  rename(`Larval density` = larval_density)

write_csv(larval_summary_stats, 
          file = here('results', 'wing_size', 'wing_size_summary_stats.csv'))
