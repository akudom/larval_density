
# Setup -------------------------------------------------------------------

library(tidyverse)
library(lme4)
library(readxl)
library(janitor)

# read in data
input_data <- read_excel('data/raw_data/Data_density dependent.xlsx') %>% 
  # remove spaces etc from column names
  clean_names() %>%
  mutate(# remove the character 'L' from larval_density
    larval_density = gsub('L', '', larval_density),
         # convert larval_density to numeric 
         larval_density = as.numeric(larval_density),
         # replace the string 'N/A' with NA values in period_of_adult_pupation_day
         period_of_adult_pupation_day = gsub('N/A', NA, period_of_adult_pupation_day),
         # convert period_of_adult_pupation_day from character to numeric
         period_of_adult_pupation_day = as.numeric(period_of_adult_pupation_day),
         # make species entirely lowercase, as it's currently mixed 
         species = tolower(species))
str(input_data)

# make a simple pair of violin plots to show summary stats
violins <- input_data %>% 
  pivot_longer(c(period_of_pupation_day, period_of_adult_pupation_day), 
               names_to = 'stat_type', values_to = 'value') %>%
  ggplot(., aes(x = species, y = value, fill = species)) +
  geom_violin() +
    theme_bw() +
  theme(legend.position = 'bottom') +
  scale_fill_viridis_d() +
  facet_grid(stat_type ~ larval_density, 
             switch = 'y')
ggsave(filename = 'figures/violinplot.pdf',
       violins)
